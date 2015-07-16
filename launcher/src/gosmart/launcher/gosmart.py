# This file is part of the Go-Smart Simulation Architecture (GSSA).
# Go-Smart is an EU-FP7 project, funded by the European Commission.
#
# Copyright (C) 2013-  NUMA Engineering Ltd. (see AUTHORS file)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# pylint: disable=line-too-long
#
import os
import re
import errno
import shutil
import time
import gosmart.config
from distutils.version import StrictVersion
from lxml import etree as ET

from gosmart.launcher.lesion import GoSmartLesion
from gosmart.launcher.elmer import GoSmartElmer
from gosmart.launcher.elmergrid import GoSmartElmerGrid
from gosmart.launcher.logger_observant import GoSmartLoggerObservant
from gosmart.launcher.mesh_optimizer import GoSmartMeshOptimizer
from gosmart.launcher.mesher import GoSmartMesher
from gosmart.launcher.mesher_axisymmetric import GoSmartMesherAxisymmetric
from gosmart.launcher.mesher_cgal import GoSmartMesherCGAL
from gosmart.launcher.needlelibrary_interface import GoSmartNeedleLibraryInterface
from gosmart.launcher.preprocessor import GoSmartPreprocessorInterface
from gosmart.launcher.globals import colorama_imported, slugify
from gosmart.launcher.errors import GoSmartModelError

if colorama_imported:
    import colorama


# We use class members to give default values
# because there is exactly one instance and it
# is clear to read and adjust. Note however,
# that if, for some unknown reason, multiple instances
# of the class are required, special care should be
# taken
class GoSmart:
    # TODO: Ensure this is the actual binary installed, perhaps using setup.py.in
    # Note that elmer_binary is _not necessarily_ the serial binary - it is
    # whichever binary is to be run by this process, so when we daisy-chain through
    # MPI run, the elmer_binary passed to the child via the arguments is the MPI
    # one. As such, when it runs, its "elmer_binary" is then ElmerSolver_mpi
    outfilename = "logger/go-smart-elmer-%d" % os.getpid()
    silent = False
    my_rank = None
    configfiles = []
    components = {}
    child_procs = None
    only = None
    elmer_binary = None

    _update_status_callback = None

    header_width = None

    def __init__(self, runname='default', args=[], global_working_directory=None, observant=None, update_status_callback=None):
        if global_working_directory is None:
            global_working_directory = os.getcwd()

        self.logger = GoSmartLoggerObservant(runname=runname, logfile="%s.log" % self.outfilename, global_working_directory=global_working_directory, observant=observant)

        self.parse_args(args)
        self.parse_config_files()

        self._update_status_callback = update_status_callback

        for component in self.components.values():
            if isinstance(component, list):
                for cpt in component:
                    cpt.set_outfile_prefix(self.outfilename)
            else:
                component.set_outfile_prefix(self.outfilename)

    def update_status(self, percentage, message):
        if self._update_status_callback is not None:
            self._update_status_callback(percentage, message)

    def add_component(self, name, component):
        if self.only is not None and self.only != name:
            return component

        self.components[name] = component
        component.suffix = name
        setattr(self, name, component)
        return component

    def print_header(self):
        header = [
            "=",
            "GO-SMART SIMULATION ENVIRONMENT",
            "-",
            "",
            "For information, see http://gosmart-project.eu/",
            "=",
        ]

        self.header_width = len(max(*header, key=len)) + 2
        for idx, line in enumerate(header):
            pad = ' '
            if line in ("=", '-'):
                pad = line

            header[idx] = pad + line.ljust(self.header_width, pad)

        for line in header:
            self.logger.print_line('|' + line + '|', prefix='', color="MAGENTA", color_bright=True)

        self.logger.print_line()

    def launch(self, default_procs=1):
        if self.child_procs is None:
            self.child_procs = default_procs

        final_output = {}

        self.logger.init_time = time.time()
        self.logger.print_debug("Seconds since Unix epoch: %d " % self.logger.init_time)

        self.logger.print_line("Using GSSF version: %s" % gosmart.config.git_revision)
        self.logger.print_line("Using Elmer (NUMA-modified) version: %s" % gosmart.config.elmer_git_revision)

        overall_percentage = 0.0
        percentage_per_component = 100 / len(self.components)

        needle_files = {}
        extent_file = None  # os.path.join("needlelibrary", "%s-extent.stl" % self.logger.runname)
        if "needlelibrary" in self.components:
            self.update_status(overall_percentage, "Needlelibrary starting")
            overall_percentage = overall_percentage + percentage_per_component
            needle_files, extent_file = self.needlelibrary.launch()
            self.update_status(overall_percentage, "Needlelibrary complete")
        else:
            self.update_status(overall_percentage, "Skipped needlelibrary")

        msh_files = {}
        default_msh_file = GoSmartMesher.get_default_msh_outfile(os.path.join('mesher', self.logger.runname), "mesher")
        if "mesher" in self.components:
            self.update_status(overall_percentage, "Mesher starting")
            msh_files.update(self.mesher.launch(needle_files, extent_file,
                             self.preprocessor if "preprocessor" in self.components else None))
            overall_percentage = overall_percentage + percentage_per_component
            self.update_status(overall_percentage, "Mesher complete")
        elif os.path.exists(os.path.join(self.logger.get_cwd(), default_msh_file["-mesher"])):
            msh_files.update(default_msh_file)
            self.update_status(overall_percentage, "Skipped mesher (and copied existing)")
        else:
            self.update_status(overall_percentage, "Skipped mesher")

        if hasattr(self, "mesher_inner"):
            for mesher_inner in self.mesher_inner:
                if mesher_inner.skip:
                    msh_files[mesher_inner.suffix] = "%s/%s.msh" % (mesher_inner.suffix, self.logger.runname)
                    self.update_status(overall_percentage, "Skipped mesher-inner")
                else:
                    self.update_status(overall_percentage, "Mesher-inner starting")
                    overall_percentage = overall_percentage + percentage_per_component
                    msh_files.update(mesher_inner.launch(needle_files, None))
                    self.update_status(overall_percentage, "Mesher-inner complete")

        if hasattr(self, "optimizer"):
            self.update_status(overall_percentage, "Optimizer starting")
            overall_percentage = overall_percentage + percentage_per_component
            for k, v in msh_files.items():
                msh_files[k] = self.optimizer.launch(v, appendix=k)
            self.update_status(overall_percentage, "Optimizer complete")
        else:
            for k, v in msh_files.items():
                optimized_msh_file = "optimizer/%s%s-optimized.msh" % (self.logger.runname, k)
                if os.path.exists(os.path.join(self.logger.get_cwd(), optimized_msh_file)):
                    msh_files[k] = optimized_msh_file
            self.update_status(overall_percentage, "Optimizer complete")

        msh_files = self.renumber_bodies(msh_files)
        eg_trees = {}
        if "elmergrid" in self.components:
            self.update_status(overall_percentage, "ElmerGrid starting")
            overall_percentage = overall_percentage + percentage_per_component
            for k, v in msh_files.items():
                eg_trees[k] = self.elmergrid.launch(v, self.child_procs, appendix=k)
            self.update_status(overall_percentage, "ElmerGrid complete")
        else:
            for k, v in msh_files.items():
                eg_trees[k] = "elmergrid/%s%s" % (self.logger.runname, k)

        if "elmer" in self.components:
            self.update_status(overall_percentage, "Elmer starting")
            self.elmer.set_update_status(lambda p, m: self.update_status(p / percentage_per_component + overall_percentage, m))
            overall_percentage = overall_percentage + percentage_per_component
            self.launch_elmer(mesh_locations=eg_trees)
            self.update_status(overall_percentage, "Elmer complete")

        if "lesion" in self.components:
            self.update_status(overall_percentage, "Lesion starting")
            overall_percentage = overall_percentage + percentage_per_component
            lesion_surface = self.lesion.launch(is_parallel=(self.child_procs is not None and self.child_procs > 1))
            self.update_status(overall_percentage, "Lesion complete")
            final_output["lesion_surface.vtp"] = lesion_surface

        if len(final_output) > 0:
            os.makedirs(self.logger.make_cwd("output"), exist_ok=True)

        for k, f in final_output.items():
            shutil.copy(os.path.join(self.logger.get_cwd(), f), os.path.join(self.logger.get_cwd(), "output", "%s-%s" % (self.logger.runname, k)))

    def renumber_bodies(self, msh_files):
        present_ids = {}

        msh_content = {}

        # Loop through the MSH files to work out which are 2D and which are 3D
        for k, v in msh_files.items():
            with open(os.path.join(self.logger.get_cwd(), v), 'r') as f:
                content = f.read()
                # If this MSH file contains volume elements, add it as the kth entry to msh_content,
                # marked as a 3D mesh
                if re.search(r"^\d+ 4 2 ", content, re.MULTILINE) is not None:
                    msh_content[k] = (True, content)
                # Otherwise, if this MSH file has a surface element, add it as the kth entry to msh_content,
                # marked as a 2D mesh
                elif re.search(r"^\d+ 2 2 ", content, re.MULTILINE) is not None:
                    msh_content[k] = (False, content)

        # At this point, msh_content is a list of meshes numbered from 1 to k

        # Note that this will break with more than ~26 zones
        #
        # Check which zones are and are not present and make sure they are
        # numbered consistently throughout the MSH files
        for zone, a in self.logger.zones.items():
            self.logger.print_debug(zone)
            for k, d in msh_content.items():
                is_3d_mesh, content = d
                elt_type = "4" if is_3d_mesh else "2"

                # If we have already come across this zone in a MSH make sure
                # that we use the same tag
                if zone not in present_ids:
                    new_id = len(present_ids) + 1
                else:
                    new_id = present_ids[zone]

                # Swap all lines that start like this (e.g. 143 4 2 6 6)
                from_sub = r"(?m)^(\d+) {1} 2 {0} \d+ ".format(a['id'], elt_type)
                # ...to be like this (e.g. 143 4 2 A A)
                to_sub = r"\1 {1} 2 {0} {0} ".format(chr(65 + new_id), elt_type)
                new_content, num_changes = re.subn(from_sub, to_sub, content)

                # If we changed anything, then this zone is officially present, so we
                # record the tag we used and update the MSH content array
                if num_changes > 0:
                    self.logger.print_line("%s: Found %d elements for %s" % (msh_files[k], num_changes, zone))
                    present_ids[zone] = new_id
                    msh_content[k] = (is_3d_mesh, new_content)

        self.logger.print_line(present_ids)
        # Go through all the present zones in all the MSH files
        for zone, i in present_ids.items():
            for k, d in msh_content.items():
                is_3d_mesh, content = d
                elt_type = "4" if is_3d_mesh else "2"
                # Turn the letters back into numbers
                from_sub = r"(?m)^(\d+) {1} 2 {0} {0} ".format(chr(65 + i), elt_type)
                to_sub = r"\1 {1} 2 {0} {0} ".format(i, elt_type)
                msh_content[k] = (is_3d_mesh, re.sub(from_sub, to_sub, content))
            # Store the updated zone tag with the zone in our Python zones array
            self.logger.zones[zone]['id'] = i

        # Strip out any zones from our records that appear in no MSH files. From now
        # on it is assumed that we have consecutively numbered zones, all appearing as
        # elements in some MSH, ready to be dropped into a SIF
        exclusion = [z for z in self.logger.zones if z not in present_ids]
        self.logger.print_debug(self.logger.zones_map)
        self.logger.print_debug(exclusion)
        for zone in exclusion:
            self.logger.zones_map.pop(self.logger.zones[zone]['id'])
            self.logger.zones.pop(zone)

        # Write out the MSH files to the filesystem
        if msh_content:
            try:
                os.makedirs(os.path.join(self.logger.get_cwd(), "meshes-reordered"))
            except OSError as e:
                if e.errno != errno.EEXIST:
                    self.print_fatal("Could not create %s directory: %s" %
                                     (self.suffix, str(e)))

            for k, d in msh_content.items():
                output_mesh = os.path.join(self.logger.get_cwd(), "meshes-reordered", "%s%s.msh" % (self.logger.runname, k))
                with open(output_mesh, 'w') as f:
                    f.write(d[1])
                msh_files[k] = output_mesh

        return msh_files

    def launch_elmer(self, default_procs=1, **kwargs):
        if self.child_procs is None:
            self.child_procs = default_procs

        self.elmer.launch(self.child_procs, **kwargs)

    def parse_args(self, args):
        global colorama_imported

        self.logger.silent = args.silent
        self.logger.debug = args.debug
        self.logger.leavetree = args.leavetree
        if args.outfilename is not None:
            self.outfilename = args.outfilename
        if args.addpid:
            self.outfilename += "-" + str(os.getpid())
        if args.elmer_binary is not None:
            self.elmer_binary = args.elmer_binary
        if args.nprocs is not None:
            self.child_procs = int(args.nprocs)
        if args.only is not None:
            self.only = args.only

        if not args.baw and colorama_imported:
            colorama.init()
        else:
            colorama_imported = False

        if len(args.configfilenames) > 0:
            self.configfiles = args.configfilenames

    def parse_config_file(self, filename):
        filename = os.path.join(self.logger.get_cwd(), filename)

        configtree = ET.parse(filename)
        root = configtree.getroot()

        if root.tag != "gosmart":
            self.logger.print_fatal("This settings file seems not to be for Go-Smart")

        if root.get('debug') == "true":
            self.logger.debug = True

        if root.get('version') is not None:
            self.logger.version = StrictVersion(root.get('version'))
        else:
            self.logger.version = StrictVersion("0.1.0")

        runname = root.get('name')
        if runname is not None:
            self.logger.runname = runname

        self.logger.parse_config(root)

        only_needle = None
        for section in root:
            skip = (section.get("skip") == "true")

            if section.tag == 'needlelibrary' and not skip:
                needlelibrary = self.add_component('needlelibrary', GoSmartNeedleLibraryInterface(self.logger))
                needlelibrary.parse_config(section)
                # RMV Tidy up.
                for needle in section:
                    if needle.tag != 'needle':
                        continue
                    if only_needle is None:
                        only_needle = needle
                    else:
                        only_needle = None
                        break

                if only_needle and only_needle.get("offset"):
                    for c, o in zip(('x', 'y', 'z'), only_needle.get("offset").split(" ")):
                        self.logger.add_or_update_constant(c, self.logger.get_constant(c, group="needle") + float(o), group="needle", typ="float")

            elif section.tag == 'needles':
                for needle in section:
                    if needle.tag != 'needle':
                        continue
                    if needle.get('name') is None:
                        raise GoSmartModelError("Missing needle name")

                    needle_name = needle.get('name')

                    for subsection in needle:
                        if subsection.tag == 'parameters':
                            if len(needle) == 1:
                                for parameter in needle[0]:
                                    self.logger.add_or_update_needle_constant(needle_name, parameter.get('name'), parameter.get('value'), parameter.get('type'))
                        else:
                            raise GoSmartModelError("There should be at most one node (parameters) inside needle section")

            elif section.tag == 'preprocessor':
                preprocessor = self.add_component('preprocessor', GoSmartPreprocessorInterface(self.logger))
                preprocessor.parse_config(section)
            elif section.tag == 'geometry':
                for setting in section:
                    if setting.tag == 'centre':
                        for c in ('x', 'y', 'z'):
                            self.logger.geometry["centre"][c] = float(setting.get(c))
                            self.logger.add_or_update_constant(c, float(setting.get(c)), group="needle", typ="float")
                    elif setting.tag == 'needleaxis':
                        self.logger.geometry["needleaxis"].append({})
                        for c in ('x', 'y', 'z'):
                            self.logger.geometry["needleaxis"][0][c] = float(setting.get(c))
                            self.logger.add_or_update_constant("axis " + c, float(setting.get(c)), group="needle", typ="float")
                    elif setting.tag == 'simulationscaling':
                        self.logger.geometry["simulationscaling"] = float(setting.get("ratio"))
                    else:
                        raise GoSmartModelError("Not understood geometry tag")
            elif section.tag == 'regions' or section.tag == 'definitions':
                for region in section:
                    if region.tag in ('region', 'surface', 'zone', 'both'):
                        if region.get("groups"):
                            groups = [s.strip() for s in region.get("groups").split(";")]
                        else:
                            groups = []
                        groups.append(region.tag)
                        if region.tag == "zone":
                            self.logger.add_region(region.get("name"), region.get("input"), groups, zone=True)
                        if region.tag == "both":
                            self.logger.add_region(region.get("name"), region.get("input"), groups, zone="both")
                        elif region.tag == "region" or region.tag == "surface":
                            self.logger.add_region(region.get("name"), region.get("input"), groups, zone=False)
                    elif region.tag in ('cad',):
                        self.logger.add_file(region.tag, region.get("name"), region.get("input"))
                    else:
                        raise GoSmartModelError("Not understood region tag")
            elif section.tag == 'mesher':
                inner = section.find("inner")
                self.has_inner = (inner is not None)

                type = section.get('type')

                skip_outer = (section.get("skip") == "outer")

                if type == "CGAL":
                    mesher = GoSmartMesherCGAL(self.logger)
                elif type == "axisymmetric":
                    mesher = GoSmartMesherAxisymmetric(self.logger)
                else:
                    self.logger.print_fatal("Unknown mesher type %s in %s" % (type, filename))

                if not skip_outer and not skip:
                    self.add_component('mesher', mesher)

                mesher.parse_config(section)

                if self.has_inner:
                    self.mesher_inner = []

                    self.logger.add_region("catheter", None, ("zone",), zone=True)
                    self.logger.add_region("slot", None, ("zone",), zone=True)
                    self.logger.add_region("dielectric cable", None, ("zone",), zone=True)

                    self.components['mesher_inner'] = self.mesher_inner
                    for inner_node in section.iterfind('inner'):
                        inner_type = inner_node.get('type')
                        inner_name = inner_node.get('name')

                        if inner_name is None:
                            inner_name = "mesher_inner"
                        else:
                            inner_name = "mesher_inner-%s" % slugify(inner_name)

                        if inner_type == "CGAL":
                            component = GoSmartMesherCGAL(self.logger)
                        elif inner_type == "axisymmetric":
                            component = GoSmartMesherAxisymmetric(self.logger)
                        else:
                            self.logger.print_fatal("Unknown mesher type %s in %s" % (inner_type, filename))
                        component.suffix = inner_name

                        component.parse_config(section)

                        template_set = inner_node.get("template")
                        if template_set is not None:
                            template_filename = "templates/inner-%s.xml" % template_set
                            with open(os.path.join(self.logger.get_cwd(), gosmart.config.template_directory, template_filename), "r") as template_file:
                                configtree = ET.parse(template_file)
                                root = configtree.getroot()
                                component.parse_config(root)

                        component.parse_config(inner_node)

                        if inner_node.get("region") is not None:
                            component.alter_extent(inner_node.get("region"))

                        if inner_node.get("needle") is not None:
                            if inner_node.get("needle") == "inner only":
                                if not skip_outer and not skip:
                                    mesher.skip_needle = True
                            elif inner_node.get("needle") == "outer only":
                                component.skip_needle = True
                            elif inner_node.get("needle") != "both":
                                self.logger.print_fatal("If supplied, 'needle' parameter to inner mesh should be 'inner only', 'outer only' or 'both'")

                        self.mesher_inner.append(component)
                        component.skip = skip

            elif section.tag == 'optimizer' and not skip:
                self.add_component('optimizer', GoSmartMeshOptimizer(self.logger))
            elif section.tag == 'lesion' and not skip:
                lesion = self.add_component('lesion', GoSmartLesion(self.logger))
                lesion.parse_config(section)
            elif section.tag == 'elmergrid' and not skip:
                elmergrid = self.add_component('elmergrid', GoSmartElmerGrid(self.logger))
                elmergrid.parse_config(section)
            elif section.tag == 'elmer' and not skip:
                elmer = self.add_component('elmer', GoSmartElmer(self.logger, elmer_binary=self.elmer_binary))
                elmer.parse_config(section)
            elif section.tag == 'constants' and not skip:
                default_set = section.get("defaults")

                if default_set is not None:
                    self.logger._load_constant_set(default_set)

                for constant in section:
                    value = constant.get("value")
                    if value is None:
                        value = constant
                    self.logger.add_or_update_constant(constant.get("name"), value, True, constant.tag, typ=constant.get("type"))
            elif not skip:
                self.logger.print_fatal("Unknown top-level section %s in %s" % (section.tag, filename))

            if skip:
                self.logger.print_line("* Skipping %s as indicated in settings file %s" % (section.tag, filename))

        self.logger.print_debug(self.logger.zones)
        self.logger.print_debug(self.logger.surfaces)
        if hasattr(self, "elmer"):
            self.elmer.configfiles.append(filename)

    def parse_config_files(self):
        if len(self.configfiles) == 0:
            self.logger.print_fatal("No config files were supplied")

        for configfile in self.configfiles:
            self.parse_config_file(configfile)
