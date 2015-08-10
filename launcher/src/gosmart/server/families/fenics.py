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
from gosmart.server.family import Family
from gosmart.server.parameters import read_parameters, convert_parameter


import os
import json
import asyncio
from gosmart.server.docker import Submitter
import traceback
import sys
import math
from lxml import etree as ET
import shutil
import yaml


class GoSmartSimulationFrameworkArguments:
    def __init__(self, elmer_binary=None, outfilename=None, addpid=False, silent=True,
                 debug=False, nprocs=None, baw=True, only=None, leavetree=False, configfilenames=[]):
        self.elmer_binary = elmer_binary
        self.outfilename = outfilename
        self.addpid = addpid
        self.silent = silent
        self.debug = debug
        self.nprocs = nprocs
        self.baw = baw
        self.only = only
        self.leavetree = leavetree
        self.configfilenames = configfilenames

    def to_list(self):
        args = {
            '--elmer': self.elmer_binary,
            '--elmer-logfile': self.outfilename,
            '--logfile-addpid': self.addpid,
            '--silent': self.silent,
            '--debug': self.debug,
            '--nprocs': self.nprocs,
            '--only': self.only,
            '--black-and-white': self.baw,
            '--leavetree': self.leavetree
        }
        command_line = []
        for k, v in args.items():
            if v is not None:
                if isinstance(v, bool):
                    if v:
                        command_line += [k]
                else:
                    command_line += [k, str(v)]
        return command_line + self.configfilenames


class MesherGSSF:
    _xml = None

    def __init__(self, files_required):
        self._needles = {}
        self._needle_order = {}
        self._files_required = files_required
        self._args = GoSmartSimulationFrameworkArguments(configfilenames=["settings.xml"])

    # Needle index can be either needle index (as given in XML input) or an
    # integer n indicating the nth needle in the order of the needles XML block
    def get_needle_parameter(self, needle_index, key, try_json=True):
        if needle_index not in self._needles and needle_index in self._needle_order:
            needle_index = self._needle_order[needle_index]

        value = self.get_parameter(key, try_json, self._needles[needle_index]["parameters"])

        return value

    def get_parameter(self, key, try_json=True, parameters=None):
        if parameters is None:
            parameters = self._parameters

        if key not in parameters:
            return None

        parameter, typ = parameters[key]

        return convert_parameter(parameter, typ, try_json)

    @asyncio.coroutine
    def mesh(self, working_directory):
        try:
            translated_xml = self.to_xml()
        except RuntimeError as e:
            traceback.print_exc(file=sys.stderr)
            raise e

        tree = ET.ElementTree(translated_xml)

        with open(os.path.join(working_directory, "settings.xml"), "wb") as f:
            tree.write(f, pretty_print=True)

        args = ["go-smart-launcher"] + self._args.to_list()

        task = yield from asyncio.create_subprocess_exec(
            *[a for a in args if a not in ('stdin', 'stdout', 'stderr')],
            cwd=working_directory
        )

        yield from task.wait()

        return task.returncode == 0

    def retrieve_files(self, destination, files):
        pass

    @asyncio.coroutine
    def clean(self):
        return True

    def load_definition(self, xml, parameters, algorithms):
        self._sif = xml.find('definition').text
        self._needles = {}
        self._regions = {}
        self._regions_by_meaning = {}

        needles = xml.find('needles')
        if needles is not None:
            k = 0
            for needle in needles:
                needle_file = needle.get("file")
                location = needle_file.split(':', 1)
                if location[0] in ('surface', 'zone'):
                    target_file = "%s%s" % (needle.get("index"), os.path.splitext(location[1])[1])
                    needle_file = "%s:%s" % (location[0], target_file)
                    self._files_required[os.path.join('input', target_file)] = location[1]  # Any changes to local/remote dirs here

                self._needles[needle.get("index")] = {
                    "parameters": read_parameters(needle.find("parameters")),
                    "file": needle_file,
                    "class": needle.get("class")
                }
                self._needle_order[k] = needle.get("index")
                k += 1

        self._parameters = parameters

        regions = xml.find('regions')
        for region in regions:
            if region.get('name') not in self._regions_by_meaning:
                self._regions_by_meaning[region.get('name')] = []

            try:
                target_file = "%s%s" % (region.get("id"), os.path.splitext(region.get('input'))[1])
            except AttributeError as e:
                print(region.get('name'), region.get('input'), region.get('groups'))
                raise e

            self._regions[region.get('id')] = {
                "format": region.get('format'),
                "meaning": region.get('name'),
                "input": target_file,
                "groups": json.loads(region.get('groups'))
            }
            if self.get_parameter("SETTING_ORGAN_AS_SUBDOMAIN") and region.get('name') == 'organ':
                self._regions[region.get('id')]["format"] = 'zone'
            if self._regions[region.get('id')]["format"] in ('surface', 'zone') and region.get('input'):
                self._files_required[os.path.join('input', target_file)] = region.get('input')  # Any changes to local/remote dirs here
            self._regions_by_meaning[region.get('name')].append(self._regions[region.get('id')])

        self._algorithms = algorithms
        self._definition = xml.find('definition').text

    def to_xml(self):
        root = ET.Element('gosmart')
        root.set('name', 'elmer_libnuma')
        root.set('version', '1.0.1')

        geometry = ET.Element('geometry')
        root.append(geometry)

        centre_location = self.get_parameter("CENTRE_LOCATION")
        if centre_location is None or centre_location == "first-needle":
            if self._needles:
                centre_location = self.get_needle_parameter(0, "NEEDLE_TIP_LOCATION")
        elif centre_location == "centroid-of-tips":
            if self._needles:
                needle_tips = [self.get_needle_parameter(i, "NEEDLE_TIP_LOCATION") for i in range(len(self._needles))]
                needle_tips = zip(*needle_tips)
                centre_location = [sum(tips) / len(self._needles) for tips in needle_tips]
                print(centre_location)

        if self._needles:
            needle_axis_node = ET.Element('needleaxis')
            tip_location = self.get_needle_parameter(0, "NEEDLE_TIP_LOCATION")
            entry_location = self.get_needle_parameter(0, "NEEDLE_ENTRY_LOCATION")
            norm = 0
            vec = []
            for c, vt, ve in zip(('x', 'y', 'z'), tip_location, entry_location):
                needle_axis_node.set(c, str(ve - vt))
                vec.append(ve - vt)
                norm += (ve - vt) * (ve - vt)
            geometry.append(needle_axis_node)

            offset = self.get_parameter("CENTRE_OFFSET")
            if offset is not None:
                for c, v in enumerate(centre_location):
                    centre_location[c] = v + offset * vec[c] / math.sqrt(norm)

        centre_location_node = ET.Element("centre")
        for c, v in zip(('x', 'y', 'z'), centre_location):
            centre_location_node.set(c, str(v))
        geometry.append(centre_location_node)

        if self.get_parameter("SIMULATION_SCALING") is not None:
            ET.SubElement(geometry, "simulationscaling") \
                .set("ratio",
                     str(self.get_parameter("SIMULATION_SCALING")))

        regions = ET.SubElement(root, "regions")
        for name, region in self._regions.items():
            regionNode = ET.SubElement(regions, region["format"])
            regionNode.set("name", name)
            regionNode.set("input", os.path.join("input/", region["input"]))
            regionNode.set("groups", "; ".join(region["groups"]))

        parameters = ET.SubElement(root, "constants")
        for key, parameterPair in self._parameters.items():
            parameter, typ = parameterPair
            parameterNode = ET.SubElement(parameters, "parameter")
            parameterNode.set("name", key)
            p = convert_parameter(parameter, typ)
            parameterNode.set("value", json.dumps(p))
            if typ is not None:
                parameterNode.set("type", typ)

        name_needle_regions = False

        needlelibrary = ET.SubElement(root, 'needlelibrary')
        solid_needles = self.get_parameter("SETTING_SOLID_NEEDLES")
        if solid_needles is not None:
            needlelibrary.set("zones", "true" if solid_needles is True else "false")
            name_needle_regions = True

        mesher = ET.SubElement(root, "mesher")
        mesher.set('type', 'CGAL')
        if self.get_parameter("SETTING_SOLID_NEEDLES") is True or self.get_parameter("SETTING_ZONE_BOUNDARIES") is True:
            mesher.set("zone_boundaries", "true")

        mesher_inner = self.get_parameter("SETTING_AXISYMMETRIC_INNER")
        if mesher_inner is not None:
            innerNode = ET.SubElement(mesher, "inner")
            innerNode.set("type", "axisymmetric")
            innerNode.set("template", mesher_inner)

        mesher_inner_coarse = self.get_parameter("SETTING_AXISYMMETRIC_INNER_COARSE")
        if mesher_inner_coarse is not None:
            innerNode = ET.SubElement(mesher, "inner")
            innerNode.set("type", "axisymmetric")
            innerNode.set("name", "coarse")
            innerNode.set("template", mesher_inner_coarse)

        extent = ET.SubElement(mesher, 'extent')
        radius = self.get_parameter("SIMULATION_DOMAIN_RADIUS")
        if radius is not None:
            extent.set('radius', str(radius))
        else:
            extent.set('radius', '50')  # TODO: This should be done in the parameters!!!!

        ET.SubElement(mesher, 'centre')

        for idx, region in self._regions.items():
            if region['meaning'] == 'organ':
                if self.get_parameter('SETTING_ORGAN_AS_SUBDOMAIN'):
                    if self.get_parameter('SETTING_ORGAN_AS_SUBDOMAIN'):
                        ET.SubElement(mesher, 'both').set('region', idx)
                    else:
                        ET.SubElement(mesher, 'zone').set('region', idx)
                else:
                    ET.SubElement(mesher, 'organ').set('region', idx)
            elif region['format'] == 'zone':
                ET.SubElement(mesher, 'zone').set('region', idx)
            elif 'vessels' in region['groups'] or 'bronchi' in region['groups']:
                ET.SubElement(mesher, 'vessel').set('region', idx)

        lengthscales = ET.SubElement(mesher, 'lengthscales')

        if self.get_parameter('RESOLUTION_HIGH'):
            lengthscale_settings = [
                ('nearfield', '1.0'), ('farfield', '2.0'), ('zonefield', '1.0'),
                ('vessels', 'far')
            ]
        else:
            lengthscale_settings = [
                ('nearfield', '2.0'), ('farfield', '5.0'), ('zonefield', '2.0'),
                ('vessels', 'far')
            ]

        nearfield = self.get_parameter('RESOLUTION_FIELD_NEAR')
        needlezonefield = self.get_parameter('RESOLUTION_FIELD_NEEDLE_ZONE')
        farfield = self.get_parameter('RESOLUTION_FIELD_FAR')
        zonefield = self.get_parameter('RESOLUTION_FIELD_ZONE')

        if nearfield:
            lengthscale_settings[0] = ('nearfield', nearfield)
        if farfield:
            lengthscale_settings[1] = ('farfield', farfield)
        if zonefield:
            lengthscale_settings[2] = ('zonefield', zonefield)

        for k, v in lengthscale_settings:
            lengthscales.set(k, str(v))
        if needlezonefield:
            lengthscales.set("needlezonefield", str(needlezonefield))

        ET.SubElement(root, 'optimizer')
        ET.SubElement(root, 'elmergrid')

        l = 0
        globalNeedlesNode = ET.SubElement(root, "needles")
        for ix, needle in self._needles.items():
            globalNeedleNode = ET.SubElement(globalNeedlesNode, "needle")
            l += 1
            globalNeedleNode.set("name", str(l))

            if needle['class'] in ('solid-boundary', 'boundary'):
                location = needle['file'].split(':', 1)

                needle_mesh = None
                if location[0] in ('surface', 'zone'):
                    needleNode = ET.SubElement(regions, location[0])
                    needleNode.set("name", str(l))
                    needleNode.set("input", os.path.join("input/", location[1]))
                    needleNode.set("groups", "needles")

                    needle_mesh = ET.SubElement(mesher, 'needle')
                    needle_mesh.set('region', str(l))
                    if needlezonefield and location[0] == 'zone':
                        needle_mesh.set("characteristic_length", needlezonefield)
                else:
                    needleNode = ET.SubElement(needlelibrary, 'needle')

                    if name_needle_regions:
                        needleNode.set("name", str(l))

                    if location[0] == 'library':
                        needleNode.set("id", location[1])
                    else:
                        needleNode.set("name", location[1])

                    tip_location = self.get_needle_parameter(ix, "NEEDLE_TIP_LOCATION")
                    entry_location = self.get_needle_parameter(ix, "NEEDLE_ENTRY_LOCATION")
                    needleNode.set("offset", " ".join(map(lambda c: str(c[1] - c[0]), zip(tip_location, centre_location))))
                    needleNode.set("axis", " ".join(map(lambda c: str(c[0] - c[1]), zip(entry_location, tip_location))))

                    parameters = ET.SubElement(globalNeedleNode, "parameters")
                    for key, parameterPair in needle["parameters"].items():
                        parameter, typ = parameterPair
                        parameterNode = ET.SubElement(parameters, "constant")
                        parameterNode.set("name", key)
                        parameterNode.set("value", str(convert_parameter(parameter, typ)))

                needle_active_length = self.get_needle_parameter(ix, "NEEDLE_ACTIVE_LENGTH")
                if needle_active_length is not None:
                    if needle_mesh is None:
                        needle_mesh = ET.SubElement(mesher, 'needle' if solid_needles else 'solid')
                        needle_mesh.set('region', str(l))
                    activity = ET.SubElement(needle_mesh, 'activity')
                    tip_location = self.get_needle_parameter(ix, "NEEDLE_TIP_LOCATION")
                    for c, vt in zip(('x', 'y', 'z'), tip_location):
                        activity.set(c, str(vt))
                    activity.set('r', str(needle_active_length))

        self._xml = root

        return self._xml


class FenicsFamily(metaclass=Family):
    family_name = "fenics"

    _py = None

    def __init__(self, files_required):
        self._needles = {}
        self._needle_order = {}
        self._files_required = files_required
        self._mesher_gssf = MesherGSSF(files_required)
        self._submitter = Submitter()

    # Needle index can be either needle index (as given in XML input) or an
    # integer n indicating the nth needle in the order of the needles XML block
    def get_needle_parameter(self, needle_index, key, try_json=True):
        if needle_index not in self._needles and needle_index in self._needle_order:
            needle_index = self._needle_order[needle_index]

        value = self.get_parameter(key, try_json, self._needles[needle_index]["parameters"])

        return value

    def get_parameter(self, key, try_json=True, parameters=None):
        if parameters is None:
            parameters = self._parameters

        if key not in parameters:
            return None

        parameter, typ = parameters[key]

        return convert_parameter(parameter, typ, try_json)

    @asyncio.coroutine
    def simulate(self, working_directory):
        success = yield from self._mesher_gssf.mesh(working_directory)
        if not success:
            return False

        msh_input = os.path.join(working_directory, "mesher", "elmer_libnuma.msh")
        mesh_labelling_yaml = os.path.join(working_directory, "mesher", "mesh_labelling.yml")
        shutil.copyfile(msh_input, os.path.join(working_directory, "input", "input.msh"))
        self._files_required["input.msh"] = msh_input

        with open(mesh_labelling_yaml, "r") as f:
            mesh_labelling = yaml.load(f)

        regions = mesh_labelling.copy()
        regions.update(self._regions)
        for k, v in regions.items():
            if k in mesh_labelling:
                v.update(mesh_labelling[k])

        regions_yaml = os.path.join(working_directory, "input", "regions.yml")
        with open(regions_yaml, "w") as f:
            yaml.dump(regions, f, default_flow_style=False)

        self._submitter.add_input(regions_yaml)

        parameters_yaml = os.path.join(working_directory, "input", "parameters.yml")
        parameters = self._parameters
        for k, v in parameters.items():
            parameters[k] = [v[1], v[0]]
        with open(parameters_yaml, "w") as f:
            yaml.dump(parameters, f, default_flow_style=False)
        self._submitter.add_input(parameters_yaml)

        needle_parameters_yaml = os.path.join(working_directory, "input", "needle_parameters.yml")
        for j, w in self._needles.items():
            needle_parameters = w['parameters']
            for k, v in needle_parameters.items():
                needle_parameters[k] = [v[1], v[0]]
            self._needles[j]['index'] = j
            self._needles[j]['parameters'] = needle_parameters

        with open(needle_parameters_yaml, "w") as f:
            yaml.dump_all(self._needles.values(), f, default_flow_style=False)
        self._submitter.add_input(needle_parameters_yaml)
        shutil.copyfile(needle_parameters_yaml, '/tmp/p.yml') #RMV

        with open(os.path.join(working_directory, "start.py"), "w") as f:
            f.write(self._py)

        loop = asyncio.get_event_loop()
        success = yield from self._submitter.run_script(
            loop,
            working_directory,
            self._files_required.keys()
        )

        return success

    def retrieve_files(self, destination, files):
        for f in files:
            print(f, '->', destination)
            print(self._submitter.copy_output(f, destination))

    @asyncio.coroutine
    def clean(self):
        yield from self._submitter.destroy()
        self._submitter.finalize()

    def load_definition(self, xml, parameters, algorithms):
        self._mesher_gssf.load_definition(xml, parameters, algorithms)

        self._py = xml.find('definition').text
        self._needles = {}
        self._regions = {}
        self._regions_by_meaning = {}

        needles = xml.find('needles')
        if needles is not None:
            k = 0
            for needle in needles:
                needle_file = needle.get("file")
                location = needle_file.split(':', 1)
                if location[0] in ('surface', 'zone'):
                    target_file = "%s%s" % (needle.get("index"), os.path.splitext(location[1])[1])
                    needle_file = "%s:%s" % (location[0], target_file)
                    self._files_required[os.path.join('input', target_file)] = location[1]  # Any changes to local/remote dirs here

                self._needles[needle.get("index")] = {
                    "parameters": read_parameters(needle.find("parameters")),
                    "file": needle_file,
                    "class": needle.get("class")
                }
                self._needle_order[k] = needle.get("index")
                k += 1

        self._parameters = parameters

        regions = xml.find('regions')
        for region in regions:
            if region.get('name') not in self._regions_by_meaning:
                self._regions_by_meaning[region.get('name')] = []

            try:
                target_file = "%s%s" % (region.get("id"), os.path.splitext(region.get('input'))[1])
            except AttributeError as e:
                print(region.get('name'), region.get('input'), region.get('groups'))
                raise e

            self._regions[region.get('id')] = {
                "format": region.get('format'),
                "meaning": region.get('name'),
                "input": target_file,
                "groups": json.loads(region.get('groups'))
            }
            if self.get_parameter("SETTING_ORGAN_AS_SUBDOMAIN") and region.get('name') == 'organ':
                self._regions[region.get('id')]["format"] = 'zone'
            if self._regions[region.get('id')]["format"] in ('surface', 'zone') and region.get('input'):
                self._files_required[os.path.join('input', target_file)] = region.get('input')  # Any changes to local/remote dirs here
            self._regions_by_meaning[region.get('name')].append(self._regions[region.get('id')])

        self._algorithms = algorithms
        self._definition = xml.find('definition').text
