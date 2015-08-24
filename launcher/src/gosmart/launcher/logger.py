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
import re
import os
import errno

from gosmart.launcher.component import GoSmartComponent
from gosmart.launcher.globals import debug, slugify, colorama_imported
from gosmart.launcher.errors import exceptions

if colorama_imported:
    import colorama


class GoSmartLogger(GoSmartComponent):
    suffix = 'logger'
    version = None
    _global_working_directory = None

    def __init__(self, runname, silent=False, debug=debug, logfile=None, global_working_directory=None):
        super().__init__(self)
        self.runname = runname
        self.silent = silent
        self.debug = debug

        self._colour_stripper = re.compile(r'\x1b[^m]*m')
        self._global_working_directory = global_working_directory

        # Note that these _may_ get renumbered partway through!
        self.zones = {}
        self.zones_map = {}
        self.zones_excluded = {}

        self.surfaces = {}
        self.surfaces_map = {}

        self.files = {}

        self._constant_needle_mapping = {}
        self._constant_needle_mapping_types = {}

        self._current_i = {
            "surfaces": 0,
            "zones": 0
        }

        self.geometry = {
            "centre": {'x': 0, 'y': 0, 'z': 0},
            "needleaxis": [],
            "simulationscaling": 1.,
        }

        self._load_constant_set("default")

        absolute_path = os.path.join(self.logger.get_cwd(), self.suffix)
        try:
            os.makedirs(absolute_path)
        except OSError as e:
            if e.errno != errno.EEXIST:
                self.print_fatal("Could not create %s directory: %s" %
                                 (absolute_path, str(e)))

        self.logfile = open(os.path.join(self.get_cwd(), logfile), "w") if logfile is not None else None

        self.add_region("tissue", None, ("zone", "tissues", "background"), primary=True, zone=True)

    def flush_logfile(self):
        if self.logfile is not None:
            self.logfile.flush()

    def find_regions_by_group(self, group):
        return [r for r in self.regions.values() if group in r["groups"]]

    def get_region_ids(self):
        regions = {}

        for lregions, sifregion in ((self.surfaces, "Boundaries"), (self.zones, "Bodies")):
            region_groups = {}
            for name, region in lregions.items():
                self.print_debug(("REGION %s: " % name) + str(region["groups"]))
                regions["REGION_%s" % slugify(name)] = region["id"]
                for group in region["groups"]:
                    for grouptype in ("REGIONS", sifregion.upper()):
                        group = "%s_%s" % (grouptype, slugify(group))
                        if group not in region_groups:
                            region_groups[group] = []
                        if region["id"] >= 0:
                            region_groups[group].append(region["id"])

            regions.update(dict((g, "Target %s(%d) = Integer %s" % (sifregion, len(set(l)), " ".join(str(i) for i in set(l)))) for g, l in region_groups.items()))

        return regions

    def get_cwd(self):
        if self._global_working_directory is not None:
            return os.path.abspath(self._global_working_directory)

        return os.path.abspath(".")

    def make_cwd(self, cwd):
        if self._global_working_directory is not None:
            return os.path.join(self._global_working_directory, cwd)

        return cwd

    def add_file(self, nature, name, filename):
        if nature not in self.files:
            self.files[nature] = {}
        if filename[0] != '/':
            filename = os.path.realpath(os.path.join(self.logger.get_cwd(), filename))
        self.files[nature][name] = filename

    def get_file(self, nature, name):
        if nature in self.files and name in self.files[nature]:
            return self.files[nature][name]
        return None

    def add_region(self, name, filename, groups, primary=False, unnumbered=False, zone=False, i="max"):
        if zone == "both":
            num = self.add_region(name, filename, groups, primary, unnumbered, True, i="max")
            self.add_region(name, filename, groups, primary, unnumbered, False, i=num)
            return num

        if i == "max":
            i = max(self._current_i["zones"], self._current_i["surfaces"]) + 1

        if zone:
            regions = self.zones
            regions_map = self.zones_map
            if i is None:
                i = self._current_i["zones"] + 1
            self._current_i["zones"] = i
        else:
            regions = self.surfaces
            regions_map = self.surfaces_map
            if i is None:
                i = self._current_i["surfaces"] + 1
            self._current_i["surfaces"] = i

        if primary and unnumbered:
            self.debug_fatal("Primary region must be numbered")

        if unnumbered:
            i = -i

        if primary and (1 in regions_map):
            regions[regions_map[1]]["id"] = i
            regions_map[i] = regions_map[1]
            i = 1

        regions[name] = {"id": i, "filename": filename, "groups": ["all", name] + list(groups)}
        self.print_debug("%s : %d" % (name, i))
        regions_map[i] = name

        return i

    def ensure_constant(self, name, value, warn=False, group="CONSTANT", typ=None):
        self.add_or_update_constant(name, value, warn, group, override=False, typ=typ)

    def ensure_needle(self, needle):
        if needle not in self._constant_needle_mapping:
            self._constant_needle_mapping[needle] = {}
            self._constant_needle_mapping_types[needle] = {}

    def add_or_update_needle_constant(self, needle, name, value, typ=None):
        self.ensure_needle(needle)
        name = slugify(name)
        self._constant_needle_mapping[needle][name] = value
        self._constant_needle_mapping_types[needle][name] = {}

    def add_or_update_constant(self, name, value, warn=False, group="CONSTANT", override=True, typ=None):
        # TODO:PTW:THIS SHOULD BE REPLACED WITH SLUGIFY!!!
        if slugify(group) == "PARAMETER":
            mangled_name = slugify(name)
        else:
            mangled_name = "%s_%s" % (slugify(group), slugify(name))

        if override or mangled_name not in self._constant_mapping:
            self._constant_mapping[mangled_name] = value
            self._constant_mapping_types[mangled_name] = typ

        if warn:
            self._constant_mapping_warn[mangled_name] = "constant : %s" % name

    def get_constant(self, name, group="CONSTANT"):
        mangled_name = "%s_%s" % (slugify(group), slugify(name))

        if name in self._constant_mapping:
            return self._constant_mapping[name]
        elif mangled_name in self._constant_mapping:
            return self._constant_mapping[mangled_name]
        return None

    def get_needle_constants(self):
        constants = self._constant_needle_mapping.copy()
        return constants

    def get_needle_constant(self, needle, name):
        if needle in self._constant_needle_mapping \
           and name in self._constant_needle_mapping[needle]:
            return self._constant_needle_mapping[needle][name]

        return None

    def get_constants(self):
        constants = self._constant_mapping.copy()
        return constants

    def get_mapping_warn(self):
        warn = self._constant_mapping_warn.copy()
        return warn

    def print_debug(self, line='', prefix='| '):
        if self.debug:
            self.print_line(line, prefix, color="CYAN")

    def print_line(self, line='', prefix='| ', color="GREEN", color_text=True, color_bright=False):
        line_plain = self._colour_stripper.sub('', str(line))

        if not self.silent:
            if colorama_imported:
                color = getattr(colorama.Fore, color)
                reset = colorama.Fore.RESET

                if color_bright:
                    color += colorama.Style.BRIGHT
                    reset = colorama.Style.RESET_ALL + reset

                if color_text:
                    print(color + prefix + str(line) + reset)
                else:
                    print(color + prefix + reset + str(line))
            else:
                print(prefix + line_plain)

        self.logfile_print(prefix + line_plain)

    def print_error(self, error, color="RED"):
        if colorama_imported:
            color = getattr(colorama.Fore, color)
            print(colorama.Fore.GREEN + error + colorama.Fore.RESET)
        else:
            print(error)

        self.logfile_print("***ERROR*** : %s" % error)

    def logfile_print(self, line):
        if self.logfile is not None:
            self.logfile.write(str(line).strip() + "\n")

    def print_fatal(self, error, code="E_UNKNOWN"):
        self.print_error(error)

        raise exceptions[code](error)
