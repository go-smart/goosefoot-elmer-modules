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
import os
import yaml

from gosmart.launcher.mesher import GoSmartMesher


class GoSmartMesherCGAL(GoSmartMesher):
    mesher_binary = 'mesher_cgal'
    logpick_pairs = (('Convert extent', 'Surface composition complete', 'Surface generation'),
                     ('Tetrahedralizing', 'Done', 'Tetrahedralization'),
                     ('Accelerating distance queries', 'Assigning boundary indices', 'Acceleration'),
                     ('Assigning boundary indices', 'Write to GMSH', 'Index assignment'))
    vessels_are_nearfield = True
    needles_are_nearfield = True
    solid_zonefield = False
    zone_boundaries = False
    suppress_logging_over_per_second = (3, 3)
    todos = ("PTW: missing centre causes segfault",)

    tags = {
        "organ": "--organ",
        "zones": "--zones",
        "tissueid": "--tissueid",
        "vessels": "--vessels",
        "extent": "--extent",
        "needles": "--needles",
        "combined": "--combined_surface",
        "boundary": "--boundary_surface",
        "structure": "--structure_surface"
    }

    def __init__(self, logger):
        super().__init__(logger)

        self.file_locations = {
        }

        self.tetrahedralize_only = False
        self.output_prefix = None
        self.nearfield = 0.3
        self.farfield = 3.0
        self.zonefield = 0.3
        self.zones_are_zonefield = True
        self.granularity = 1.0
        self.zone_radius = 0.
        self.needle_characteristic_length = None

        self.zone_priorities = {}
        self.zone_characteristic_lengths = {}
        self.zone_activity_spheres = {}

        self._meshed_regions = {}

    def alter_extent(self, filename):
        self.file_locations["extent"] = filename

    def parse_config(self, config_node):
        super().parse_config(config_node)

        centre_node = config_node.find('centre')
        if centre_node is not None:
            self.mesh['centre'] = list()
            for c in ('x', 'y', 'z'):
                xyz = self.logger.geometry['centre'][c]
                if centre_node.get(c) is not None:
                    xyz += float(centre_node.get(c))
                self.mesh['centre'].append(xyz)

            if centre_node.get("radius") is not None:
                self.mesh['centre_radius'] = centre_node.get("radius")

        self.tetrahedralize_only = (config_node.get("tetrahedralize_only") == "true")
        self.zone_boundaries = (config_node.get("zone_boundaries") == "true")

        for node in config_node:
            plural = '%ss' % str(node.tag)  # Unfortunately simplistic plural spotting, if needs be use 'inflect'

            if node.tag in self.tags:
                if node.get('skip') == 'true':
                    if node.tag in self.file_locations:
                        del self.file_locations[node.tag]
                elif node.get('region') is not None:
                    self.file_locations[node.tag] = node.get('region')
            elif plural in self.tags:
                if node.get('skip') == 'true':
                    if plural in self.file_locations:
                        del self.file_locations[plural]
                else:
                    if plural not in self.file_locations:
                        self.file_locations[plural] = []
                    self.file_locations[plural].append(node.get('region'))
            elif node.tag == 'lengthscales':
                self.nearfield = float(node.get('nearfield'))
                self.farfield = float(node.get('farfield'))
                self.zonefield = self.nearfield
                if node.get('zonefield') is not None:
                    self.zonefield = float(node.get('zonefield'))
                elif node.get('zonefield') == 'ignore':
                    self.zones_are_zonefield = False

                if node.get('granularity'):
                    self.granularity = float(node.get('granularity'))
                if node.get('vessels'):
                    self.vessels_are_nearfield = (node.get('vessels') != 'far')
                if node.get('needles'):
                    self.needles_are_nearfield = (node.get('needles') != 'far')
                if node.get('zones'):
                    self.solid_zonefield = (node.get('zones') == 'solid')
                if node.get('zone_radius'):
                    self.zone_radius = float(node.get('zone_radius'))

                needle_characteristic_length = node.get('needlezonefield')
                if needle_characteristic_length:
                    self.needle_characteristic_length = float(needle_characteristic_length)

            if node.tag == 'zone' or node.tag == 'needle':
                region = node.get('region')
                if node.get('characteristic_length') is not None:
                    self.zone_characteristic_lengths[region] = node.get('characteristic_length')
                if node.get('priority') is not None:
                    self.zone_priorities[region] = node.get('priority')
                activity = node.find('activity')
                if activity is not None:
                    inactive_region = region + " inactive"
                    region_detail = self.logger.get_region(region)
                    if not region_detail:
                        continue
                    inactive_groups = tuple(g + "-inactive" for g in region_detail["groups"])
                    if inactive_region not in self.logger.surfaces:
                        self.logger.add_region(inactive_region, None, inactive_groups, zone=False)
                    inactive_index = self.logger.surfaces[inactive_region]["id"]
                    self.zone_activity_spheres[region] = {
                        'x': activity.get('x'),
                        'y': activity.get('y'),
                        'z': activity.get('z'),
                        'r': activity.get('r'),
                        'i': inactive_index
                    }

            if node.tag == 'extent' and node.get('radius') is not None:
                self.mesh['bounding_radius'] = node.get('radius')

    def _prep_zone_arg(self, tag, surface=False):
        if surface:
            zone_filename = str(self.logger.surfaces[tag]["filename"])
            zone_id = self.logger.surfaces[tag]["id"]

            self._meshed_regions[tag] = {"meshed_as": "surface"}
            self._meshed_regions[tag].update(self.logger.surfaces[tag])
            zone_id *= -1
        else:
            zone_filename = str(self.logger.zones[tag]["filename"])
            zone_id = self.logger.zones[tag]["id"]

            self._meshed_regions[tag] = {"meshed_as": "zone"}
            self._meshed_regions[tag].update(self.logger.zones[tag])

        out = "%s:%d" % (zone_filename, zone_id)
        if tag in self.zone_characteristic_lengths:
            out += ":" + self.zone_characteristic_lengths[tag]
        elif tag in self.zone_priorities or tag in self.zone_activity_spheres:
            out += ":-1"

        if tag in self.zone_priorities:
            out += ":" + self.zone_priorities[tag]
        elif tag in self.zone_activity_spheres:
            out += ":0"

        if tag in self.zone_activity_spheres:
            sphere = self.zone_activity_spheres[tag]
            out += ":" + "_".join(str(sphere[s]) for s in ('x', 'y', 'z', 'r', 'i'))

        return out

    def launch(self, needle_files, extent_file, preprocessor=None, appendix=""):
        file_locations = {}

        if self.skip_needle and "needles" in self.file_locations:
            del self.file_locations["needles"]

        if preprocessor is not None:
            for f in preprocessor.components:
                if f in self.logger.surfaces:
                    self.logger.surfaces[f]["filename"] = preprocessor.launch(self.logger.surfaces[f]["filename"])

        for composite in ("combined", "boundary", "structure"):
            if composite not in self.logger.surfaces:
                self.logger.add_region(composite, os.path.join(self.logger.make_cwd(self.suffix), "_%s.off" % composite), (), unnumbered=True)

        if not self.skip_needle and ("needles" not in self.file_locations or len(self.file_locations["needles"]) == 0) and needle_files is not None:
            #for name, filename in needle_files.items():
            #    if "needles" not in self.logger.surfaces:
            #        self.logger.add_region("needle-%s" % name, filename, ("needles",))
            needles = ["needle-%s" % i for i in needle_files.keys()]

            if "zones" not in self.file_locations:
                self.file_locations["zones"] = []
            if "needles" not in self.file_locations:
                self.file_locations["needles"] = []

            for needle in needles:
                if needle not in self.file_locations["zones"]:
                    self.file_locations["zones"].append(needle)
                if needle not in self.zone_characteristic_lengths and self.needle_characteristic_length is not None:
                    self.zone_characteristic_lengths[needle] = str(self.needle_characteristic_length)
                if needle not in self.zone_priorities:
                    self.zone_priorities[needle] = "-1000"

        if "extent" not in self.logger.surfaces:
            if extent_file is not None:
                location = os.path.join(self.logger.make_cwd(self.suffix), "extent.stl")
            else:
                location = None
            self.logger.add_region("extent", location, ("boundary",), primary=True)

        for k, v in self.file_locations.items():
            if isinstance(v, str):
                v = (v,)
            try:
                file_locations[k] = [self._prep_zone_arg(l, l not in self.logger.zones) for l in v]
            except KeyError as e:
                self.logger.print_fatal("Key missing for '%s' in file locations" % e.args[0])

        self.output_prefix = "%s/%s" % (self.suffix, self.logger.runname)

        args = [
            "--output_prefix", self.output_prefix,
            "--nearfield", str(self.nearfield),
            "--farfield", str(self.farfield),
            "--zonefield", str(self.zonefield),
            "--granularity", str(self.granularity),
            "--zone_radius", str(self.zone_radius),
            "--output_gmsh"
        ]

        if "centre" in self.mesh:
            args += ["--dense_centre", "--centre"] + list(map(str, self.mesh["centre"]))

        if "centre_radius" in self.mesh:
            args += ["--centre_radius", self.mesh["centre_radius"]]

        if "bounding_radius" in self.mesh:
            args += ["--bounding_radius", self.mesh["bounding_radius"]]

        for tag in filter(lambda x: x in file_locations, self.tags):
            if isinstance(file_locations[tag], list):
                if not file_locations[tag]:
                    continue
                args += [self.tags[tag]] + file_locations[tag]
            else:
                args += [self.tags[tag], file_locations[tag]]

        if self.tetrahedralize_only:
            args.append("--tetrahedralize-only")

        if self.zone_boundaries:
            args.append("--mark_zone_boundaries")

        if self.vessels_are_nearfield:
            args.append("--boundary_tree")

        if not self.needles_are_nearfield:
            args.append("--omit_needles_tree")

        if not self.zones_are_zonefield:
            args.append("--omit_zones_tree")

        if self.solid_zonefield:
            args.append("--solid_zone")

        if "extent" in self.logger.surfaces:
            args += ["--extent_index", self.logger.surfaces["extent"]["id"]]
            self._meshed_regions["extent"] = {"meshed_as": "surface"}
            self._meshed_regions["extent"].update(self.logger.surfaces["extent"])

        if "tissue" in self.logger.zones:
            args += ["--tissueid", self.logger.zones["tissue"]["id"]]
            self._meshed_regions["tissue"] = {"meshed_as": "zone"}
            self._meshed_regions["tissue"].update(self.logger.zones["tissue"])

        success = super().launch(needle_files, extent_file, args, appendix)

        with open(os.path.join(self.logger.make_cwd(self.suffix), "mesh_labelling.yml"), 'w') as f:
            yaml.dump(self._meshed_regions, f, default_flow_style=False)

        return success
