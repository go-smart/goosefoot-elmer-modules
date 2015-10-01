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
import collections
import csv
import numpy as N
from gosmart.launcher.errors import GoSmartModelError
from gosmart.launcher.globals import \
    _generate_rotation_matrix_numpy
M = N.matrix


class GoSmartElmerProbeLocation(collections.OrderedDict):
    time = None


class GoSmartElmerProbeLocationFactory:
    def generate_locations(self):
        return None

    def parse_config(self, config_node, logger=None):
        pass


class GoSmartElmerProbeLocationFactoryUmbrellaTines(GoSmartElmerProbeLocationFactory):
    def generate_locations(self):
        locations = {}
        location_times = {}

        intersection = M([[self.target[i] for i in ('x', 'y', 'z')]]).T

        for i, extentry in enumerate(sorted(self.extensions.items())):
            time, extension = extentry
            location_times[i] = time
            locations[i] = GoSmartElmerProbeLocation()
            locations[i].time = time

            p10 = M([[extension, 0.0, 0.0]]).T
            radi = 36 * N.pi / 180

            Rz = M([[N.cos(radi), N.sin(radi), 0], [-N.sin(radi), N.cos(radi), 0], [0, 0, 1]])
            p1 = Rz * p10
            tuplify = lambda t: tuple(*N.array(t.T[0]))
            # locations[i]["ends"] = [(tuplify(p1 + intersection), 1), (tuplify(p10 + intersection), 2)]
            locations[i]["ends"] = [(tuplify(p1 + intersection), -1), (tuplify(p10 + intersection), -1)]
            # locations[i]["middles"] = [(tuplify(p1 / 2 + intersection), 1), (tuplify(p10 / 2 + intersection), 2)]
            locations[i]["middles"] = [(tuplify(p1 / 2 + intersection), -1), (tuplify(p10 / 2 + intersection), -1)]
            # controlling_thermocouple = 2
            controlling_thermocouple = -1

            for j in range(2, 10):
                angle = (j - 1) * radi
                Rxm = M([[1, 0, 0], [0, N.cos(angle), N.sin(angle)], [0, -N.sin(angle), N.cos(angle)]])
                Rxe = M([[1, 0, 0], [0, 0, 1], [0, -1, 0]])
                pen = (Rxe * p1) + intersection
                pmn = (Rxm * p1) / 2 + intersection

                # controlling_thermocouple += j % 2
                locations[i]["ends"].append((tuplify(pen), controlling_thermocouple))
                locations[i]["middles"].append((tuplify(pmn), controlling_thermocouple))
            locations[i]["middles"].append((tuplify(intersection), -1))

            locations[i]["thermocouples"] = [locations[i]["ends"][0]]
            locations[i]["thermocouples"] += locations[i]["ends"][1:8:2]

        return locations

    def parse_config(self, config_node, logger):
        self.needleaxis = logger.geometry["needleaxis"]
        self.scale = logger.geometry["simulationscaling"]
        self.target = logger.geometry["centre"]

        if config_node.get('offset') is not None:
            offset = list(map(float, config_node.get('offset').split(' ')))
            self.target['x'] += offset[0]
            self.target['y'] += offset[1]
            self.target['z'] += offset[2]

        for t in self.target.keys():
            self.target[t] *= self.scale
        self.extensions = {}
        for node in config_node:
            if node.tag == "extensions":
                for extension in node:
                    if extension.get("time") is not None:
                        self.extensions[extension.get("time")] = float(extension.get("length"))
                    else:
                        self.extensions[extension.get("phase")] = float(extension.get("length"))
            else:
                raise GoSmartModelError("Unrecognised tag describing needle in simulation")


# Based on Mika's needle.m file
class GoSmartElmerProbeLocationFactoryExtrapolated(GoSmartElmerProbeLocationFactory):
    def generate_locations(self):
        locations = {}
        location_times = {}

        intersection = M([[self.target[i] for i in ('x', 'y', 'z')]]).T
        needleaxis = M([[self.needleaxis[i] for i in ('x', 'y', 'z')]]).T
        needleaxis = needleaxis / N.sqrt(needleaxis.dot(needleaxis))

        ax, ay, az = [self.needleaxis[i] for i in ('x', 'y', 'z')]
        VV = N.array([ax, ay, az])
        # FIXME: this seems wrong.... surely...
        if(N.linalg.norm(VV) >= 1.1):
            ax = ax * 0.001
            ay = ay * 0.001
            az = az * 0.001

        max_extension = max(self.extensions.values())
        tuplify = lambda t: tuple(*N.array(t.T[0]))

        for i, extentry in enumerate(sorted(self.extensions.items())):
            time, extension = extentry
            location_times[i] = time
            locations[i] = GoSmartElmerProbeLocation()
            locations[i].time = time

            controlling_thermocouple = 0
            locations[i]["ends"] = []
            locations[i]["middles"] = []
            for j in range(0, 9):
                controlling_thermocouple += j % 2
                point = M([self.points[j]]).T

                adjusted_end = (extension / max_extension) * (point - intersection) + intersection
                locations[i]["ends"].append((tuplify(adjusted_end), controlling_thermocouple))

                adjusted_middle = 0.5 * (extension / max_extension) * (point - intersection) + intersection
                locations[i]["middles"].append((tuplify(adjusted_middle), controlling_thermocouple))

            trocar = intersection + 0.5 * extension * needleaxis  # Treated as mid-point length
            locations[i]["middles"].append((tuplify(intersection), -1))
            locations[i]["ends"].append((tuplify(trocar), -1))

            locations[i]["thermocouples"] = [locations[i]["ends"][0]]
            locations[i]["thermocouples"] += locations[i]["ends"][1:8:2]

        return locations

    def parse_config(self, config_node, logger):
        self.needleaxis = logger.geometry["needleaxis"][0]
        self.scale = logger.geometry["simulationscaling"]
        self.target = logger.geometry["centre"]

        if config_node.get('offset') is not None:
            offset = list(map(float, config_node.get('offset').split(' ')))
            self.target['x'] += offset[0]
            self.target['y'] += offset[1]
            self.target['z'] += offset[2]

        for t in self.target.keys():
            self.target[t] *= self.scale

        self.extensions = {}
        self.points = {}
        for node in config_node:
            if node.tag == "extensions":
                for extension in node:
                    if extension.get("time") is not None:
                        self.extensions[extension.get("time")] = float(extension.get("length"))
                    else:
                        self.extensions[extension.get("phase")] = float(extension.get("length"))
            elif node.tag == "points":
                for point in node:
                    self.points[int(point.get("i"))] = [float(point.get(x)) for x in ('x', 'y', 'z')]
            else:
                raise GoSmartModelError("Unrecognised tag describing needle in simulation")


# Based on Mika's needle.m file
class GoSmartElmerProbeLocationFactoryStraightTines(GoSmartElmerProbeLocationFactory):
    def generate_locations(self):
        locations = {}
        location_times = {}

        intersection = M([[self.target[i] for i in ('x', 'y', 'z')]]).T

        ax, ay, az = [self.needleaxis[i] for i in ('x', 'y', 'z')]

        VV=N.array([ax,ay,az])
        if(N.linalg.norm(VV)>=1.1):
          ax=ax*0.001
          ay=ay*0.001
          az=az*0.001
        R = _generate_rotation_matrix_numpy(ax, ay, az, backward=False, rx=-1, ry=0, rz=0)
        print("------>", R, "<-----")
        for i, extentry in enumerate(sorted(self.extensions.items())):
            time, extension = extentry
            location_times[i] = time
            locations[i] = GoSmartElmerProbeLocation()
            locations[i].time = time

            p9 = M([[extension, 0.0, 0.0]]).T
            trocar = M([[- extension * 0.5, 0.0, 0.0]]).T
            radi = 45 * N.pi / 180

            Rz = M([[N.cos(radi), N.sin(radi), 0], [-N.sin(radi), N.cos(radi), 0], [0, 0, 1]])
            p1 = Rz * p9
            tuplify = lambda t: tuple(*N.array(t.T[0]))
            locations[i]["ends"] = [(tuplify(R * p1 + intersection), 1), (tuplify(R * p9 + intersection), 2)]
            locations[i]["middles"] = [(tuplify(R * p1 / 2 + intersection), 1), (tuplify(R * p9 / 2 + intersection), 2)]
            controlling_thermocouple = 2

            for j in range(2, 9):
                angle = (j - 1) * radi
                Rx = M([[1, 0, 0], [0, N.cos(angle), N.sin(angle)], [0, -N.sin(angle), N.cos(angle)]])
                pen = R * (Rx * p1) + intersection
                pmn = R * (Rx * p1) / 2 + intersection

                controlling_thermocouple += j % 2
                locations[i]["ends"].append((tuplify(pen), controlling_thermocouple))
                locations[i]["middles"].append((tuplify(pmn), controlling_thermocouple))
            locations[i]["middles"].append((tuplify(intersection), -1))
            locations[i]["ends"].append((tuplify(R * trocar + intersection), -1))

            locations[i]["thermocouples"] = [locations[i]["ends"][0]]
            locations[i]["thermocouples"] += locations[i]["ends"][1:8:2]
        return locations

    def parse_config(self, config_node, logger):
        self.needleaxis = logger.geometry["needleaxis"][0]
        self.scale = logger.geometry["simulationscaling"]
        self.target = logger.geometry["centre"]

        if config_node.get('offset') is not None:
            offset = list(map(float, config_node.get('offset').split(' ')))
            self.target['x'] += offset[0]
            self.target['y'] += offset[1]
            self.target['z'] += offset[2]

        for t in self.target.keys():
            self.target[t] *= self.scale
        self.extensions = {}
        for node in config_node:
            if node.tag == "extensions":
                for extension in node:
                    if extension.get("time") is not None:
                        self.extensions[extension.get("time")] = float(extension.get("length"))
                    else:
                        self.extensions[extension.get("phase")] = float(extension.get("length"))
            else:
                raise GoSmartModelError("Unrecognised tag describing needle in simulation")


class GoSmartElmerProbeLocationFactoryManual(GoSmartElmerProbeLocationFactory):
    def generate_locations(self):
        return self.locations

    def parse_config(self, config_node, logger=None):
        self.locations = {}
        self.locations[1] = GoSmartElmerProbeLocation()
        self.locations[1].time = 0.0

        for section in config_node:
            if section.tag not in self.locations:
                self.locations[1][section.tag] = []
            if section.get('input') == 'block':
                csvtext = map(lambda x: x.strip(), section.text.strip().split("\n"))
                csvreader = csv.reader(csvtext, delimiter="\t")
                for row in csvreader:
                    self.locations[1][section.tag].append(tuple(map(float, row)))
            else:
                for node in section:
                    self.locations[1][section.tag].append(
                        tuple(map(float, (node.get(c) for c in ('x', 'y', 'z', 't'))))
                    )


probe_location_factories = {
    "manual": GoSmartElmerProbeLocationFactoryManual,
    "extrapolated": GoSmartElmerProbeLocationFactoryExtrapolated,
    "straight tines": GoSmartElmerProbeLocationFactoryStraightTines,
    "umbrella tines": GoSmartElmerProbeLocationFactoryUmbrellaTines,
}
