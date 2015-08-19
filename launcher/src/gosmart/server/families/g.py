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


class GalilFoamFamily(metaclass=Family):
    family_name = "galilFoam"
    _docker_image = 'gosmart/galilfoam'

    _py = None

    def __init__(self, files_required):
        self._needles = {}
        self._needle_order = {}
        self._files_required = files_required
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
        regions_yaml = os.path.join(working_directory, "input", "regions.yml")
        with open(regions_yaml, "w") as f:
            yaml.dump(self._regions, f, default_flow_style=False)

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

        with open(os.path.join(working_directory, "start.py"), "w") as f:
            f.write(self._py)

        loop = asyncio.get_event_loop()
        success = yield from self._submitter.run_script(
            loop,
            working_directory,
            self._docker_image,
            self._files_required.keys(),
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
