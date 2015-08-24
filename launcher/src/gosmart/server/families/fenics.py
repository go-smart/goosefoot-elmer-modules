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
from gosmart.server.parameters import convert_parameter


import os
import asyncio
from gosmart.server.docker import Submitter
from gosmart.server.families.mesher_gssf import MesherGSSFMixin
import shutil
import yaml


class FenicsFamily(Family, MesherGSSFMixin):
    family_name = "fenics"
    _docker_image = 'gosmart/fenics-stable-ppa'

    _py = None

    def __init__(self, files_required):
        self._needles = {}
        self._needle_order = {}
        self._files_required = files_required
        self.init_mesher(files_required)
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
        success = yield from self.mesh(working_directory)
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

        if self._definition is not None:
            with open(os.path.join(working_directory, "start.py"), "w") as f:
                f.write(self._definition)
            magic_script = "start.py"
        else:
            definition_tar = os.path.join("input", "start.tar.gz")
            # Need to make sure this is last uploaded
            self._submitter.add_input(os.path.join(working_directory, definition_tar))
            if definition_tar in self._files_required:
                del self._files_required[definition_tar]
                print("Removing definition of tar from files required")
            magic_script = None
            print("Using package instead of magic script")

        loop = asyncio.get_event_loop()
        success = yield from self._submitter.run_script(
            loop,
            working_directory,
            self._docker_image,
            self._files_required.keys(),
            magic_script
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
        self.load_core_definition(xml, parameters, algorithms)
