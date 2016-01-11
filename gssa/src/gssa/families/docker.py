import asyncio
import os
import yaml
import io
import tarfile

from gosmart.server.family import Family
from gosmart.server.docker import Submitter
from gosmart.server.parameters import convert_parameter


class DockerFamily(Family):
    _retrievable_files = ['logs/job.err', 'logs/job.out']

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
    def prepare_simulation(self, working_directory):
        return True

    def get_percentage_socket_location(self, working_directory):
        return os.path.join(working_directory, 'update.sock')

    @asyncio.coroutine
    def simulate(self, working_directory):
        proceed = yield from self.prepare_simulation(working_directory)

        update_socket = self.get_percentage_socket_location(working_directory)
        os.chmod(update_socket, 0o777)
        self._submitter.set_update_socket(update_socket)

        regions_yaml = os.path.join(working_directory, "input", "regions.yml")
        regions = self._regions
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

        if not proceed:
            return False

        definition_tar = os.path.join("input", "start.tar.gz")
        self._submitter.add_input(os.path.join(working_directory, definition_tar))
        magic_script = None

        if self._definition is not None:
            declared_parameters, python_script = self._definition.split("\n==========ENDPARAMETERS========\n")
            tar = tarfile.open(os.path.join(working_directory, definition_tar), "w:gz")

            for name, content in (('start.py', python_script), ('parameters.yml', declared_parameters)):
                encoded_definition = content.encode('utf-8')
                stringio = io.BytesIO(encoded_definition)
                info = tarfile.TarInfo(name=name)
                info.size = len(encoded_definition)
                tar.addfile(tarinfo=info, fileobj=stringio)

            tar.close()

        # Need to make sure this is last uploaded
        if definition_tar in self._files_required:
            del self._files_required[definition_tar]
            print("Removing definition of tar from files required")

        loop = asyncio.get_event_loop()
        success = yield from self._submitter.run_script(
            loop,
            working_directory,
            self._docker_image,
            self._files_required.keys(),
            magic_script
        )
        print("DONE")

        return success

    @asyncio.coroutine
    def clean(self):
        yield from self._submitter.destroy()
        self._submitter.finalize()

    def load_definition(self, xml, parameters, algorithms):
        self.load_core_definition(xml, parameters, algorithms)

    def retrieve_files(self, destination):
        for f in self._retrievable_files:
            print(f, '->', destination)
            print(self._submitter.copy_output(f, destination))
