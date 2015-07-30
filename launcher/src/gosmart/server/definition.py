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
import shutil
import sys
import traceback

# Replace with better integrated approach!
import asyncio
from gosmart.server.transferrer import transferrer_register
from zope.interface.verify import verifyObject
from gosmart.server.transferrer import ITransferrer
import gosmart.server.family as families

from lxml import etree as ET


class GoSmartSimulationDefinition:
    _guid = None
    _dir = None
    _remote_dir = ''
    _finalized = False
    _files = None
    _exit_status = None
    _model_builder = None

    def set_exit_status(self, success, message=None):
        self._exit_status = (success, message)

    def get_exit_status(self):
        return self._exit_status

    def __init__(self, guid, xml_string, tmpdir, translator, finalized=False, update_status_callback=None):
        self._guid = guid
        self._dir = tmpdir
        self._finalized = finalized
        self._files = {}
        self._translator = translator
        self._update_status_callback = update_status_callback

        try:
            self.create_xml_from_string(xml_string)
        except Exception as e:
            print(e)

        input_dir = os.path.join(tmpdir, 'input')
        if not os.path.exists(input_dir):
            try:
                os.mkdir(input_dir)
            except Exception:
                traceback.print_exc(file=sys.stderr)

        with open(os.path.join(tmpdir, "original.xml"), "w") as f:
            f.write(xml_string)

        with open(os.path.join(tmpdir, "guid"), "w") as f:
            f.write(guid)

    def get_remote_dir(self):
        return self._remote_dir

    def set_remote_dir(self, remote_dir):
        self._remote_dir = remote_dir

    def get_guid(self):
        return self._guid

    def create_xml_from_string(self, xml):
        self._finalized = False

        try:
            self._xml = ET.fromstring(bytes(xml, 'utf-8'))
        except Exception as e:
            traceback.print_exc(file=sys.stderr)
            raise e

        return True

    def update_files(self, files):
        self._files.update(files)

    def get_files(self):
        return self._files

    def finalize(self):
        print("Finalize - Translating Called")
        if self._xml is None:
            return False

        try:
            print("Instantiating transferrer")
            transferrer_node = self._xml.find('transferrer')
            cls = transferrer_node.get('class')
            self._transferrer = transferrer_register[cls]()
            verifyObject(ITransferrer, self._transferrer)
            self._transferrer.configure_from_xml(transferrer_node)

            print("Starting to Translate")
            family, numerical_model_node, parameters, algorithms = \
                self._translator.translate(self._xml)

            if family is None or family not in families.register:
                raise RuntimeError("Unknown family of models : %s" % family)

            files_required = self._translator.get_files_required()

            self._model_builder = families.register[family](files_required)
            self._model_builder.load_definition(numerical_model_node, parameters=parameters, algorithms=algorithms)

            self._files.update(files_required)
            self._transferrer.connect()
            self._transferrer.pull_files(self._files, self.get_dir(), self.get_remote_dir())
            self._transferrer.disconnect()
        except Exception:
            traceback.print_exc(file=sys.stderr)
            return False

        self._finalized = True
        return True

    def finalized(self):
        return self._finalized

    def get_dir(self):
        return self._dir

    def clean(self):
        shutil.rmtree(self._dir)

        return True

    def push_files(self, files):
        uploaded_files = {}

        for local, remote in files.items():
            path = os.path.join(self.get_dir(), local)
            if os.path.exists(path):
                uploaded_files[local] = remote
            else:
                print("Could not find %s for SFTP PUT" % path)

        self._transferrer.connect()
        self._transferrer.push_files(uploaded_files, self.get_dir(), self.get_remote_dir())
        self._transferrer.disconnect()

        return uploaded_files

    @asyncio.coroutine
    def simulate(self):
        task = yield from self._model_builder.simulate(self.get_dir())
        return task
