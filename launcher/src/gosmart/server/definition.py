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
import urllib.request

from lxml import etree as ET


class GoSmartSimulationDefinition:
    _guid = None
    _dir = None
    _remote_dir = ''
    _finalized = False
    _files = None

    def __init__(self, guid, xml_string, tmpdir, translator, finalized=False):
        self._guid = guid
        self._dir = tmpdir
        self._finalized = finalized
        self._files = {}
        self._translator = translator

        try:
            #database_xml_response = urllib.request.urlopen("http://gsmock.numa.oan/simulation/%s" % guid)
            #xml_string = database_xml_response.read().decode('UTF-8')
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
            self._xml = ET.fromstring(xml)
        except Exception:
            traceback.print_exc(file=sys.stderr)
            return False

        return True

    def update_files(self, files):
        self._files.update(files)

    def get_files(self):
        return self._files

    def finalize(self):
        if self._xml is None or self._pull_files_cb is None:
            return False

        try:
            translated_xml = self._translator.translate(self._xml)
            self._files.update(self._translator.get_files_required())
            tree = ET.ElementTree(translated_xml)

            self._pull_files_cb(self._files, self.get_dir(), self.get_remote_dir())

            with open(os.path.join(self._dir, "settings.xml"), "wb") as f:
                tree.write(f, pretty_print=True)
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

    def set_pull_files_cb(self, pull_files_cb):
        self._pull_files_cb = pull_files_cb
