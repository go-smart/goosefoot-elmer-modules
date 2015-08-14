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
from autobahn.asyncio.wamp import ApplicationSession
import uuid
from lxml import etree as ET
import asyncio
import os


# This should be adjusted when this issue resolution hits PIP: https://github.com/tavendo/AutobahnPython/issues/332
# http://stackoverflow.com/questions/28293198/calling-a-remote-procedure-from-a-subscriber-and-resolving-the-asyncio-promise
from functools import wraps
def wrapped_coroutine(f):
    def wrapper(*args, **kwargs):
        coro = f(*args, **kwargs)
        asyncio.async(coro)
    return wrapper
#endSO


class GoSmartSimulationClientComponent(ApplicationSession):

    def __init__(self, x, gssa_file, subdirectory, output_files, definition_file=None, skip_clean=False):
        ApplicationSession.__init__(self, x)
        self._gssa = ET.parse(gssa_file)
        if definition_file is not None:
            definition_node = self._gssa.find('.//definition')
            with open(definition_file, 'r') as f:
                definition_node.text = f.read()
        self._guid = uuid.uuid1()
        self._subdirectory = subdirectory
        self._output_files = output_files
        self._skip_clean = skip_clean
        print(self._skip_clean)

    @asyncio.coroutine
    def onJoin(self, details):
        print("session ready")

        guid = str(self._guid)
        gssa = ET.tostring(self._gssa, encoding="unicode")
        yield from self.call('com.gosmartsimulation.init', guid)
        yield from self.call('com.gosmartsimulation.update_settings_xml', guid, gssa)
        yield from self.call('com.gosmartsimulation.finalize', guid, self._subdirectory)
        yield from self.call('com.gosmartsimulation.start', guid)
        self.subscribe(self.onComplete, 'com.gosmartsimulation.complete')
        self.subscribe(self.onFail, 'com.gosmartsimulation.fail')

    @wrapped_coroutine
    @asyncio.coroutine
    def onComplete(self, guid, success, time):
        print("Complete - requesting files")
        files = yield from self.call('com.gosmartsimulation.request_files', guid, {
            f: os.path.join('/tmp', f) for f in self._output_files
        })
        print(files)
        yield from self.finalize(guid)

    @wrapped_coroutine
    @asyncio.coroutine
    def onFail(self, guid, message, time):
        print("Failed - %s" % message)
        yield from self.finalize(guid)

    def finalize(self, guid):
        if not self._skip_clean:
            yield from self.call('com.gosmartsimulation.clean', guid)
            self.disconnect()
        else:
            print("Skipping clean-up")
