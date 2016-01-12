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
import tarfile
import tempfile


# This should be adjusted when this issue resolution hits PIP: https://github.com/tavendo/AutobahnPython/issues/332
# http://stackoverflow.com/questions/28293198/calling-a-remote-procedure-from-a-subscriber-and-resolving-the-asyncio-promise
def wrapped_coroutine(f):
    def wrapper(*args, **kwargs):
        coro = f(*args, **kwargs)
        asyncio.async(coro)
    return wrapper
# endSO


# This is the application object for the shell GSSA client
class GoSmartSimulationClientComponent(ApplicationSession):

    # Accept arguments from the command line
    def __init__(self, x, gssa_file, subdirectory, output_files, input_files=None, definition_files=None, skip_clean=False, server=None):
        ApplicationSession.__init__(self, x)
        self._gssa = ET.parse(gssa_file)
        self._definition_files = definition_files
        self._input_files = input_files
        self._server = server

        # We tar the definition files into one object for transferring and add
        # it to the definition node
        if self._definition_files is not None:
            self._definition_tmp = tempfile.NamedTemporaryFile(suffix='.tar.gz')
            definition_tar = tarfile.open(fileobj=self._definition_tmp, mode='w:gz')
            for definition_file in self._definition_files:
                definition_tar.add(definition_file, os.path.basename(definition_file))
                print("Added [%s]" % os.path.basename(definition_file))
            definition_tar.close()
            self._definition_tmp.flush()
            print("Made temporary tar at %s" % self._definition_tmp.name)
            definition_node = self._gssa.find('.//definition')
            definition_node.set('location', self._definition_tmp.name)

        # Do the same with the input surfaces
        if self._input_files is not None:
            self._input_tmp = tempfile.NamedTemporaryFile(suffix='.tar.gz')
            input_tar = tarfile.open(fileobj=self._input_tmp, mode='w:gz')
            for input_file in self._input_files:
                input_tar.add(input_file, os.path.basename(input_file))
                print("Added [%s]" % os.path.basename(input_file))
            input_tar.close()
            self._input_tmp.flush()
            print("Made temporary tar at %s" % self._input_tmp.name)
            input_node = ET.SubElement(self._gssa.find('.//transferrer'), 'input')
            input_node.set('location', self._input_tmp.name)

        # Generate a simulation ID
        self._guid = uuid.uuid1()
        self._subdirectory = subdirectory
        self._output_files = output_files
        self._skip_clean = skip_clean

    def make_call(self, suffix):
        # If we have a specific server, address it, otherwise we call whichever
        # one got the full namespace
        if self._server:
            return "com.gosmartsimulation.%s.%s" % (self._server, suffix)
        else:
            return "com.gosmartsimulation.%s" % suffix

    @asyncio.coroutine
    def onJoin(self, details):
        print("session ready")

        # Run the simulation
        guid = str(self._guid)
        gssa = ET.tostring(self._gssa, encoding="unicode")
        yield from self.call(self.make_call('init'), guid)
        print("Initiated...")
        yield from self.call(self.make_call('update_settings_xml'), guid, gssa)
        print("Sent XML...")
        yield from self.call(self.make_call('finalize'), guid, self._subdirectory)
        print("Finalized settings...")
        yield from self.call(self.make_call('start'), guid)
        print("Started...")

        # Listen for responses from the server
        self.subscribe(self.onComplete, self.make_call('complete'))
        self.subscribe(self.onFail, self.make_call('fail'))

    @wrapped_coroutine
    @asyncio.coroutine
    def onComplete(self, guid, success, directory, time, validation):
        print("Complete")

        # Once we have completed, print the validation if available
        if validation:
            print("Validation:", validation)
        print("Requesting files")

        # Request files from the tmp transferrer
        files = yield from self.call(self.make_call('request_files'), guid, {
            f: os.path.join('/tmp', f) for f in self._output_files
        })
        print(files)
        yield from self.finalize(guid)

    # NB: this is not currently hooked in (see
    # self.subscribe(self.onComplete...)
    # for example)
    @wrapped_coroutine
    @asyncio.coroutine
    def onStatus(self, guid, message, directory, time, validation):
        percentage, state = message
        # Print each status message to the command line
        progress = "%.2lf" % percentage if percentage else '##'
        print("%s [%r] ---- %s%%: %s" % (id, time, progress, state['message']))

    @wrapped_coroutine
    @asyncio.coroutine
    def onFail(self, guid, message, directory, time, validation):
        print("Failed - %s" % message)
        yield from self.finalize(guid)

    # Tidy up, if needs be
    def finalize(self, guid):
        if not self._skip_clean:
            yield from self.call(self.make_call('clean'), guid)
            self.shutdown()
        else:
            print("Skipping clean-up")

    def shutdown(self):
        self.leave()

    @wrapped_coroutine
    @asyncio.coroutine
    def onLeave(self, details):
        self.disconnect()
