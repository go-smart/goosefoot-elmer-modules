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
# This is a workaround for syntastic lack of Py3 recognition
from __future__ import print_function

from autobahn.asyncio.wamp import ApplicationSession
from zope.interface.verify import verifyObject

import os
import sys
import tempfile
import threading
import traceback

try:
    import StatsCore
    from StatsCore.SimpleTransports import UDPStatsTransport, TCPStatsTransport
    from configparser import RawConfigParser as CParser
    use_observant = True
except:
    use_observant = False

from gosmart.server.definition import GoSmartSimulationDefinition
from gosmart.server.transferrer import ITransferrer
from gosmart.server.translator import GoSmartSimulationTranslator
from gosmart.launcher import gosmart


class GoSmartArguments:
    def __init__(self, elmer_binary=None, outfilename=None, addpid=False, silent=True,
                 debug=False, nprocs=None, baw=True, only=None, leavetree=False, configfilenames=[]):
        self.elmer_binary = elmer_binary
        self.outfilename = outfilename
        self.addpid = addpid
        self.silent = silent
        self.debug = debug
        self.nprocs = nprocs
        self.baw = baw
        self.only = only
        self.leavetree = leavetree
        self.configfilenames = configfilenames


class GoSmartSimulationComponent(ApplicationSession):
    current = None
    client = None
    _db = None

    def __init__(self, x, transferrer, database):
        global use_observant

        ApplicationSession.__init__(self, x)
        verifyObject(ITransferrer, transferrer)

        self._transferrer = transferrer
        self._args = GoSmartArguments(configfilenames=["settings.xml"])
        self.current = {}

        if use_observant:
            config = CParser()
            config.read('/home/pweir/Code/observant/etc/observant/observant.cfg')

            lock = str(config.get('daemon', 'lock'))
            sock = str(config.get('daemon', 'sock'))
            transport_type = str(config.get('transport', 'type'))
            host = str(config.get('transport', 'host'))
            port = int(config.get('transport', 'port'))
            transport_means = UDPStatsTransport if transport_type == 'udp' else TCPStatsTransport
            transport = transport_means(host=host, port=port)

            self.client = StatsCore.attachOrCreateStatsDaemon(transport, pid=lock, sock=sock)
            self.client.postWatchPid('go-smart-launcher', os.getpid())

        # Convert this to a zope interface
        self._db = database

    def doInit(self, guid):
        pass

    def doClean(self, guid):
        if guid not in self.current:
            return False

        current = self.current[guid]

        return current.clean()

    def doStart(self, guid):
        if guid not in self.current:
            return False

        t = threading.Thread(target=lambda: self.doSimulate(guid))
        t.start()
        return True

    def doUpdateFiles(self, guid, files):
        if guid not in self.current or not isinstance(files, dict):
            return False

        current = self.current[guid]
        current.update_files(files)

        return True

    def doRequestFiles(self, guid, files):
        if guid not in self.current or not isinstance(files, dict):
            return {}

        current = self.current[guid]

        uploaded_files = {}

        for local, remote in files.items():
            path = os.path.join(current.get_dir(), local)
            if os.path.exists(path):
                uploaded_files[local] = remote
            else:
                print("Could not find %s for SFTP PUT" % path)

        try:
            self._transferrer.connect()
            self._transferrer.push_files(uploaded_files, current.get_dir(), current.get_remote_dir())
            self._transferrer.disconnect()
        except Exception:
            traceback.print_exc(file=sys.stderr)
            return {}

        return uploaded_files

    def doUpdateSettingsXml(self, guid, xml):
        try:
            tmpdir = tempfile.mkdtemp(prefix='gssf-')
            translator = GoSmartSimulationTranslator()
            self.current[guid] = GoSmartSimulationDefinition(guid, xml, tmpdir, translator)
        except Exception:
            traceback.print_exc(file=sys.stderr)
            return False

        return True

    def doSimulate(self, guid):
        if guid not in self.current:
            self.eventFail("Not fully prepared before launching - no current simulation set")

        current = self.current[guid]

        print("Running simulation in %s" % current.get_dir(), file=sys.stderr)

        try:
            wrapper = gosmart.GoSmart(args=self._args, global_working_directory=current.get_dir(), observant=self.client)

            wrapper.print_header()

            wrapper.launch(default_procs=1)
        except Exception as e:
            traceback.print_exc(file=sys.stderr)
            self.eventFail(guid, str(e))
        else:
            self.eventComplete(guid)

    def doFinalize(self, guid, client_directory_prefix):
        if guid not in self.current:
            return False

        current = self.current[guid]
        current.set_remote_dir(client_directory_prefix)
        current.set_pull_files_cb(self._transferrer.pull_files)

        self._db.addOrUpdate(current)

        self._transferrer.connect()
        result = current.finalize()
        self._transferrer.disconnect()

        return result

    def doWorkflow(self, guid, xml, input_files, request_files):
        self.doInit(self, guid)
        self.doUpdateSettingsXml(self, guid, xml)
        self.doUpdateFiles(self, guid, input_files)
        self.doRequestFiles(self, guid, request_files)
        self.doFinalize(self, guid, '')

    def eventComplete(self, guid):
        if guid not in self.current:
            print("Tried to send simulation-specific completion event with no current simulation definition", file=sys.stderr)

        self.publish(u'com.gosmartsimulation.complete', guid)

    def eventFail(self, guid, message):
        if guid not in self.current:
            print("Tried to send simulation-specific failure event with no current simulation definition", file=sys.stderr)

        self.publish(u'com.gosmartsimulation.fail', guid, message)

    def onJoin(self, details):
        print("session ready")

        try:
            self.register(self.doInit, u'com.gosmartsimulation.init')
            self.register(self.doStart, u'com.gosmartsimulation.start')
            self.register(self.doUpdateSettingsXml, u'com.gosmartsimulation.update_settings_xml')
            self.register(self.doUpdateFiles, u'com.gosmartsimulation.update_files')
            self.register(self.doRequestFiles, u'com.gosmartsimulation.request_files')
            self.register(self.doFinalize, u'com.gosmartsimulation.finalize')
            self.register(self.doClean, u'com.gosmartsimulation.clean')
            self.register(self.doWorkflow, u'com.gosmartsimulation.workflow')
            print("procedure registered")
        except Exception as e:
            print("could not register procedure: {0}".format(e))
