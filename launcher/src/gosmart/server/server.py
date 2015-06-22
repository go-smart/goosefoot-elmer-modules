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

import asyncio
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
from gosmart.comparator import Comparator
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

    def __init__(self, x, database):
        global use_observant

        ApplicationSession.__init__(self, x)
        self.traceback_app = True

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
        loop = asyncio.get_event_loop()
        loop.call_soon_threadsafe(lambda: self.setDatabase(database()))

    def setDatabase(self, database):
        self._db = database
        self._db.markAllOld()

    def doInit(self, guid):
        print("Test")
        pass

    def doClean(self, guid):
        if guid not in self.current:
            return False

        current = self.current[guid]

        return current.clean()

    def doStart(self, guid):
        if guid not in self.current:
            return False

        loop = asyncio.get_event_loop()
        print(loop.run_in_executor(None, lambda: self.doSimulate(guid, loop)))
        #t = threading.Thread(target=lambda: self.doSimulate(guid))
        #t.start()
        return True

    def doUpdateFiles(self, guid, files):
        if guid not in self.current or not isinstance(files, dict):
            return False
        print("Update Files")
        for local, remote in files.items():
            print("remote" + remote)
            print("Local" + local)
        current = self.current[guid]
        current.update_files(files)

        return True

    def doRequestFiles(self, guid, files):
        if guid not in self.current or not isinstance(files, dict):
            return {}

        current = self.current[guid]

        try:
            uploaded_files = current.push_files(files)
        except Exception:
            traceback.print_exc(file=sys.stderr)
            return {}

        return uploaded_files

    def doCompare(self, this_xml, that_xml):
        comparator = Comparator(this_xml, that_xml)
        return comparator.diff()

    def doUpdateSettingsXml(self, guid, xml):
        try:
            tmpdir = tempfile.mkdtemp(prefix='gssf-')
            translator = GoSmartSimulationTranslator()
            self.current[guid] = GoSmartSimulationDefinition(guid, xml, tmpdir, translator, lambda p, m: self.updateStatus(guid, p, m))
            self.publish(u'com.gosmartsimulation.announce', guid, [0, 'XML uploaded'], tmpdir)
        except Exception as e:
            traceback.print_exc(file=sys.stderr)
            raise e

        print("XML set")

        return True

    def doSimulate(self, guid, loop):
        if guid not in self.current:
            self.eventFail("Not fully prepared before launching - no current simulation set")

        current = self.current[guid]

        print("Running simulation in %s" % current.get_dir(), file=sys.stderr)

        try:
            wrapper = gosmart.GoSmart(
                args=self._args,
                global_working_directory=current.get_dir(),
                observant=self.client,
                update_status_callback=lambda p, m: self.updateStatus(guid, p, m, loop)
            )

            wrapper.print_header()

            wrapper.launch(default_procs=1)
        except Exception as e:
            traceback.print_exc(file=sys.stderr)
            self.eventFail(guid, str(e), loop)
        except Exception:
            pass
        else:
            self.eventComplete(guid, loop)

        print("Finished simulation")

    def doFinalize(self, guid, client_directory_prefix):
        print("Converting the Xml")
        if guid not in self.current:
            return False

        current = self.current[guid]
        current.set_remote_dir(client_directory_prefix)

        self._db.addOrUpdate(current)

        result = current.finalize()

        return result

    def doProperties(self, guid):
        return self.getProperties(guid)

    def getProperties(self, guid):
        if guid not in self.current:
            raise RuntimeError("Simulation not found: %s" % guid)

        return {"location": self.current[guid].get_dir()}

    def doWorkflow(self, guid, xml, input_files, request_files):
        print("WorkflowStarted")
        self.doInit(self, guid)
        self.doUpdateSettingsXml(self, guid, xml)
        self.doUpdateFiles(self, guid, input_files)
        self.doRequestFiles(self, guid, request_files)
        self.doFinalize(self, guid, '')

    def eventComplete(self, guid, loop):
        if guid not in self.current:
            print("Tried to send simulation-specific completion event with no current simulation definition", file=sys.stderr)

        try:
            loop.call_soon_threadsafe(lambda: self._db.setStatus(guid, "Success", "100"))
        except Exception as e:
            print(e)
            traceback.print_exc(file=sys.stderr)

        self.current[guid].set_exit_status(True)
        print('Success', guid)

        self.publish(u'com.gosmartsimulation.complete', guid)

    def eventFail(self, guid, message, loop):
        if guid not in self.current:
            print("Tried to send simulation-specific failure event with no current simulation definition", file=sys.stderr)

        try:
            loop.call_soon_threadsafe(lambda: self._db.setStatus(guid, "Failed", None))
        except Exception as e:
            print(e)
            traceback.print_exc(file=sys.stderr)

        self.current[guid].set_exit_status(False, message)
        print('Failure', guid, message)

        self.publish(u'com.gosmartsimulation.fail', guid, message)

    def onRequestAnnounce(self):
        try:
            simulations = self._db.all()
            for simulation in simulations:
                self.publish(u'com.gosmartsimulation.announce', simulation['guid'], (simulation['percentage'], simulation['status']), simulation['directory'])
                print("Announced: %s" % simulation['guid'])

        except Exception:
            for simulation in self.current:
                exit_status = self.current[simulation].get_exit_status()
                properties = self.getProperties(simulation)
                self.publish(u'com.gosmartsimulation.announce', simulation, (100 if exit_status[0] else 0, exit_status[1]), properties['location'])
                print("Announced (from map): %s" % simulation)

    def updateStatus(self, id, percentage, message, loop):
        try:
            loop.call_soon_threadsafe(lambda: self._db.setStatus(id, message, percentage))
        except Exception as e:
            print(e)
            traceback.print_exc(file=sys.stderr)

        self.publish('com.gosmartsimulation.status', id, percentage, message)

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
            self.register(self.doCompare, u'com.gosmartsimulation.compare')
            self.register(self.doWorkflow, u'com.gosmartsimulation.workflow')
            self.register(self.doProperties, u'com.gosmartsimulation.properties')

            self.subscribe(self.onRequestAnnounce, u'com.gosmartsimulation.request_announce')
            print("procedure registered")
        except Exception as e:
            print("could not register procedure: {0}".format(e))
