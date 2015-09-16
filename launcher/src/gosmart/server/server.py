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

import os
import socket
import sys
import multiprocessing
import tempfile
import time
import traceback
import gosmart.server.family as families

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
from .error import Error, makeError


class GoSmartSimulationComponent(ApplicationSession):
    current = None
    client = None
    _db = None

    def __init__(self, x, server_id, database):
        global use_observant

        ApplicationSession.__init__(self, x)
        self.traceback_app = True

        self.server_id = server_id
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

        if not os.path.exists(server_id):
            print("Creating server ID directory")
            os.mkdir(server_id)

        os.chdir(server_id)

        with open("identity", "w") as f:
            f.write(server_id)

        loop.call_soon_threadsafe(lambda: self.setDatabase(database()))

    def setDatabase(self, database):
        self._db = database
        self._db.markAllOld()

    def doInit(self, guid):
        return True

    def doClean(self, guid):
        if guid not in self.current:
            return False

        current = self.current[guid]

        result = yield from current.clean()

        return result

    def doStart(self, guid):
        if guid not in self.current:
            return False

        loop = asyncio.get_event_loop()
        coro = self.doSimulate(guid)
        try:
            task = loop.create_task(coro)
        except AttributeError:
            task = asyncio.async(coro, loop=loop)

        task.add_done_callback(lambda f: asyncio.async(self._handle_simulation_done(f, guid=guid)))

        return True

    def doTmpValidation(self, guid, directory):
        # RMV: This is hacky
        loop = asyncio.get_event_loop()
        coro = families.register["elmer-libnuma"].validation(None, directory)
        try:
            task = loop.create_task(coro)
        except AttributeError:
            task = asyncio.async(coro, loop=loop)

        task.add_done_callback(lambda f: loop.call_soon_threadsafe(lambda: self._db.updateValidation(guid, f.result())))

        return True

    @asyncio.coroutine
    def _handle_simulation_done(self, fut, guid):
        success = fut.result()
        print("EXITED")

        current = self.current[guid]

        if success:
            yield from self.eventComplete(guid)
        else:
            code = Error.E_UNKNOWN
            error_message = "Unknown error occurred"
            error_message_path = os.path.join(current.get_dir(), 'error_message')

            if (os.path.exists(error_message_path)):
                with open(error_message_path, 'r') as f:
                    code = f.readline().strip()
                    error_message = f.read().strip()
                    error_message.encode('ascii', 'xmlcharrefreplace')
                    error_message.encode('utf-8')

            print("Failed simulation in %s" % current.get_dir())
            yield from self.eventFail(guid, makeError(code, error_message))

        print("Finished simulation")

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
        print(files, self.current, guid)
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
            os.chmod(tmpdir, 0o770)
            print("Changed permissions")
            translator = GoSmartSimulationTranslator()
            self.current[guid] = GoSmartSimulationDefinition(
                guid,
                xml,
                tmpdir,
                translator,
                finalized=False,
                update_status_callback=lambda p, m: self.updateStatus(guid, p, m)
            )
            self.publish(u'com.gosmartsimulation.announce', self.server_id, guid, [0, 'XML uploaded'], tmpdir, time.time())
        except Exception as e:
            traceback.print_exc(file=sys.stderr)
            raise e

        print("XML set")

        return True

    @asyncio.coroutine
    def doSimulate(self, guid):
        if guid not in self.current:
            yield from self.eventFail(guid, makeError(Error.E_CLIENT, "Not fully prepared before launching - no current simulation set"))

        current = self.current[guid]

        print("Running simulation in %s" % current.get_dir(), file=sys.stderr)

        self.updateStatus(guid, 0, "Launched subprocess")
        yield from current.init_percentage_socket_server()

        success = yield from current.simulate()

        return success

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

    @asyncio.coroutine
    def eventComplete(self, guid):
        print("complete", guid)
        if guid not in self.current:
            print("Tried to send simulation-specific completion event with no current simulation definition", file=sys.stderr)

        timestamp = time.time()

        print(timestamp)
        try:
            loop = asyncio.get_event_loop()
            loop.call_soon_threadsafe(lambda: self._db.setStatus(guid, "SUCCESS", "Success", "100", timestamp))
            validation = yield from self.current[guid].validation()
            if validation:
                loop.call_soon_threadsafe(lambda: self._db.updateValidation(guid, validation))
        except Exception as e:
            print(e)
            validation = None
            traceback.print_exc(file=sys.stderr)

        self.current[guid].set_exit_status(True)
        print('Success', guid)

        self.publish(u'com.gosmartsimulation.complete', guid, makeError('SUCCESS', 'Success'), self.current[guid].get_dir(), timestamp, validation)

    @asyncio.coroutine
    def eventFail(self, guid, message):
        if guid not in self.current:
            print("Tried to send simulation-specific failure event with no current simulation definition", file=sys.stderr)

        timestamp = time.time()

        try:
            loop = asyncio.get_event_loop()
            loop.call_soon_threadsafe(lambda: self._db.setStatus(guid, message["code"], message["message"], None, timestamp))
        except Exception as e:
            print(e)
            traceback.print_exc(file=sys.stderr)

        self.current[guid].set_exit_status(False, message)
        print('Failure', guid, message)

        self.publish(u'com.gosmartsimulation.fail', guid, message, self.current[guid].get_dir(), timestamp, None)

    def doRetrieveStatus(self, guid):
        simulation = self._db.retrieve(guid)
        exit_code = simulation['exit_code']

        if exit_code is None:
            if simulation['guid'] in self.current:
                exit_code = 'IN_PROGRESS'
            else:
                exit_code = 'E_UNKNOWN'

        status = makeError(exit_code, simulation['status'])
        percentage = simulation['percentage']

        return {
            "server_id": self.server_id,
            "simulation_id": simulation['guid'],
            "status": (percentage, status),
            "directory": simulation['directory'],
            "timestamp": simulation['timestamp'],
            "validation": simulation['validation']
        }

    def onRequestAnnounce(self):
        try:
            simulations = self._db.all()
            for simulation in simulations:
                exit_code = simulation['exit_code']

                if exit_code is None:
                    if simulation['guid'] in self.current:
                        exit_code = 'IN_PROGRESS'
                    else:
                        exit_code = 'E_UNKNOWN'

                status = makeError(exit_code, simulation['status'])
                percentage = simulation['percentage']

                self.publish(u'com.gosmartsimulation.announce', self.server_id, simulation['guid'], (percentage, status), simulation['directory'], simulation['timestamp'], simulation['validation'])
                print("Announced: %s %s %r" % (simulation['guid'], simulation['directory'], simulation['validation'] is not None))

        except Exception:
            for simulation in self.current:
                exit_status = self.current[simulation].get_exit_status()
                properties = self.getProperties(simulation)
                self.publish(
                    u'com.gosmartsimulation.announce',
                    self.server_id,
                    simulation,
                    (100 if exit_status[0] else 0, makeError('SUCCESS' if exit_status[0] else 'E_UNKNOWN', exit_status[1])),
                    properties['location'],
                    time.time()
                )
                print("Announced (from map): %s" % simulation)

        self.onRequestIdentify()

    def updateStatus(self, id, percentage, message):
        timestamp = time.time()

        progress = "%.2lf" % percentage if percentage else '##'
        print("%s [%r] ---- %s%%: %s" % (id, timestamp, progress, message))

        try:
            loop = asyncio.get_event_loop()
            loop.call_soon_threadsafe(lambda: self._db.setStatus(id, 'IN_PROGRESS', message, percentage, timestamp))
        except Exception as e:
            print(e)
            traceback.print_exc(file=sys.stderr)

        self.publish('com.gosmartsimulation.status', id, (percentage, makeError('IN_PROGRESS', message)), timestamp, None)

    def onRequestIdentify(self):
        try:
            active_simulations = self._db.active_count()
            score = multiprocessing.cpu_count() - active_simulations
            server_name = socket.gethostname()

            self.publish(
                u'com.gosmartsimulation.identify',
                self.server_id,
                server_name,
                score
            )
            print("Announced score: %d [%s]" % (score, self.server_id))
        except Exception as e:
            print("Didn't send score!")
            raise e

    def onJoin(self, details):
        print("session ready")

        try:
            for i in ('.' + self.server_id, ''):
                self.register(self.doInit, u'com.gosmartsimulation%s.init' % i)
                self.register(self.doStart, u'com.gosmartsimulation%s.start' % i)
                self.register(self.doUpdateSettingsXml, u'com.gosmartsimulation%s.update_settings_xml' % i)
                self.register(self.doUpdateFiles, u'com.gosmartsimulation%s.update_files' % i)
                self.register(self.doRequestFiles, u'com.gosmartsimulation%s.request_files' % i)
                self.register(self.doTmpValidation, u'com.gosmartsimulation%s.tmp_validation' % i)
                self.register(self.doFinalize, u'com.gosmartsimulation%s.finalize' % i)
                self.register(self.doClean, u'com.gosmartsimulation%s.clean' % i)
                self.register(self.doCompare, u'com.gosmartsimulation%s.compare' % i)
                self.register(self.doWorkflow, u'com.gosmartsimulation%s.workflow' % i)
                self.register(self.doProperties, u'com.gosmartsimulation%s.properties' % i)
                self.register(self.doRetrieveStatus, u'com.gosmartsimulation%s.retrieve_status' % i)

                self.subscribe(self.onRequestAnnounce, u'com.gosmartsimulation%s.request_announce' % i)
                self.subscribe(self.onRequestIdentify, u'com.gosmartsimulation%s.request_identify' % i)
            print("procedure registered")
        except Exception as e:
            print("could not register procedure: {0}".format(e))
