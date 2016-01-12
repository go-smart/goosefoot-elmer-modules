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
from . import family as families

# Try to hook into vigilant if present
try:
    import StatsCore
    from StatsCore.SimpleTransports import UDPStatsTransport, TCPStatsTransport
    from configparser import RawConfigParser as CParser
    use_observant = True
except:
    use_observant = False

from .definition import GoSmartSimulationDefinition
from .comparator import Comparator
from .translator import GoSmartSimulationTranslator
from .error import Error, makeError
from .config import etc_location


# This subclasses ApplicationSession, which runs inside an Autobahn WAMP session
class GoSmartSimulationServerComponent(ApplicationSession):
    current = None
    client = None
    _db = None

    def __init__(self, x, server_id, database, ignore_development=False):
        global use_observant

        ApplicationSession.__init__(self, x)

        # This forwards exceptions to the client
        self.traceback_app = True

        self.server_id = server_id
        self.current = {}
        # Flag that tells the server to ignore anything with a parameter
        # `DEVELOPMENT` true
        self._ignore_development = ignore_development

        # If we are using vigilant, do the relevant set-up
        if use_observant:
            config = CParser()
            config.read(os.path.join(etc_location, 'vigilant.cfg'))

            lock = str(config.get('daemon', 'lock'))
            sock = str(config.get('daemon', 'sock'))
            transport_type = str(config.get('transport', 'type'))
            host = str(config.get('transport', 'host'))
            port = int(config.get('transport', 'port'))
            transport_means = UDPStatsTransport if transport_type == 'udp' else TCPStatsTransport
            transport = transport_means(host=host, port=port)

            self.client = StatsCore.attachOrCreateStatsDaemon(transport, pid=lock, sock=sock)
            self.client.postWatchPid('go-smart-simulation-server', os.getpid())

        # Convert this to a zope interface
        loop = asyncio.get_event_loop()

        # Create a directory to hold information specific to this server ID
        if not os.path.exists(server_id):
            print("Creating server ID directory")
            os.mkdir(server_id)

        # Use this as the working directory
        os.chdir(server_id)

        # Provide a directory-internal way to find out our ID (i.e. without
        # looking at the name in the directory above)
        with open("identity", "w") as f:
            f.write(server_id)

        # Flag this up to be done, but don't wait for it
        loop.call_soon_threadsafe(lambda: self.setDatabase(database()))

    # For start-up, mark everything in-progress in the DB as not-in-progress/unfinished
    def setDatabase(self, database):
        self._db = database
        self._db.markAllOld()

    # com.gosmartsimulation.init - dummy call for the moment
    def doInit(self, guid):
        return True

    # com.gosmartsimulation.clean - remove anything in simulation working
    # directory, for instance
    def doClean(self, guid):
        if guid not in self.current:
            return False

        current = self.current[guid]

        result = yield from current.clean()

        return result

    # com.gosmartsimulation.start - execute the simulation in a coro
    def doStart(self, guid):
        if guid not in self.current:
            return False

        loop = asyncio.get_event_loop()
        coro = self.doSimulate(guid)
        try:
            task = loop.create_task(coro)
        except AttributeError:
            task = asyncio.async(coro, loop=loop)

        # Once the simulation has completed, we must handle it
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
        # This should be the return value of the simulate call
        success = fut.result()
        print("EXITED")

        current = self.current[guid]

        if success:
            yield from self.eventComplete(guid)
        elif success is None:
            # None indicates we've dealt with failure (errored) already
            pass
        else:
            # We know this did not succeed, but not why it failed
            code = Error.E_UNKNOWN
            error_message = "Unknown error occurred"

            # In theory, an error message should have been written here, in any
            # case
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

    # com.gosmartsimulation.update_files - add the passed files to the
    # simulation's reference dictionary of required input files (available to be
    # requested later)
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

    # com.gosmartsimulation.request_files - push the requested output files
    # through the transferrer and return the list that was sent
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

    # com.gosmartsimulation.compare - check whether two GSSA-XML files match
    # and, if not, what their differences are
    def doCompare(self, this_xml, that_xml):
        comparator = Comparator(this_xml, that_xml)
        return comparator.diff()

    # com.gosmartsimulation.update_settings_xml - set the GSSA-XML for a given
    # simulation
    def doUpdateSettingsXml(self, guid, xml):
        try:
            # Create a working directory for the simulation (this is needed even
            # if the tool runs elsewhere, as in the Docker case)
            tmpdir = tempfile.mkdtemp(prefix='gssf-')
            os.chmod(tmpdir, 0o770)
            print("Changed permissions")

            # Set up the translator to parse the standard bits of GSSA-XML
            translator = GoSmartSimulationTranslator()
            self.current[guid] = GoSmartSimulationDefinition(
                guid,
                xml,
                tmpdir,
                translator,
                finalized=False,
                ignore_development=self._ignore_development,
                update_status_callback=lambda p, m: self.updateStatus(guid, p, m)
            )

            # Announce that XML has been uploaded
            # TODO: why announce this? Surely the response is sufficient?
            self.publish(u'com.gosmartsimulation.announce', self.server_id, guid, [0, 'XML uploaded'], tmpdir, time.time())
        except Exception as e:
            traceback.print_exc(file=sys.stderr)
            raise e

        print("XML set")

        return True

    # Start the simulation. This occurs in a separately scheduled coro from the
    # RPC call so it will almost certainly have returned by time we do
    @asyncio.coroutine
    def doSimulate(self, guid):
        if guid not in self.current:
            yield from self.eventFail(guid, makeError(Error.E_CLIENT, "Not fully prepared before launching - no current simulation set"))
            success = None

        current = self.current[guid]

        print("Running simulation in %s" % current.get_dir(), file=sys.stderr)

        # Inform the user that we got this far
        self.updateStatus(guid, 0, "Starting simulation...")
        # Start the socket server before simulating
        yield from current.init_percentage_socket_server()

        # Start the simulation. If we get an error, then blame the server side -
        # it should have returned False
        # FIXME: so how exactly does any non E_SERVER message get sent back?
        try:
            success = yield from current.simulate()
        except Exception as e:
            yield from self.eventFail(guid, makeError(Error.E_SERVER, "[%s] %s" % (type(e), str(e))))
            success = None

        return success

    # com.gosmartsimulation.finalize - do any remaining preparation before the
    # simulation can start
    def doFinalize(self, guid, client_directory_prefix):
        print("Converting the Xml")
        if guid not in self.current:
            return False

        current = self.current[guid]
        current.set_remote_dir(client_directory_prefix)

        # Make sure the simulation is in the DB
        self._db.addOrUpdate(current)

        # Execute the finalization
        result = current.finalize()

        return result

    # com.gosmartsimulation.properties - return important server-side simulation
    # properties
    def doProperties(self, guid):
        return self.getProperties(guid)

    # Server-specific properties for this simulation (at present, just wd location)
    def getProperties(self, guid):
        if guid not in self.current:
            raise RuntimeError("Simulation not found: %s" % guid)

        return {"location": self.current[guid].get_dir()}

    # DEPRECATED: do whole workflow
    def doWorkflow(self, guid, xml, input_files, request_files):
        print("WorkflowStarted")
        self.doInit(self, guid)
        self.doUpdateSettingsXml(self, guid, xml)
        self.doUpdateFiles(self, guid, input_files)
        self.doRequestFiles(self, guid, request_files)
        self.doFinalize(self, guid, '')

    # Called when simulation completes - publishes a completion event
    @asyncio.coroutine
    def eventComplete(self, guid):
        print("complete", guid)
        if guid not in self.current:
            print("Tried to send simulation-specific completion event with no current simulation definition", file=sys.stderr)

        # Record the finishing time, as we see it
        timestamp = time.time()

        print(timestamp)
        try:
            # Tell the database we have finished
            loop = asyncio.get_event_loop()
            loop.call_soon_threadsafe(lambda: self.setStatus(guid, "SUCCESS", "Success", "100", timestamp))
            # Run validation (if req)
            validation = yield from self.current[guid].validation()
            if validation:
                loop.call_soon_threadsafe(lambda: self._db.updateValidation(guid, validation))
        except Exception as e:
            print(e)
            validation = None
            traceback.print_exc(file=sys.stderr)

        self.current[guid].set_exit_status(True)
        print('Success', guid)

        # Notify any subscribers
        self.publish(u'com.gosmartsimulation.complete', guid, makeError('SUCCESS', 'Success'), self.current[guid].get_dir(), timestamp, validation)

    # Called when simulation fails - publishes a failure event
    @asyncio.coroutine
    def eventFail(self, guid, message):
        if guid not in self.current:
            print("Tried to send simulation-specific failure event with no current simulation definition", file=sys.stderr)

        # Record the failure time as we see it
        timestamp = time.time()

        try:
            loop = asyncio.get_event_loop()
            # Update the database
            loop.call_soon_threadsafe(lambda: self.setStatus(guid, message["code"], message["message"], None, timestamp))
        except Exception as e:
            print(e)
            traceback.print_exc(file=sys.stderr)

        self.current[guid].set_exit_status(False, message)
        print('Failure', guid, message)

        # Notify any subscribers
        self.publish(u'com.gosmartsimulation.fail', guid, message, self.current[guid].get_dir(), timestamp, None)

    # com.gosmartsimulation.retrieve_status - get the latest status for a
    # simulation
    def doRetrieveStatus(self, guid):
        # Get this from the DB, not current, as the DB should give a consistent
        # answer even after restart (unless marked unfinished)
        simulation = self._db.retrieve(guid)
        exit_code = simulation['exit_code']

        if exit_code is None:
            if simulation['guid'] in self.current:
                exit_code = 'IN_PROGRESS'
            else:
                exit_code = 'E_UNKNOWN'

        # NB: makeError can return SUCCESS or IN_PROGRESS
        status = makeError(exit_code, simulation['status'])
        percentage = simulation['percentage']

        # This format matches the fail/status/complete events
        return {
            "server_id": self.server_id,
            "simulation_id": simulation['guid'],
            "status": (percentage, status),
            "directory": simulation['directory'],
            "timestamp": simulation['timestamp'],
            "validation": simulation['validation']
        }

    # com.gosmartsimulation.request_announce - release a status report on each
    # simulation in the database
    # TODO: this gets unweildy, perhaps it should have an earliest simulation
    # timestamp argument?
    def onRequestAnnounce(self):
        try:
            # Go through /every/ simulation
            simulations = self._db.all()
            for simulation in simulations:
                exit_code = simulation['exit_code']

                # If it hasn't exited, it should be running...
                if exit_code is None:
                    if simulation['guid'] in self.current:
                        exit_code = 'IN_PROGRESS'
                    else:
                        exit_code = 'E_UNKNOWN'

                status = makeError(exit_code, simulation['status'])
                percentage = simulation['percentage']

                # Tell the world
                self.publish(u'com.gosmartsimulation.announce', self.server_id, simulation['guid'], (percentage, status), simulation['directory'], simulation['timestamp'], simulation['validation'])
                print("Announced: %s %s %r" % (simulation['guid'], simulation['directory'], simulation['validation'] is not None))

        except Exception:
            # If we can't get it from the database, fall back to the
            # self.current map
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

        # Follow up with an identify event
        self.onRequestIdentify()

    # Record a status change in the database and on the filesystem. Note that,
    # for both those reasons, this could be slow and so should always be run
    # with call_soon_threadsafe
    # FIXME: we need some sort of rate limiting here, or producer-consumer
    # pattern with ability to skip once getting behind
    def setStatus(self, id, key, message, percentage, timestamp):
        # Write this message to the database
        self._db.setStatus(id, key, message, percentage, timestamp)

        # Write the last message in a format that the status can be easily
        # re-read
        with open(os.path.join(self.current[id].get_dir(), 'last_message'), 'w') as f:
            f.write("%s\n" % id)
            f.write("%s\n" % key.strip())
            if percentage:
                f.write("%lf\n" % float(percentage))
            else:
                f.write("\n")
            if message:
                f.write(message.strip())

    # Update the status, setting up a callback for asyncio
    def updateStatus(self, id, percentage, message):
        timestamp = time.time()

        # Write out to the command line for debug
        # TODO: switch to `logging` and `vigilant`
        progress = "%.2lf" % percentage if percentage else '##'
        print("%s [%r] ---- %s%%: %s" % (id, timestamp, progress, message))

        try:
            # Call the setStatus method asynchronously
            loop = asyncio.get_event_loop()
            loop.call_soon_threadsafe(lambda: self.setStatus(id, 'IN_PROGRESS', message, percentage, timestamp))
        except Exception as e:
            print(e)
            traceback.print_exc(file=sys.stderr)

        directory = None
        if id in self.current:
            directory = self.current[id].get_dir()

        # Publish a status update for the WAMP clients to see
        self.publish('com.gosmartsimulation.status', id, (percentage, makeError('IN_PROGRESS', message)), directory, timestamp, None)

    # com.gosmartsimulation.request_identify - publish basic server information
    def onRequestIdentify(self):
        # score = #cores - #active_simulations
        # this gives an availability estimate, the higher the better
        # FIXME: this seems to give a wrong number consistently, needs checked
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

    # Fired when we first join the router - this gives us a chance to register
    # everything
    def onJoin(self, details):
        print("session ready")

        # Register an us-specific set of RPC calls. Also attempts to do the same
        # for the generic set, if we haven't been beaten to the punch
        try:
            for i in ('.' + self.server_id, ''):
                self.subscribe(self.onRequestAnnounce, u'com.gosmartsimulation%s.request_announce' % i)
                self.subscribe(self.onRequestIdentify, u'com.gosmartsimulation%s.request_identify' % i)

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
            print("procedure registered")
        except Exception as e:
            print("could not register procedure: {0}".format(e))
