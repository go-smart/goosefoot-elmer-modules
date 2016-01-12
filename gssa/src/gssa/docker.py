#!/usr/bin/python3

import asyncio
import shutil
import json
import os

from watchdog.observers import Observer
from watchdog.events import PatternMatchingEventHandler
from hachiko.hachiko import AIOEventHandler

socket_location = '/var/run/docker-launch/docker-launch.sock'


# Check for output in the Docker volume
class OutputHandler(AIOEventHandler, PatternMatchingEventHandler):
    patterns = ['*']
    ignore_patterns = ['.*']

    def __init__(self, notify_callback, loop=None, **kwargs):
        self._notify_callback = notify_callback

        AIOEventHandler.__init__(self, loop)
        PatternMatchingEventHandler.__init__(self, **kwargs)

    @asyncio.coroutine
    def on_created(self, event):
        self._notify_callback(event.src_path)


# Submit the request to run an instance to the dockerlaunch daemon, also
# handling some of the standard bits and pieces of interface
class Submitter:
    reader = None
    writer = None
    _socket_location = None

    def __init__(self):
        self._input_files = []
        self._output_files = []
        self._output_directory = None

    def __del__(self):
        # Tidy up before quitting
        self.finalize()

    def set_update_socket(self, socket_location):
        self._socket_location = socket_location

    # Copy output files from the Docker volume to the simulation 'working
    # directory'
    def copy_output(self, requested, target):
        if not self._output_directory:
            return None

        requested_location = os.path.join(self._output_directory, requested)
        target_location = os.path.join(target, requested)

        try:
            shutil.copyfile(requested_location, target_location)
        except Exception as e:
            print(e)
            return None

        return True

    # Make a note that this file should be transferred to the Docker volume for
    # the simulation
    def add_input(self, input_file):
        self._input_files.append(input_file)

    # Return the content of the file `requested`
    def output(self, requested):
        if not self._output_directory:
            return None

        requested_location = os.path.join(self._output_directory, requested)

        try:
            with open(requested_location, 'r') as f:
                return f.read()
        except:
            return None

    # Print a message to highlight a new output file from Docker
    def notify_output(self, filename):
        print('Output:', filename)
        if filename not in self._output_files:
            self._output_files.append(filename)

    # Send a command to the dockerlaunch daemon
    def send_command(self, writer, command, arguments):
        print('-->', command, arguments)
        writer.write(bytes("%s\n" % json.dumps({
            'command': command,
            'arguments': arguments
        }), 'UTF-8'))

    # Run the simulation
    @asyncio.coroutine
    def run_script(self, loop, working_directory, image, files_required=[], magic_script=None):
        success = True

        # Connect to the dockerlaunch daemon (we should be in the `dockerlaunch`
        # group for this to work)
        try:
            reader, writer = yield from asyncio.open_unix_connection(
                socket_location
            )
        except Exception as e:
            print("Could not open connection: %s" % str(e))
            raise e

        # Read and write objects for reaching the daemon
        self.reader, self.writer = reader, writer

        print("Simulating")
        try:
            # Tell the daemon to fire up an instance
            self.send_command(writer, 'START', {'image': image, 'update socket': self._socket_location})
            success, message = yield from self.receive_response(reader)
            print('<--', success, message)

            if not success:
                raise RuntimeError('Could not start: %s', message)

            try:
                # Set up our basic locations, for accessing the Docker volume
                if magic_script is not None:
                    magic_script = os.path.join(
                        message['volume location'],
                        message['input subdirectory'],
                        magic_script
                    )
                self._output_directory = os.path.join(
                    message['volume location'],
                    message['output subdirectory']
                )
                self._input_directory = os.path.join(
                    message['volume location'],
                    message['input subdirectory']
                )
            except KeyError as e:
                print(str(e))
                raise e

            # Start watching for output files of interest in the Docker volume
            event_handler = OutputHandler(self.notify_output, loop=loop)
            observer = Observer()
            observer.schedule(
                event_handler,
                self._output_directory,
                recursive=True
            )
            # FIXME: this causes occasional spontaneous segfaults during file updating... diagnosis pending
            # observer.start()

            # Go through each file required by the simulation and put it into
            # the Docker volume
            for f in files_required:
                to_location = os.path.join(self._input_directory, os.path.basename(f))
                from_location = os.path.join(working_directory, 'input', os.path.basename(f))
                if not f.startswith('.') and not os.path.isdir(to_location):
                    print("Transferring %s to %s for docker" % (from_location, to_location))

                    shutil.copyfile(
                        from_location,
                        to_location,
                    )

            for input_file in self._input_files:
                shutil.copyfile(input_file, os.path.join(self._input_directory, os.path.basename(input_file)))

            # Once those are copied, we can send the magic script, which tells
            # the instance to start processing. Note that, if the definition was
            # passed to the server in a TAR.GZ, this may already have been sent
            # above - however, this is None in that case.
            if magic_script is not None:
                with open(magic_script, 'w') as f, open(os.path.join(working_directory, 'start.py'), 'r') as g:
                    f.write(g.read())

                print("Wrote magic script to %s" % magic_script)

            # Wait for the simulation to finish
            self.send_command(writer, 'WAIT', None)
            success, message = yield from self.receive_response(reader)
            print('<--', success, message)

            if not success:
                raise RuntimeError('Could not wait: %s', message)

            # observer.stop()

            self.send_command(writer, 'LOGS', None)
            success, message = yield from self.receive_response(reader)
            print('<--', success, message.replace('\\n', '\n'))

            if not success:
                raise RuntimeError('Could not retrieve logs: %s', message)

            # Get the simulation exit status
            exit_status = self.output(os.path.join('logs', 'exit_status'))
            code, message = exit_status.split('\n', 1)

            # If we did not exit cleanly, inform the server
            if int(code) != 0:
                raise RuntimeError('Non-zero exit status: %d %s' % (int(code), message))
            print('<==>', code, message)

            # Copy the logs back to the simulation 'working directory'
            for output_file in ('docker_inner.log', 'job.out', 'job.err'):
                print('-' * 20)
                print(output_file.upper())

                output_log = self.output(os.path.join('logs', output_file))

                # Print out the logs
                if output_log:
                    print(output_log)
                else:
                    print("[no output from %s]" % output_file)
        except Exception as e:
            print(str(e))
            # Redundant?!
            success = False
            self.finalize()
            raise e

        return success

    @asyncio.coroutine
    def destroy(self):
        if not self.reader or not self.writer:
            raise RuntimeError('No reader/writer members to access launcher daemon')

        # Tell the Docker side to tidy up
        self.send_command(self.writer, 'DESTROY', None)
        success, message = yield from self.receive_response(self.reader)
        print('<--', success, message)

        if not success:
            raise RuntimeError('Could not destroy: %s', message)

    def finalize(self):
        if not self.reader or not self.writer:
            return

        writer = self.writer

        self.reader, self.writer = None, None

        writer.close()

    @asyncio.coroutine
    def receive_response(self, reader):
        while True:
            line = yield from reader.readline()

            if line:
                break

        # Turn the flat lines into a JSON pair: 'success' and 'message'
        line = line.decode('UTF-8').strip()

        try:
            message = json.loads(line)
        except ValueError as e:
            # Insert logging
            raise e

        try:
            success = message['success']
            message = message['message']
        except KeyError as e:
            raise e

        return success, message
