#!/usr/bin/python3

import asyncio
import shutil
import json
import os

from watchdog.observers import Observer
from watchdog.events import PatternMatchingEventHandler
from hachiko.hachiko import AIOEventHandler

socket_location = '/var/run/docker-launch/docker-launch.sock'


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


class Submitter:
    reader = None
    writer = None

    def __init__(self):
        self._input_files = []
        self._output_files = []
        self._output_directory = None

    def __del__(self):
        self.finalize()

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

    def add_input(self, input_file):
        self._input_files.append(input_file)

    def output(self, requested):
        if not self._output_directory:
            return None

        requested_location = os.path.join(self._output_directory, requested)

        try:
            with open(requested_location, 'r') as f:
                return f.read()
        except:
            return None

    def notify_output(self, filename):
        print('Output:', filename)
        if filename not in self._output_files:
            self._output_files.append(filename)

    def send_command(self, writer, command, arguments):
        print('-->', command, arguments)
        writer.write(bytes("%s\n" % json.dumps({
            'command': command,
            'arguments': arguments
        }), 'UTF-8'))

    @asyncio.coroutine
    def run_script(self, loop, working_directory, image, files_required=[]):
        success = True

        try:
            reader, writer = yield from asyncio.open_unix_connection(
                socket_location
            )
        except Exception as e:
            print("Could not open connection: %s" % str(e))
            raise e

        self.reader, self.writer = reader, writer

        print("Simulating")
        try:
            self.send_command(writer, 'START', {'image': image})
            success, message = yield from self.receive_response(reader)
            print('<--', success, message)

            if not success:
                raise RuntimeError('Could not start: %s', message)

            try:
                magic_script = os.path.join(
                    message['volume location'],
                    message['input subdirectory'],
                    message['magic script']
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

            event_handler = OutputHandler(self.notify_output, loop=loop)
            observer = Observer()
            observer.schedule(
                event_handler,
                self._output_directory,
                recursive=True
            )
            observer.start()

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

            with open(magic_script, 'w') as f, open(os.path.join(working_directory, 'start.py'), 'r') as g:
                f.write(g.read())

            print("Wrote magic script to %s" % magic_script)

            self.send_command(writer, 'WAIT', None)
            success, message = yield from self.receive_response(reader)
            print('<--', success, message)

            if not success:
                raise RuntimeError('Could not wait: %s', message)

            observer.stop()

            self.send_command(writer, 'LOGS', None)
            success, message = yield from self.receive_response(reader)
            print('<--', success, message.replace('\\n', '\n'))

            if not success:
                raise RuntimeError('Could not retrieve logs: %s', message)

            for output_file in ('docker_inner.log', 'job.out', 'job.err'):
                print('-' * 20)
                print(output_file.upper())

                output_log = self.output(os.path.join('logs', output_file))

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


def main():
    submitter = Submitter()
    loop = asyncio.get_event_loop()
    loop.run_until_complete(submitter.run_script(loop, '.', 'gosmart/fenics-stable-ppa'))
    loop.close()


if __name__ == '__main__':
    main()
