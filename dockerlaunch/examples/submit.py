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
    def __init__(self):
        self._output_files = []
        self._output_directory = None

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
    def run_script(self, loop):
        try:
            reader, writer = yield from asyncio.open_unix_connection(
                socket_location
            )
        except Exception as e:
            print("Could not open connection: %s" % str(e))

        try:
            self.send_command(writer, 'START', None)
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

            for f in os.listdir('input'):
                location = os.path.join('input', f)
                if not f.startswith('.') and not os.path.isdir(location):
                    shutil.copyfile(
                        location,
                        os.path.join(self._input_directory, f)
                    )

            with open(magic_script, 'w') as f, open('start.py', 'r') as g:
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

            self.send_command(writer, 'DESTROY', None)
            success, message = yield from self.receive_response(reader)
            print('<--', success, message)

            if not success:
                raise RuntimeError('Could not destroy: %s', message)
        except Exception as e:
            print(str(e))
        finally:
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
    loop.run_until_complete(submitter.run_script(loop))
    loop.close()


if __name__ == '__main__':
    main()
