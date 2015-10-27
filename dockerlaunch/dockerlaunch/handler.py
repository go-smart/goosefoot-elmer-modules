import socketserver
import select
import json
import sys
import traceback

from .layer import DockerLayer


# From Py3 docs
class ThreadedUnixRequestHandler(socketserver.StreamRequestHandler):
    _docker_layer = None
    _started = False

    def __init__(self, docker_settings, logger, *args, **kwargs):
        self._logger = logger
        self._logger.debug("New handler for new connection")

        self._configure(docker_settings)

        super().__init__(*args, **kwargs)

    def _configure(self, docker_settings):
        kwargs = docker_settings
        kwargs['logger'] = self._logger
        self._docker_layer = DockerLayer(**kwargs)

    def handle(self):
        # We need to retain the connection
        sock = self.connection

        message = b''

        while True:
            input_ready, output_ready, error_ready = \
                select.select([sock], [], [])

            if not input_ready:
                continue

            data = sock.recv(1024)

            if data:
                breakpoint = data.index(b'\n')

                if breakpoint < 0:
                    message += data
                else:
                    message += data[:breakpoint]
                    data = data[(breakpoint + 1):]

                    if message:
                        try:
                            message = json.loads(message.decode('UTF-8'))
                        except ValueError as e:
                            self._logger.error(
                                "Could not parse message as JSON: %s" % str(e)
                            )
                            success = False
                            message = "Unparseable message"

                        try:
                            command = message["command"]
                            arguments = message["arguments"]
                        except KeyError as e:
                            self._logger.error(
                                "Missing component of message: %s" % str(e)
                            )

                        try:
                            success, message = \
                                self._process_message(command, arguments)
                        except Exception as e:
                            exc_type, exc_value, exc_traceback = sys.exc_info()
                            success, message = False, "Exception: %s\n%s" % \
                                (str(e), "\n".join(traceback.format_tb(exc_traceback)))

                        response = json.dumps({
                            'success': success,
                            'message': message
                        })

                        print(response)
                        self.request.sendall(bytes("%s\n" % response, 'UTF-8'))

                    message = b''

    def _process_message(self, message, arguments):
        self._logger.debug(message)

        if message == "START":
            if arguments is not None and 'image' in arguments and 'update socket':
                success, message = self._docker_layer.try_launch(
                    arguments['image'],
                    arguments['update socket'] if 'update socket' in arguments else None
                )
            else:
                success, message = False, "Image must be supplied in arguments"

            if success:
                self._started = True
        elif message == "LOGS":
            container_id = self._docker_layer.get_container_id()
            if container_id is None:
                success, message = False, "No container associated"
            else:
                container_logs = self._docker_layer.get_container_logs()
                if container_logs is None:
                    success, message = False, "Could not retrieve logs"
                else:
                    success, message = True, container_logs.decode('UTF-8')
        elif message == "WAIT":
            if arguments is not None and 'timeout' in arguments:
                timeout = arguments['timeout']
            else:
                timeout = None

            if not self._started:
                success, message = False, "Must successfully start first"
            else:
                success, message = self._docker_layer.wait(timeout=timeout)
        elif message == "USAGE":
            container_count = self._docker_layer.get_container_count()
            if container_count is None:
                success, message = False, "No Docker client connection"
            else:
                success, message = True, container_count
        elif message == "CONTAINER":
            container_id = self._docker_layer.get_container_id()
            if container_id is None:
                success, message = False, "No container associated"
            else:
                success, message = True, container_id
        elif message == "DESTROY":
            if not self._started:
                success, message = False, "Must successfully start first"
            else:
                success, message = self._docker_layer.destroy()
        else:
            success = False
            message = "Unknown command: %s" % message

        return success, message
