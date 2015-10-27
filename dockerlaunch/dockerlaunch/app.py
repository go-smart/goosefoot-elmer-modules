import threading
import os
import signal

from .server import ThreadedUnixServer
from .handler import ThreadedUnixRequestHandler


class DockerLaunchApp:
    def __init__(self, docker_settings, socket_location,
                 pidfile_path, logger, pidfile_timeout=5,
                 stdout_path='/dev/tty', stderr_path='/dev/tty',
                 stdin_path='/dev/null'):
        self.docker_settings = docker_settings
        self.socket_location = socket_location
        self.pidfile_path = pidfile_path
        self.pidfile_timeout = pidfile_timeout
        self.stdout_path = stdout_path
        self.stderr_path = stderr_path
        self.stdin_path = stdin_path
        self._logger = logger

    def terminate(self, signo, stackframe):
        self._logger.info("Terminating on signal %d" % signo)

        self._server.shutdown()

        self._logger.info("Server shutdown")

        if os.access(self.socket_location, 0):
            os.remove(self.socket_location)

    def run(self):
        self._logger.info("Starting up...")

        def bootstrap_request_handler(*args, **kwargs):
            request_handler = ThreadedUnixRequestHandler(
                self.docker_settings,
                self._logger,
                *args,
                **kwargs
            )

            return request_handler

        print(self.socket_location)
        self._server = ThreadedUnixServer(
            self.socket_location,
            bootstrap_request_handler
        )

        self._logger.info("Starting server")

        signal.signal(signal.SIGTERM, self.terminate)

        # NOTE: This must be in a separate thread or terminate will deadlock
        server_thread = threading.Thread(target=self._server.serve_forever)
        server_thread.start()
