import os
import socketserver


class ThreadedUnixServer(socketserver.ThreadingMixIn,
                         socketserver.UnixStreamServer):
    daemon_threads = True

    def server_activate(self):
        super().server_activate()
        os.chmod(self.server_address, 0o770)
