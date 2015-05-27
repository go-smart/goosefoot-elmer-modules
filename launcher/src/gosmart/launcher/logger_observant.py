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
from gosmart.launcher.logger import GoSmartLogger

try:
    import StatsCore
    from StatsCore.SimpleTransports import UDPStatsTransport, TCPStatsTransport
    from configparser import RawConfigParser as CParser
    import os
    import time
    use_observant = True
except:
    use_observant = False


class GoSmartLoggerObservant(GoSmartLogger):
    def __init__(self, observant=None, *args, **kwargs):
        global use_observant
        super(GoSmartLoggerObservant, self).__init__(*args, **kwargs)

        self._use_observant = use_observant

        if observant is not None:
            self.client = observant
        else:
            self.client = None

            if self._use_observant:
                self._start_observing()
                self.print_line('Initiated Observant')

    def _start_observing(self):
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
        time.sleep(1)

    def print_debug(self, line='', prefix='| '):
        if self.debug:
            self.log_line(line, prefix, color="CYAN", transmit=False)

    def print_line(self, line='', prefix='| ', color="GREEN", color_text=True, color_bright=False):
        self.log_line(line, prefix, color, color_text, color_bright, transmit=True)

    def log_line(self, line='', prefix='| ', color="GREEN", color_text=True, color_bright=False, transmit=True):
        super(GoSmartLoggerObservant, self).print_line(line, prefix, color, color_text, color_bright)

        if self._use_observant and transmit and self.client is not None:
            self.client.postLogMessageForKey('go-smart-launcher', line)

    def __del__(self):
        if self.client is not None:
            self.client.close()
