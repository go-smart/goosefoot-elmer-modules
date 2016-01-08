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

# NB: redbrain has renamed observant to vigilant. The comments use 'vigilant' as
# the correct current name for the project. The code should be updated when
# vigilant support is updated to a sufficiently recent version

# See if we can find vigilant
try:
    import StatsCore
    from StatsCore.SimpleTransports import UDPStatsTransport, TCPStatsTransport
    from configparser import RawConfigParser as CParser
    import os
    import time
    use_observant = True
except:
    use_observant = False

from .config import etc_location


# This is a derived class of GSL including some vigilant support
class GoSmartLoggerVigilant(GoSmartLogger):
    def __init__(self, observant=None, *args, **kwargs):
        global use_observant
        super(GoSmartLoggerVigilant, self).__init__(*args, **kwargs)

        self._use_observant = use_observant

        # If we have a running vigilant client, go ahead and use it
        if observant is not None:
            self.client = observant
        else:
            self.client = None

            if self._use_observant:
                self._start_observing()
                self.print_line('Initiated Vigilant')

    def _start_observing(self):
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
        self.client.postWatchPid('go-smart-launcher', os.getpid())
        time.sleep(1)

    def print_debug(self, line='', prefix='| '):
        if self.debug:
            self.log_line(line, prefix, color="CYAN", transmit=False)

    def print_line(self, line='', prefix='| ', color="GREEN", color_text=True, color_bright=False):
        self.log_line(line, prefix, color, color_text, color_bright, transmit=True)

    def log_line(self, line='', prefix='| ', color="GREEN", color_text=True, color_bright=False, transmit=True):
        super(GoSmartLoggerVigilant, self).print_line(line, prefix, color, color_text, color_bright)

        #FIXME: this can hold up the logging until the original process chokes on the lack of pipe cleanage
        #if self._use_observant and transmit and self.client is not None:
        #    self.client.postLogMessageForKey('go-smart-launcher', line)

    def __del__(self):
        if self.client is not None:
            self.client.close()
