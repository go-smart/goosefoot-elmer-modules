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
from autobahn.asyncio.wamp import ApplicationSession
import uuid
from lxml import etree as ET


class GoSmartSimulationClientComponent(ApplicationSession):

    def __init__(self, x, gssa_file, subdirectory):
        ApplicationSession.__init__(self, x)
        self._gssa = ET.parse(gssa_file)
        self._guid = uuid.uuid1()
        self._subdirectory = subdirectory

    def onJoin(self, details):
        print("session ready")

        guid = str(self._guid)
        gssa = ET.tostring(self._gssa, encoding="unicode")
        self.call(u'com.gosmartsimulation.init', guid)
        self.call(u'com.gosmartsimulation.update_settings_xml', guid, gssa)
        self.call(u'com.gosmartsimulation.finalize', guid, self._subdirectory)
        self.call(u'com.gosmartsimulation.start', guid)
