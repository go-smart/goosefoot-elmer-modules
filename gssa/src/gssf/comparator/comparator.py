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

from lxml import etree as ET
from .parse import gssa_xml_to_definition


# This class sets up two SimulationDefinitions and instructs one to compare
# against the other
class Comparator:
    left_text = None
    right_text = None

    def __init__(self, left_text, right_text):
        # ElementTree can still only handle byte-strings
        self.left = ET.fromstring(bytes(left_text, 'utf-8'))
        self.right = ET.fromstring(bytes(right_text, 'utf-8'))

    def diff(self):
        # We must construct SimulationDefinitions for both sides
        # As we have a clear Left and Right, based on the initializing
        # arguments, we name them accordingly in the output
        left_structure = self.__analyse(self.left, "Left")
        right_structure = self.__analyse(self.right, "Right")

        # The left definition runs a comparison against the right
        return left_structure.diff(right_structure)

    def equal(self):
        return self.diff() == []

    def __analyse(self, root, label):
        # In theory, we might want to something extra here, based on additional
        # parameters or settings, but for now we just return the parsed XML as a
        # SimulationDefinition
        return gssa_xml_to_definition(root, label)
