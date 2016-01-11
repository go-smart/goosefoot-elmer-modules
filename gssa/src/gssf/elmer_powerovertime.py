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

from .errors import GoSmartModelError


# THIS MODULE SHOULD NOW BE REDUNDANT AND IS DEPRECATED (FUNCTIONALITY NOW IN
# CDM PROTOCOLS)


# This provides a template for a power-over-time factory. It must be able to
# receive an XML configuration node and get a ``generate_powers`` member call.
class GoSmartElmerPowerOverTimeFactory:
    def generate_powers(self):
        return None

    def parse_config(self, config_node):
        pass


class GoSmartElmerPowerOverTimeFactoryPiecewiseLinear(GoSmartElmerPowerOverTimeFactory):
    delta = 0.1

    def __init__(self):
        self.powers = {}

    def generate_powers(self, endtime):
        currentlast = max(self.powers.keys())
        if currentlast < endtime:
            self.powers[endtime] = self.powers[currentlast]

        veryend = max(*self.powers.keys()) + self.delta
        self.powers[veryend] = 0.0
        return self.powers

    def parse_config(self, config_node):
        for node in config_node:
            if node.tag != "power":
                raise GoSmartModelError("Piecewise linear power over time should have 'power' subelements")
            self.powers[float(node.get("time"))] = float(node.get("magnitude"))


class GoSmartElmerPowerOverTimeFactoryConstant(GoSmartElmerPowerOverTimeFactory):
    delta = 0.1

    def __init__(self):
        pass

    def generate_powers(self, endtime):
        return {0.0: self.constant, self.offtime: self.constant, (self.offtime + self.delta): 0.0}

    def parse_config(self, config_node):
        self.constant = float(config_node.get('constant'))
        self.offtime = float(config_node.get('offtime'))

power_over_time_factories = {
    "constant": GoSmartElmerPowerOverTimeFactoryConstant,
    "piecewise linear": GoSmartElmerPowerOverTimeFactoryPiecewiseLinear,
}
