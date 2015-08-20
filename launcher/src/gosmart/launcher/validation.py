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
import re
import os
import shutil

from gosmart.launcher.component import GoSmartComponent
from gosmart.launcher.globals import EPS
from gosmart.launcher.errors import GoSmartClientError, GoSmartServerError


# Note that this module requires availability of the (closed source) Aalto validation tool
class GoSmartValidation(GoSmartComponent):
    suffix = 'validation'

    binary_name = 'AaltoTCV'
    registration = True
    refdata = None

    def __init__(self, logger):
        super().__init__(logger)

    def parse_config(self, config_node):
        if config_node.get('registration') and config_node.get('registration').lower() == 'false':
            self.registration = False

        self.refdata = config_node.get('reference')

        super().parse_config(config_node)

    def launch(self, input_cwd=None, input_prefix=None, is_parallel=False):
        super().launch()

        if input_cwd is None:
            input_cwd = "elmer"

        input_cwd = os.path.join(self.logger.get_cwd(), input_cwd)
        input_name = self.logger.runname + ".vtp"
        output_name = self.logger.runname + "-deviation.vtp"
        analysis_name = self.logger.runname + "-analysis.xml"
        refdata_name = self.logger.surfaces[self.refdata]

        try:
            shutil.copy(os.path.join(input_cwd, input_name), self.logger.make_cwd(self.suffix))
        except Exception as e:
            self.logger.print_fatal("Could not copy input mesh across for lesion exciser: %s" % str(e))

        args = [
            refdata_name,
            input_name,
            analysis_name,
            output_name,
            1 if self.registration else 0
        ]

        self.cwd = self.suffix

        self._launch_subprocess(self.binary_name, args)
        return os.path.join(self.cwd, output_name)
