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
import os

from gosmart.launcher.component import GoSmartComponent
from gosmart.launcher.globals import defaults


class GoSmartPreprocessorInterface(GoSmartComponent):
    suffix = 'preprocessor'

    def __init__(self, logger):
        super().__init__(logger)

        self.command = defaults["preprocessor command"]
        self.parameters = {
            "radius": 30,
            "centre": (0, 0, 0),
        }
        self.components = []

    def parse_config(self, config_node):
        super().parse_config(config_node)

        for section in config_node:
            if section.tag == 'radius':
                self.parameters['radius'] = section.get('value')
            elif section.tag == 'centre':
                self.parameters['centre'] = [float(section.get(c)) for c in ('x', 'y', 'z')]
            elif section.tag == 'components':
                for f in section:
                    self.components.append(f.get('name'))
            else:
                self.logger.print_fatal("Unknown section for preprocessor: %s\n" % section.tag)

    def launch(self, input_file):
        super().launch()

        output_file = os.path.join(self.logger.get_cwd(), self.suffix, '%s.stl' % os.path.basename(input_file))

        args = [
            input_file,
            output_file,
            self.parameters['radius']
        ] + self.parameters['centre']

        self._launch_subprocess(self.command, args)

        self.logger.print_line("Preprocessed %s -> %s" % (input_file, output_file))

        return output_file
