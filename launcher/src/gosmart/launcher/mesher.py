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
import shutil

from gosmart.launcher.component import GoSmartComponent


class GoSmartMesher(GoSmartComponent):
    suffix = 'mesher'

    @staticmethod
    def get_default_msh_outfile(prefix, appendix=""):
        suffix = "-%s" % appendix if appendix != "" else ""
        return {suffix: prefix + ".msh"}

    def __init__(self, logger):
        super().__init__(logger)
        self.mesh = {}
        self.skip_needle = False

    def parse_config(self, config_node):
        super().parse_config(config_node)

    def launch(self, needle_files, extent_file, args=None, appendix=""):
        super().launch()

        if args is None:
            args = []

        if self.skip_needle:
            needle_files = {}

        filelist = []
        if needle_files is not None:
            filelist += list(needle_files.items())

        if extent_file is not None and os.path.exists(extent_file):
            filelist.append(("extent", extent_file))

        for f in filelist:
            t = os.path.join(self.logger.make_cwd(self.suffix), "needle-%s.stl" % f[0])
            if f[1] is not None:
                try:
                    shutil.copy(os.path.join(self.logger.get_cwd(), f[1]), t)
                except Exception as e:
                    self.logger.print_fatal("Could not copy STL across for mesher: %s" % str(e))

        # Need to put filename argument in here

        self._launch_subprocess(self.mesher_binary, args)

        # Meshers should return the input MSH file for ElmerGrid
        return GoSmartMesher.get_default_msh_outfile(self.output_prefix, self.suffix)

    def alter_extent(self, filename):
        # Subclasses should update the boundary extent filename based on this.
        pass
