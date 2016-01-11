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
import os.path
import shutil

from .component import GoSmartComponent


# Class to hold settings specific to the mesh optimizer (GMSH)
class GoSmartMeshOptimizer(GoSmartComponent):
    suffix = 'optimizer'

    # This relates optimization method to the actual GMSH flags
    _methods = {"gmsh": "-optimize", "netgen": "-optimize_netgen"}

    def __init__(self, logger, method=None, optimizer_binary=None):
        super().__init__(logger)

        # If we don't have an override, this works out as
        # 'gmsh -0 -optimize ...'
        if optimizer_binary is None:
            optimizer_binary = "gmsh"
        if method is None:
            method = "gmsh"

        self.method = method
        self.optimizer_binary = optimizer_binary

    def parse_config(self, config_node):
        super().parse_config(config_node)

        method = config_node.find("method")
        if method is not None and method.text in self._methods:
            self.method = method.text
        else:
            self.method = "gmsh"

    def launch(self, input_mesh=None, appendix=""):
        super().launch()

        self.mesh_name = self.logger.runname + appendix + ".msh"
        copied_mesh = os.path.join(self.suffix, self.mesh_name)
        output_mesh = os.path.join(self.suffix, self.logger.runname + appendix + "-optimized.msh")

        if input_mesh is None:
            input_mesh = "mesher/%s.msh" % self.logger.runname

        input_mesh = os.path.join(self.logger.get_cwd(), input_mesh)
        copied_mesh = os.path.join(self.logger.get_cwd(), copied_mesh)
        output_mesh = os.path.join(self.logger.get_cwd(), output_mesh)

        # Get mesh, probably from the mesher, ready for optimization
        try:
            shutil.copy(input_mesh, copied_mesh)
        except Exception as e:
            self.logger.print_fatal("Could not copy input mesh across for optimizer: %s" % str(e))

        args = [
            "-0",
            self._methods[self.method],
            "-o", output_mesh,
            self.mesh_name,
        ]

        self.cwd = self.suffix

        self._launch_subprocess(self.optimizer_binary, args)
        return os.path.join(self.cwd, output_mesh)
