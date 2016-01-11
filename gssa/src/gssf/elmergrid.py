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

from .component import GoSmartComponent
from .globals import EPS


# Component for running ElmerGrid
class GoSmartElmerGrid(GoSmartComponent):
    suffix = 'elmergrid'

    # This is the format ID used in the ElmerGrid command-line representing the ElmerSolver-compatible mesh format
    _elmer_format = 2
    scale = None

    # ``from_format`` is the format ID used in the ElmerGrid command-line to indicate MSH input (GMSH)
    def __init__(self, logger, from_format=14, elmergrid_binary=None):
        super().__init__(logger)

        # Unless overridden, we assume ElmerGrid is accessible globally as ``ElmerGrid``
        if elmergrid_binary is None:
            elmergrid_binary = "ElmerGrid"

        self.elmergrid_binary = elmergrid_binary
        self._from_format = from_format

    # Extract data from the GSSF-XML
    def parse_config(self, config_node):
        super().parse_config(config_node)

        # Scale the meshes, by the provided parameter or, otherwise, the global simulationscaling
        if config_node.get('scale'):
            self.scale = config_node.get('scale').split(" ")
        elif abs(self.logger.geometry["simulationscaling"] - 1.) > EPS:
            self.scale = [self.logger.geometry["simulationscaling"]] * 3

        # This should be placed somewhere better, but as the ElmerGrid scaling
        # translates to the problem scaling, we need to add it in to the needle
        # scaling. This does mean needlelibrary should precede ElmerGrid
        needle_scale = [1., 1., 1.]
        for i, c in enumerate(('x', 'y', 'z')):
            if self.scale is not None:
                self.add_or_update_constant("scale " + c, self.scale[i])
                needle_scale[i] *= self.scale[i]

            if self.get_constant(c, group="needle") is not None:
                self.add_or_update_constant(
                    "scaled " + c,
                    float(self.get_constant(c, group='needle')) * float(needle_scale[i]),
                    group="needle")

            if self.get_constant("axis ", group="needle") is not None:
                self.add_or_update_constant(
                    "axis scaled " + c,
                    float(self.get_constant("axis " + c), group="needle") * float(needle_scale[i]),
                    group="needle")

    # Run the subprocess
    def launch(self, input_mesh=None, nprocs=1, appendix="", reorder_zones=True):
        super().launch()

        mesh_name = self.logger.runname + appendix
        copied_mesh = self.suffix + "/" + mesh_name + ".msh"

        if input_mesh is None:
            input_mesh = "mesher/%s.msh" % self.logger.runname

        input_mesh = os.path.join(self.logger.get_cwd(), input_mesh)
        copied_mesh = os.path.join(self.logger.get_cwd(), copied_mesh)

        try:
            shutil.copy(input_mesh, copied_mesh)
        except Exception as e:
            self.logger.print_fatal("Could not copy input mesh across for ElmerGrid: %s" % str(e))

        # By default, we only use removedunused flag for additional processing
        args = [self._from_format, self._elmer_format, mesh_name + ".msh", "-removeunused"]

        # If the simulation will be run with multiple processes, we need to
        # split the mesh
        if nprocs > 1:
            args += ["-metis", nprocs]
        if self.scale is not None:
            args += ["-scale"] + self.scale

        self.cwd = self.suffix

        # Run the subprocess using the arguments selected above
        self._launch_subprocess(self.elmergrid_binary, args)

        return os.path.join(self.cwd, mesh_name)
