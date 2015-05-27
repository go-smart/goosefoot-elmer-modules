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


# Class to hold settings specific to the lesion post-processing
class GoSmartLesion(GoSmartComponent):
    suffix = 'lesion'

    binary_name = 'go-smart-lesion'
    threshold = 0.8
    selection = 'most-recent'
    field = 'dead'
    connectivity = False
    subdivide = False
    smoothing_iterations = 0
    scaling = 1000
    threshold_lower = None
    threshold_upper = None

    def _most_recent(self, cwd, prefix, criterion):
        filtered = filter(lambda f: criterion(f, prefix), os.listdir(cwd))
        most_recent = max(filtered, key=lambda f: os.stat(os.path.join(self.logger.get_cwd(), cwd, f)).st_mtime)
        return most_recent

    def _largest_time(self, cwd, prefix, criterion):
        filtered = filter(lambda f: criterion(f, prefix), os.listdir(cwd))
        largest_time = max(filtered, key=lambda f: re.findall(r'\d+', f)[0])
        return largest_time

    def __init__(self, logger):
        super().__init__(logger)

        self._find_input = {
            "most-recent": self._most_recent,
            "largest-time": self._largest_time,
        }

    def parse_config(self, config_node):
        super().parse_config(config_node)

        threshold_lower = config_node.get("threshold_lower")
        if threshold_lower is not None:
            self.threshold_lower = float(threshold_lower)

        threshold_upper = config_node.get("threshold_upper")
        if threshold_upper is not None:
            self.threshold_upper = float(threshold_upper)

        selection = config_node.get("selection")
        if selection is not None:
            self.selection = selection

        field = config_node.get("field")
        if field is not None:
            self.field = field

        scaling = config_node.get("scaling")
        if scaling is not None:
            self.scaling = float(scaling)
        elif abs(self.logger.geometry["simulationscaling"] - 1.) > EPS:
            self.scaling = 1 / self.logger.geometry["simulationscaling"]

        self.connectivity = config_node.get("connectivity") == "true"
        self.subdivide = config_node.get("subdivide") == "true"
        if config_node.get("smoothing_iterations"):
            self.smoothing_iterations = int(config_node.get("smoothing_iterations"))
        else:
            self.smoothing_iterations = 0

    def launch(self, input_cwd=None, input_prefix=None, is_parallel=False):
        super().launch()

        if input_cwd is None:
            input_cwd = "elmer/%s-mesher" % self.logger.runname

        input_cwd = os.path.join(self.logger.get_cwd(), input_cwd)

        if is_parallel:
            criterion = lambda f, p: f.startswith(p) and f.endswith(".pvtu")
        else:
            criterion = lambda f, p: f.startswith(p) and f.endswith(".vtu")

        def prefix_ct(p):
            return len([f for f in os.listdir(input_cwd) if criterion(f, p)])

        if input_prefix is None:
            input_prefix = self.logger.runname.lower()
            if prefix_ct(input_prefix) != 0:
                if prefix_ct("case") != 0:
                    raise RuntimeError("Cannot guess prefix ('%s' or 'case') as VTUs of both exist - clean one or other out" % self.logger.runname)
            else:
                input_prefix = "case"
        else:
            input_prefix = input_prefix.lower()

        if prefix_ct(input_prefix) == 0:
            raise RuntimeError("No VTU output found for prefix %s" % input_prefix)

        selection_method = self._find_input[self.selection]
        input_name = selection_method(input_cwd, input_prefix, criterion)

        output_name = self.logger.runname + ".vtp"
        analysis_name = self.logger.runname + "-analysis.xml"

        try:
            shutil.copy(os.path.join(input_cwd, input_name), self.logger.make_cwd(self.suffix))
        except Exception as e:
            self.logger.print_fatal("Could not copy input mesh across for lesion exciser: %s" % str(e))

        args = [
            "--input", input_name,
            "--output", output_name,
            "--analysis", analysis_name,
            "--scale", self.scaling,
            "--field", self.field,
            "--smoothing-iterations", self.smoothing_iterations,
        ]

        if self.threshold_lower:
            args.append("--threshold-lower")
            args.append(self.threshold_lower)

        if self.threshold_upper:
            args.append("--threshold-upper")
            args.append(self.threshold_upper)

        if self.connectivity:
            args.append("--connectivity")
        if self.subdivide:
            args.append("--subdivide")
        if is_parallel:
            args.append("--parallel")

        self.cwd = self.suffix

        self._launch_subprocess(self.binary_name, args)
        return os.path.join(self.cwd, output_name)
