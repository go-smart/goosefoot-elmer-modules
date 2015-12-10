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
import string
import errno

from gosmart.launcher.mesher import GoSmartMesher
from gosmart.launcher.globals import slugify
import gosmart.config


class GoSmartMesherAxisymmetric(GoSmartMesher):
    mesher_binary = 'gmsh'

    def __init__(self, logger):
        super().__init__(logger)

        self.output_prefix = None
        self.nearfield = 0.3
        self.farfield = 3.0
        self.height = 1.0
        self.width = 1.0
        self._geo_mapping = {}

    def parse_config(self, config_node):
        super().parse_config(config_node)

        for node in config_node:
            if node.tag == 'template':
                if node.get('height') is not None:
                    self.height = float(node.get('height'))
                if node.get('width') is not None:
                    self.width = float(node.get('width'))

                self.template_name = node.get('name')
                for dimension in node:
                    self._geo_mapping[slugify("CONSTANT_%s" % dimension.get('name'))] = dimension.get('value')
                    self.logger.ensure_constant(dimension.get('name'), dimension.get('value'), typ='float')
            elif node.tag == 'lengthscales':
                self.nearfield = float(node.get('nearfield'))
                self.farfield = float(node.get('farfield'))

    def launch(self, needle_file, extent_file, preprocessor=None, appendix=""):
        self.output_prefix = "%s/%s" % (self.suffix, self.logger.runname)

        meshes = {}
        meshes.update(self._launch_mesh(self.template_name, appendix))

        # if self.add_needleless:
        #    meshes.update(self._launch_mesh(self.template_name + '_needleless', appendix + '-needleless'))

        return meshes

    def _launch_mesh(self, template, appendix):
        # output_prefix = "%s/%s" % (self.suffix, self.logger.runname)

        # f = lambda n: n.replace("OUTDIR", self.suffix)

        geo_mapping = {
            "NEARFIELD": self.nearfield,
            "FARFIELD": self.farfield,
            "INNERHEIGHT": self.height,
            "INNERWIDTH": self.width,
        }
        geo_mapping.update(self._geo_mapping)
        geo_mapping.update(self.logger.get_region_ids())
        geo_template_filename = os.path.basename("go-smart-axisymm_%s.geo" % self.template_name)
        self.logger.print_line(geo_template_filename)
        geo_template_stream = open(os.path.join(gosmart.config.template_directory, "templates", geo_template_filename), "r")
        geo_template = string.Template(geo_template_stream.read())
        geo_template_stream.close()

        try:
            os.makedirs(self.logger.make_cwd(self.suffix))
        except OSError as e:
            if e.errno != errno.EEXIST:
                self.print_fatal("Could not create %s directory: %s" %
                                 (self.suffix, str(e)))

        geo_file = "%s.geo" % self.logger.runname
        geo_string = geo_template.substitute(geo_mapping)
        geo_stream = open(os.path.join(self.logger.make_cwd(self.suffix), geo_file), "w")
        geo_stream.write(geo_string)
        geo_stream.close()

        args = ["-2", geo_file]

        self.cwd = self.suffix

        return super().launch(None, None, args, appendix)
