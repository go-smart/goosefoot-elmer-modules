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
from gosmart.server.families.docker import DockerFamily

import asyncio
from gosmart.server.families.mesher_gssf import MesherGSSFMixin


class FenicsFamily(DockerFamily, MesherGSSFMixin):
    family_name = "fenics"
    _docker_image = 'gosmart/fenics-stable-ppa'

    def __init__(self, files_required):
        super().__init__(files_required)
        self.init_mesher(files_required)

    @asyncio.coroutine
    def prepare_simulation(self, working_directory):
        return ((yield from self.mesh(working_directory)) and
                (yield from super().prepare_simulation(working_directory)))
