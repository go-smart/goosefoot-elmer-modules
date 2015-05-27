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
from __future__ import print_function

import numpy as N


U = N.array((0,1,0))
V = N.array((-0.41, -0.48, -0.78))

n = N.cross(U, V) / (N.linalg.norm(U) * N.linalg.norm(V))
print(n)
s = N.linalg.norm(n)
print(s)
c = N.dot(U, V) / (N.linalg.norm(U) * N.linalg.norm(V))
print(c)

nx = N.matrix(((0, -n[2], n[1]),
               (n[2], 0, -n[0]),
               (-n[1], n[0], 0)))
print(nx)
print(nx * nx)

R = N.eye(3) + nx + nx * nx * (1 - c)

print(R)

Px = N.array((0, 1, 0))
Py = N.array((0, 0, 1))
Pz = N.array((1, 0, 0))

print("Rx: ", N.arccos(N.dot(Px, N.dot(R, Px.T).T)) * 180 / N.pi)
print("Ry: ",N.arccos(N.dot(Py, N.dot(R, Py.T).T)) * 180 / N.pi)
print("Rz: ", N.arccos(N.dot(Pz, N.dot(R, Pz.T).T)) * 180 / N.pi)
