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
import numpy as N

colorama_imported = False
try:
    import colorama
    colorama_imported = True
except:
    colorama_imported = False

launcher_command = "go-smart-launcher"

debug = False

defaults = {
    "elmer binary": "ElmerSolver_mpi",
    "fortran binary": "gfortran",
    "elmer mpi binary": "ElmerSolver_mpi",
    "needle library command": "go-smart-needle-library",
    "preprocessor command": "go-smart-preprocessor",
}

numa_elmer_library_directory = "lib/numa-elmer"

_elmer_modules_required = {
    "rfa point sources": [],
    "rfa point sources (modified bioheat)": [],
    "rfa point sources (old elmer)": [],
    "rfa joule heating": [],
    "mwa": [],
    "mwa (nonlinear)": ["mwa_RelPerm", "mwa_ElecCond","mwa_PowerOverTime"],
    "cryoablation": [],
    "ire": [],
}

EPS = 1e-8


# TODO:PTW:THIS SHOULD BE REPLACED WITH SLUGIFY!!!
def slugify(inp):
    inp = inp.upper().replace('-', ' ')
    inp = inp.replace('(', '')
    inp = inp.replace(')', '')
    inp = inp.replace(' ', '_')

    return inp


def _generate_rotation_matrix(x, y, z, backward=False):
    R = _generate_rotation_matrix_numpy(x, y, z, backward)
    return " ".join([str(d) for d in R.flat])


def _generate_rotation_matrix_numpy(x, y, z, backward=False, rx=0, ry=1, rz=0):
    a = N.array((rx, ry, rz))
    b = N.array((float(x), float(y), float(z)))
    if backward:
        U = b
        V = a
    else:
        U = a
        V = b

    n = N.cross(U, V) / (N.linalg.norm(U) * N.linalg.norm(V))
    s = N.linalg.norm(n)
    if abs(s) > 1e-12:
        n = n / s
    elif (abs(x) < 1e-12 and abs(y) < 1e-12):
        n = N.array((1, 0, 0))
    else:
        n = N.array((-y, x, 0))
        n = n / N.linalg.norm(n)

    c = N.dot(U, V) / (N.linalg.norm(U) * N.linalg.norm(V))

    nx = N.matrix(((0, -n[2], n[1]),
                   (n[2], 0, -n[0]),
                   (-n[1], n[0], 0)))
    R = N.eye(3) + s * nx + nx * nx * (1 - c)
    return R


_sif_calculations = {
    "PERFUSION_COEFFICIENT": {"requirements":
                              ("CONSTANT_PERFUSION_RATE_TISSUE",
                               "CONSTANT_THERMAL_CONDUCTIVITY_TISSUE"),
                              "formula": lambda wb, kt:
                              ((1. / (60. * 1000.)) * wb / kt)
                              },  # Convert to ml/(ml sec)
    "ROTATION MATRIX": {"requirements":
                        ("NEEDLE_AXIS_X",
                         "NEEDLE_AXIS_Y",
                         "NEEDLE_AXIS_Z",),
                        "formula": _generate_rotation_matrix},
    "ROTATION MATRIX_BACKWARD": {"requirements":
                                 ("NEEDLE_AXIS_X",
                                  "NEEDLE_AXIS_Y",
                                  "NEEDLE_AXIS_Z",),
                                 "formula": lambda x, y, z:
                                 _generate_rotation_matrix(x, y, z, backward=True)},
}
