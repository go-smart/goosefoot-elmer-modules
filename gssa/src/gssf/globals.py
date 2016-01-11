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


# If we can use colorama, import it and provide pretty colour output
colorama_imported = False
try:
    import colorama
    colorama_imported = True
except:
    colorama_imported = False

# We use this for relaunching when multiple processes are used
launcher_command = "go-smart-launcher"

# Ups the level of debug statements
# TODO: switch all this to the logging module
debug = False

# If we cannot find the install-time configuration, guess these binaries - it is
# unlikely that they are wrong
defaults = {
    "elmer binary": "ElmerSolver_mpi",
    "fortran binary": "gfortran",
    "elmer mpi binary": "ElmerSolver_mpi",
    "needle library command": "go-smart-needle-library",
    "preprocessor command": "go-smart-preprocessor",
}

# DEPRECATED: certain pre-known SIF templates require dynamically compiled
# modules
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

# Used to test approximate equality (e.g. simulationscaling~=1)
EPS = 1e-8


# TODO:PTW:THIS SHOULD BE REPLACED WITH SLUGIFY!!!
# However, we need to now check that that won't break anything
def slugify(inp):
    inp = inp.upper().replace('-', ' ')
    inp = inp.replace('(', '')
    inp = inp.replace(')', '')
    inp = inp.replace(' ', '_')

    return inp


# Utility to create a rotation matrix for Elmer
def _generate_rotation_matrix(x, y, z, backward=False):
    R = _generate_rotation_matrix_numpy(x, y, z, backward)
    return " ".join([str(d) for d in R.flat])


# Create rotation matrix to take a reference axis to a desired axis
# This is primarily useful for taking points on, say, a reference needle, to the
# actual orientation of the embedded needle, prior to translation from the
# origin. It uses Rodrigues' rotation formula.
def _generate_rotation_matrix_numpy(x, y, z, backward=False, rx=0, ry=1, rz=0):
    a = N.array((rx, ry, rz))
    b = N.array((float(x), float(y), float(z)))
    if backward:
        U = b
        V = a
    else:
        U = a
        V = b

    # As our rotation axis, we choose the cross product of the supplied vectors.
    # If they are parallel, we take the vector perpendicular to U in the x-y
    # plane. If U and V lie on the z-axis (which happens surprisingly often), we
    # choose the x-axis.
    n = N.cross(U, V) / (N.linalg.norm(U) * N.linalg.norm(V))
    s = N.linalg.norm(n)
    if abs(s) > 1e-12:
        n = n / s
    elif (abs(x) < 1e-12 and abs(y) < 1e-12):
        n = N.array((1, 0, 0))
    else:
        n = N.array((-y, x, 0))
        n = n / N.linalg.norm(n)

    # The remainder of the routine is as standard in R. rotation formula
    c = N.dot(U, V) / (N.linalg.norm(U) * N.linalg.norm(V))

    nx = N.matrix(((0, -n[2], n[1]),
                   (n[2], 0, -n[0]),
                   (-n[1], n[0], 0)))
    R = N.eye(3) + s * nx + nx * nx * (1 - c)
    return R


# These are hard-coded algorithms for calculating additional SIF template
# parameters when the requirements are all available. The lambda formula takes
# the requirements as arguments in the order they are listed below.
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
