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
import argparse


def parse_args():
    parser = argparse.ArgumentParser(description='Wrapper for Go-Smart simulation environment')
    parser.add_argument('--elmer', dest='elmer_binary', help='Name of the ElmerSolver binary')
    parser.add_argument('--elmer-logfile', dest='outfilename', help='Name of the ElmerSolver binary')
    parser.add_argument('--logfile-addpid', dest='addpid',
                        action='store_true', help='Whether the PID should be appended to the given logfile name')
    parser.add_argument('--silent', dest='silent', action='store_true', help='Prevent wrapper output')
    parser.add_argument('--debug', dest='debug', action='store_true', help='Additional, debug output (overridden by --silent)')
    parser.add_argument('--nprocs', dest='nprocs', help='Number of processes to start')
    parser.add_argument('--only', dest='only', help='Only execute a single component')
    parser.add_argument('--black-and-white', action='store_true', dest='baw', help='Force colorama off')
    parser.add_argument('--leavetree', action='store_true', dest='leavetree', help='Do not touch the mesh filetree in Elmer')
    parser.add_argument(dest='configfilenames', nargs=argparse.REMAINDER, help='Locations of configuration file (latter override former)')

    return parser.parse_args()
