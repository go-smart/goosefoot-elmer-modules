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
import json


def convert_parameter(parameter, typ=None, try_json=True):
    # Why do we distinguish between numeric classes in Python?!
    # Because we do not want to introduce rounding errors where
    # none are expected by switching a counter to float. Also,
    # some Python functions, like range, require an int.

    if parameter == "null" or parameter is None:
        return None

    # Some basic types will be done manually
    if typ == "float":
        cast = float
    elif typ == "integer":
        cast = int
    elif typ == "boolean":
        cast = lambda s: (s.lower() != "false" and bool(s))
    elif typ == "string":
        cast = str
    else:
        cast = None

    # If we found one, pass the parameter
    if cast is not None:
        try:
            return cast(parameter)
        except ValueError:
            pass

    # If we have had no success yet and should try converting from JSON, do so
    if try_json:
        try:
            return json.loads(parameter)
        except:
            pass

    return parameter


def read_parameters(element):
    return dict(map(lambda p: (p.get('name'), (p.get('value'), p.get('type') if p.get('type') else None)), element))
