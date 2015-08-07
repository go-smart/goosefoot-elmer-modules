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
import yaml

with open('/shared/input/parameters.yml', 'r') as f:
    parameter_dict = yaml.safe_load(f)

with open('/shared/input/needle_parameters.yml', 'r') as f:
    needle_parameter_dicts = dict({v['index']: v['parameters'] for v in yaml.safe_load_all(f)})


# Note that this class is not ready to replace dict entirely (that
# requires more method overriding than here)
class ParameterDict(dict):
    def __getattr__(self, attr):
        return self.__getitem__(attr)

    def __setitem__(self, attr, value):
        param = convert_parameter(value[1], value[0])
        super(ParameterDict, self).__setitem__(attr, param)

    def update(self, *args, **kwargs):
        update_dict = dict(*args, **kwargs)
        super(ParameterDict, self).update({k: convert_parameter(v[1], v[0]) for k, v in update_dict.items()})


def convert_parameter(parameter, typ=None, try_json=True):
    # Why do we distinguish between numeric classes in Python?!
    # Because we do not want to introduce rounding errors where
    # none are expected by switching a counter to float. Also,
    # some Python functions, like range, require an int.

    if parameter == "null" or parameter is None:
        return None

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

    if cast is not None:
        try:
            return cast(parameter)
        except ValueError:
            pass

    if try_json:
        try:
            return json.loads(parameter)
        except:
            pass

    return parameter


P = ParameterDict()
P.update(parameter_dict)
NP = {k: ParameterDict().update(v) for k, v in needle_parameter_dicts.items()}
