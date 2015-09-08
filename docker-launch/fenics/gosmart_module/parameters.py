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
import socket
import os

with open('/shared/input/parameters.yml', 'r') as f:
    parameter_dict = yaml.safe_load(f)

with open('/shared/input/regions.yml', 'r') as f:
    region_dict = yaml.safe_load(f)

with open('/shared/input/needle_parameters.yml', 'r') as f:
    needle_parameter_dicts = dict({v['index']: v['parameters'] for v in yaml.safe_load_all(f)})


class StatusUpdater:
    def __init__(self, update_socket_location):
        self._update_socket_location = update_socket_location

    def connect(self):
        self._update_socket = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        if not os.path.exists(self._update_socket_location):
            return False
        self.connect(self._update_socket_location)

    def status(self, percentage, message):
        try:
            percentage = float(percentage)
        except ValueError:
            self.sendall(b'%s\n' % message)
        else:
            self.sendall(b'%lf|%s\n' % (percentage, message))

update = StatusUpdater('/update.sock')
update_available = update.connect()


class AttributeDict(dict):
    def __getattr__(self, attr):
        return self.__getitem__(attr)


# Note that this class is not ready to replace dict entirely (that
# requires more method overriding than here)
class ParameterDict(AttributeDict):
    def __init__(self, *args, **kwargs):
        self.update(*args, **kwargs)

    def __getattr__(self, attr):
        return self.__getitem__(attr)

    def __setitem__(self, attr, value):
        param = convert_parameter(value[1], value[0])
        super(ParameterDict, self).__setitem__(attr, param)

    def update(self, *args, **kwargs):
        update_dict = dict(*args, **kwargs)
        super(ParameterDict, self).update({k: convert_parameter(v[1], v[0]) for k, v in update_dict.items()})


class Region:
    __regions_by_group = {}
    __regions_by_meshed_as = {}

    @classmethod
    def meshed_as(cls, meshed_as):
        if meshed_as in cls.__regions_by_meshed_as:
            return cls.__regions_by_meshed_as[cls]
        return None

    @classmethod
    def zone(cls, idx):
        return cls.__regions_by_meshed_as["zone"][idx]

    @classmethod
    def surface(cls, idx):
        return cls.__regions_by_meshed_as["surface"][idx]

    @classmethod
    def group(cls, group):
        if group in cls.__regions_by_group:
            return cls.__regions_by_group[group]
        return None

    @classmethod
    def add_region_to_group(cls, group, region):
        if group not in cls.__regions_by_group:
            cls.__regions_by_group[group] = []
        cls.__regions_by_group[group].append(region)

    @classmethod
    def add_region_to_meshed_as(cls, meshed_as, region):
        if meshed_as not in cls.__regions_by_meshed_as:
            cls.__regions_by_meshed_as[meshed_as] = {}
        cls.__regions_by_meshed_as[meshed_as][region.idx] = region

    def am(self, group):
        return group in self.groups

    def __init__(self, region_dict):
        self.idx = region_dict['id']
        self.groups = region_dict['groups']
        self.filename = region_dict['filename']

        for group in self.groups:
            self.add_region_to_group(group, self)

        if 'meshed_as' in region_dict:
            self.meshed_as = region_dict['meshed_as']
            self.add_region_to_meshed_as(self.meshed_as, self)
        else:
            self.meshed_as = None


R = AttributeDict({k: Region(v) for k, v in region_dict.items()})
R.group = Region.group
R.meshed_as = Region.meshed_as
R.zone = Region.zone
R.surface = Region.surface


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
            print("UNCASTABLE", parameter, cast)
            pass

    if try_json:
        try:
            return json.loads(parameter)
        except:
            pass

    return parameter


P = ParameterDict()
P.update(parameter_dict)
NP = {k: ParameterDict(v) for k, v in needle_parameter_dicts.items()}
