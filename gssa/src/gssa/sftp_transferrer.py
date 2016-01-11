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

from zope.interface import implementer
import paramiko

import os

from .transferrer import ITransferrer


@implementer(ITransferrer)
class SFTPTransferrer:
    def __init__(self):
        self._ssh_client = paramiko.SSHClient()
        self._ssh_client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        self._sftp_client = None

    def connect(self):
        pkey = paramiko.RSAKey.from_private_key_file(self._key_file)
        self._ssh_client.connect(hostname=self._host, port=self._port, username='dummy', password='dummy', pkey=pkey)
        self._sftp_client = self._ssh_client.open_sftp()

    def disconnect(self):
        self._sftp_client.close()

    def pull_files(self, files, root, remote_root):
        if self._sftp_client is None:
            raise RuntimeError("Must connect to SFTP on host first")

        for local, remote in files.items():
            absolute_path = os.path.join(root, local)
            remote_absolute_path = os.path.join(remote_root, remote)
            try:
                self._sftp_client.get(remote_absolute_path, absolute_path)
            except FileNotFoundError as e:
                print("Could not transfer %s on SFTP to %s locally - not found" % (remote_absolute_path, absolute_path))
                raise e

    def push_files(self, files, root, remote_root):
        if self._sftp_client is None:
            raise RuntimeError("Must connect to SFTP on host first")

        for local, remote in files.items():
            absolute_path = os.path.join(root, local)
            remote_absolute_path = os.path.join(remote_root, remote)
            print("Putting", absolute_path, remote_absolute_path)
            self._sftp_client.put(absolute_path, remote_absolute_path)

    def configure_from_xml(self, xml):
        self._host = xml.find("host").text
        self._port = xml.find("port").text
        self._key_file = xml.find("keyFile").text
