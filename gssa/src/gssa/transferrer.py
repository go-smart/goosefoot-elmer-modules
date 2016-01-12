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
import zope.interface


class ITransferrer(zope.interface.Interface):
    """Transfers files to simulation directory"""

    def __init__():
        """Initialize"""

    def disconnect():
        """Disconnect from existing connection"""

    def connect():
        """Initiate any necessary connection and wait for instructions"""

    def push_files(files, root, remote_root):
        """Push files to remote based on the file map. files should be dictionary mapping local paths (based at root) to remotes"""

    def pull_files(files, root, remote_root):
        """Pull files from remote based on the file map. files should be dictionary mapping local paths (based at root) to remotes"""

    def configure_from_xml(xml):
        """Configure this transferrer using the transferrer node of a GSSA-XML input"""


from .http_transferrer import HTTPTransferrer
from .sftp_transferrer import SFTPTransferrer
from .tmp_transferrer import TmpTransferrer


# Transferrers must be manually registered
transferrer_register = {
    "http": HTTPTransferrer,
    "sftp": SFTPTransferrer,
    "tmp": TmpTransferrer
}
