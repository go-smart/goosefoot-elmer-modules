from __future__ import print_function

from zope.interface import implementer
import shutil

import os

from gosmart.server.transferrer import ITransferrer


@implementer(ITransferrer)
class TmpTransferrer:
    def __init__(self, host, port, key_file):
        pass

    def connect(self):
        pass

    def disconnect(self):
        pass

    def pull_files(self, files, root, remote_root):
        for local, remote in files.items():
            absolute_path = os.path.join(root, local)
            remote_absolute_path = os.path.join(remote_root, remote)
            try:
                shutil.copy(os.path.join('/tmp', remote_absolute_path), absolute_path)
            except FileNotFoundError as e:
                print("Could not transfer %s on /tmp to %s - not found" % (remote_absolute_path, absolute_path))
                raise e

    def push_files(self, files, root, remote_root):
        for local, remote in files.items():
            absolute_path = os.path.join(root, local)
            remote_absolute_path = os.path.join(remote_root, remote)
            print("Putting", absolute_path, remote_absolute_path)
            shutil.copy(absolute_path, os.path.join('/tmp', remote_absolute_path))
