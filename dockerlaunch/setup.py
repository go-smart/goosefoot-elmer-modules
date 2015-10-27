from distutils.core import setup
from distutils.command.install import install
import subprocess


class WrappedInstall(install):
    def run(self):
        install.run(self)
        subprocess.call(['./create-unix.sh'])

setup(
    name='docker-launch',
    version='0.1',
    scripts=['docker-launch'],
    cmdclass={'install': WrappedInstall}
)
