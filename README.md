GO-SMART SIMULATION ARCHITECTURE
====================================================================
NUMA ENGINEERING SERVICES LTD (NUMA)
Dundalk, Ireland

This project is co-funded by: European Commission under grant agreement no. 600641.
http://www.gosmart-project.eu/

This tool provides the necessary scripts and libraries to run the Go-Smart server and architecture.

DEPENDENCIES
------------

* Python 2.7
* Python 3
* Elmer (with NUMA modifications)
* Crossbar.io
* GMSH
* VTK 5.8
* libjsoncpp-dev
* (Python 3) munkres pyyaml crossbar docker
* (Python 2) hachiko paramiko

INSTALLATION
------------

CMake installation is recommended from an out-of-source build directory.

USAGE
-----

The simulation server (GSSA) may be launched by the command

  crossbar --debug start --cbdir path/to/directory(web)[default gssf-release/web]
  go-smart-simulation-server --host HOSTADDRESS/localhost --websocket-port PORTNUMBER
  go-smart-simulation-client --gssa-file XMLFILE --websocket-port PORTNUMBER --host HOSTADDRESS/localhost --definitions path/to/file.py --skip-clean --output Lesion.vtp

Adding --help will show documentation of command line arguments. You should start Crossbar.io in the gssf/web directory of the build folder before launching this script. Ensure that the configuration in the web directory matches the port and host to which go-smart-simulation-server will connect for WAMP interaction.
