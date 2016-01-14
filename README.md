Go-Smart Simulation Architecture (GSSA)
=======================================

Primary authors: [NUMA Engineering Services Ltd](http://www.numa.ie) (NUMA), Dundalk, Ireland

This project is co-funded by: European Commission under grant agreement no. 600641.

Project website: http://www.gosmart-project.eu/

This tool provides the necessary scripts and libraries to run the Go-Smart server and architecture.

Dependencies
------------

* Python 2.7
* Python 3
* [Elmer (with NUMA modifications)](https://github.com/go-smart/gssf-elmer)
* GMSH
* VTK 5.8
* libjsoncpp-dev
* (Python 3) munkres pyyaml
* (Python 2) PythonOCC

Documentation
-------------

Documentation for this component is available at https://go-smart.github.io/gssf

Installation
------------

CMake installation is recommended from an out-of-source build directory.

Usage
-----

The simulation workflow may be launched by the command

```sh
  go-smart-launcher settings.xml
```

where `settings.xml` is a GSSF-XML file. Adding --help will show documentation of command line arguments.
