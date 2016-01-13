# Go-Smart Simulation Architecture - FEniCS Family

This is a family within the [Docker+CGAL
workflow](overview.md#dockercgal-workflow), incorporating meshing from
[mesher-cgal](../tools/mesher-cgal.md) and simulation provided by
[FEniCS](http://fenicsproject.org/). FEniCS is a general-purpose finite element
framework, capable of automated solution of PDEs.

The
[dolfin-convert](https://github.com/FEniCS/dolfin/blob/master/scripts/dolfin-convert/dolfin-convert)
script is used to translate the volumetric [MSH](http://gmsh.info) into
DOLFIN-XML, for direct import into the Python script. The reference upstream Docker
image for this is
[fenicsproject/stable-ppa](https://bitbucket.org/fenics-project/docker).

## Definition

The definition used for this family should be one or a series of Python files,
containing a `start.py`. This has access to all the FEniCS dependencies,
including many core scientific Python libraries.
