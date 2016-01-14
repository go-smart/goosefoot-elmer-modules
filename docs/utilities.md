# Utility Functions in GSSF

There are several helper functions used in GSSF that may be of use if you are
extending or modifying behaviour. While these are underscore prefixed, this may
be changed later - however, they are intended for use within the gssf.*
space.

## generate_rotation_matrix

*Location*: `gssf.globals._generate_rotation_matrix(x, y, z, backward=False)`,
`_generate_numpy_rotation_matrix(x, y, z, backward=False, rx=0, ry=1, rz=0)`

The [Numpy](http://www.numpy.org/) version of this function creates a rotation
[matrix](http://docs.scipy.org/doc/numpy/reference/generated/numpy.matrix.html)
taking the vector `(rx, ry, rz)` to `(x, y, z)`, rotating about an axis
perpendicular to both. The `backward` argument allows for easy reversal of the
rotation. The algorithm used is [Rodrigues' rotation
formula](https://en.wikipedia.org/wiki/Rodrigues'_rotation_formula#Matrix_notation).
The non-Numpy version renders the matrix as a SIF-embeddedable space-separated
list.

This utility is especially useful for translating needles from reference
coordinates to the embedded location and for moving backwards and forwards between
an axisymmetric problem and its 3D embedding. Note that reference needles in the
GSSF library are aligned to `(0, 0, 1)`, not `(0, 1, 0)`. However, library
axisymmetric meshes lie in this plane. [Extensions](tools/numa-elmer.md) to the mesh
interpolation functionality in Elmer allow

## slugify

*Location*: `gssf.globals.slugify(inp)`

This is a workaround for lack of a Python3 slugify early in the project, and is
to be replaced with a real slugify (e.g.
[python-slugify](https://pypi.python.org/pypi/python-slugify)). It is documented
here solely because any differences between our (very basic, 4-line) behaviour
and an upstream slugify's needs to be considered before swapping.

It uppercases input, removes any parentheses and replaces dash and space with an
underscore. For example:

    "Electric conductivity (tissue)" ----> ELECTRIC_CONDUCTIVITY_TISSUE

This is normally seen in generation of constant names for the parameter
dictionaries in the SIF template.
