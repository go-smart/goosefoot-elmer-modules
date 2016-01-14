# NumaTM Module

Produces `libnuma-tm.so` module.

# NumaTMHarmonicSolver

Minorly adjusted version of the upstream Elmer MagnetoDynamics2DHarmonic solver.
(TODO: is this still necessary?)

# Transform

Routine to transform a solution to somewhere else in 3D. Used in conjunction
with 2D-&gt;3D rotational extrapolation enhancements to embed axisymmetric problems.

Parameter | Location | Type | Default | Description
----------|----------|------|---------|--
Backward | Solver | Logical | FALSE | Reverse the transformation
Transformation Matrix | Solver | Real Array (3x3) | - | Matrix representing the linear part of the transformation
Translation | Solver | Real Array (3) | - | Offset representing the affine part of the transformation
Interpolant | Solver | String | - | Name of variable that should be interpolated to new location
