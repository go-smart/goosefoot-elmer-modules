# NumaAlternating Module

This builds to `libnuma-alternatebc.so`.

## NumaAlternatingBCSolver

This solver switches boundary conditions between Dirichlet and Neumann based on
a `Alternating Boundary Condition` 0D value in each `BoundaryCondition`. When it is found, the
solver checks if is positive and sets the Dirichlet flag for its elements, when
not, it removes the Dirichlet flag.

This is primarily useful when `Alternating Boundary Condition` is, for example, a variable of
time or another 0D variable (total power, etc.). For this to work correctly, all
boundary conditions intended to alternate should have both Dirichlet and Neumann
conditions set.
