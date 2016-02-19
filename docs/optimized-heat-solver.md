# OptimizedHeatSolver Module

Produces the `libnuma-eheatsolver.so` module. This is a refactoring of the upstream
HeatSolver module into separate much more manageable routines. It also adds
support for [cell-death](numa-cell-state.md) dependent parameters.

## HeatSolve

Only differences from the upstream HeatSolve are documented here. The cell death
extensions include new parameters for coagulation (cell-death based power deposition
adjustment) and vapourization (temperature-based power deposition adjustment).
Perfusion rate is also variable based on a cell-death status. Can output a Heat
Source variable that shows the profile going into the Pennes
equation.

Code
improvements include splitting routines between files to ensure namespacing,
proper declarations for routines, ensuring only used variables are allocated,
consistent capitalization, ensuring only used variables are declared.

**TODO**: this code included initial work on assembling only cells that had changed (e.g. cell-death status). This needs to be finished.

Parameter | Location | Type | Default | Description
----------|----------|------|---------|--
Coagulation Cut Off | Solver | Real | - | Level over which cell-death value indicates no local heating
Coagulation Gradient | Solver | Real | - | Slope backward from cell-death cut off as a coefficient for local heating (slope stops when it hits x1)
Coagulation Minimum Deposition | Solver | Real | - | Level below which cell-death-based coefficient cannot drop
Vapourization Cut Off | Solver | Real | - | Level over which temperature value indicates no local heating
Vapourization Gradient | Solver | Real | - | Slope backward from temperature cut off as a coefficient for local heating (slope stops when it hits x1)
Vapourization Minimum Deposition | Solver | Real | - | Level below which temperature-based coefficient cannot drop
Dead Threshold | Solver | Real | 0.8 | Value above which a cell is considered dead. This is separate from coagulation and vapourization.
Death Perfusion Rate | Solver | Real | 0.0 | Perfusion rate for cells identified as dead
Death Heat Capacity | Solver | Real | 670.0 | Heat capacity for cells identified as dead
Heat Source Visualization | Solver | Logical | FALSE | Calculate a Heat Source variable showing the final, combined heating
