# NumaPowerField Module

Produces `libnuma-powerfield.so` module.

## NumaPowerFieldTrigger

This solver relates time to a sequence of *phases* with different powers. Each
power should correspond to an pre-defined input field. When the phase changes,
this solver updates the solver loading the field data with the new filename and
instructs it to recalculate. This is intended to work with NumaPowerFieldSolver.

This sets a Phase variable, a sequential integer indicating the present phase, a
Power variable, indicating the total power for this phase, and a RecalculatePower
flag variable (positive when set) to tell the data solver to reload its
data. This works well with a DataToFieldSolver when the Exec Condition for that
solver is set to be this solvers RecalculatePower variable, as it updates the
Point Data Filename property.

Parameter | Location | Type | Default | Description
----------|----------|------|---------|--
Time | - | Variable | - |
Phases | Solver | Real Array (2xN) | - | An ordered row of final times for each phase over a row of new powers at those times. If the timestep exceeds the final phase end, or a negative power is encountered, the simulation exits
Data Solver | Solver | Integer | - | The index of the solver which should have the Point Data Filename property set on it
Profile File Prefix | Solver | String | `sar-` | Prefix used to obtain the deposition profile (in the form "sar-PPPP.dat", say)

**TODO**: make the data solver optional
