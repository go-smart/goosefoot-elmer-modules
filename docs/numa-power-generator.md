# NumaPowerGenerator Module

Produces `libnuma-powergenerator.so` module. This module is used to approximate
the behaviour of an radiofrequency ablation power generator, which takes
readings from several thermocouples and distributes power to a series of tines,
spreading out from the end of a percutaneous needle.

## NumaPowerGeneratorSolver

Updates the power field based on the present phase. If the phase changes, the
distribution field profile is recalculated. If the power is negative, the simulation
gracefully exits. If the overall input power is controlled by a PID controller,
this is also taken into account and calculated. Output variables are Electric
Distribution (normalized field), ObservedTemperature (from the thermocouples),
Impedance (FIXME: requires work), Applied Power (post-PID total output) and
Phase. All but the first are global (0D) variables.

Parameter | Location | Type | Default | Description
----------|----------|------|---------|--
Temperature | - | Variable | - |
Present Phase | Solver | Integer | - | Sets the phase for the present timestep (req)
Thermocouple Cut-Off Temperature | Solver | Real | - | Tines controlled by this thermocouple are shut off when its reading exceeds this temperature
Thermocouple Cut-Off Temperature | Solver | Real | - | Tines controlled by this thermocouple are reactivated (if they were shut off) when its reading drops below this temperature
Electric Power | Solver | Real | - | Power factor to product with distribution field profile
Impedance Voltage | Solver | Real | - | Voltage used to calculate impedance
Temperature Controlled Electric Power | Solver | Logical | FALSE | Whether to base power delivery on a PID controller
Target Temperature | Solver | Real | - | Used by the PID controller
Proportional Gain for Electric Power Control | Solver | Real | 0.0 | Used by the PID controller
Derivative Gain for Electric Power Control | Solver | Real | 0.0 | Used by the PID controller
Integral Gain for Electric Power Control | Solver | Real | 0.0 | Used by the PID controller
Integral Length for Electric Power Control | Solver | Real | 0.0 | Used by the PID controller
Electric Power Filename | Simulation | String | - | Name of the file containing the power distribution between tines
