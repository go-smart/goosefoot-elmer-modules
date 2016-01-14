# NumaIRE Module

This is a macro-scale IRE solver based on solving Laplace's equation with a
deposition-dependent electric conductivity (using a similar approach to e.g.
[Garcia et al, 2014](http://lbk.fe.uni-lj.si/pdfs/plos2014.pdf)).

The IRE protocol is formed of a sequence of pairings of *in situ* needles. For
instance, six needles may be attached to the generator and placed in the liver,
preferably to be parallel. The protocol may be, say, nine consecutive pairings
of needles with one as acting an anode and another as a cathode. The remainder
are inert for that protocol step.

As each step is treated as a steady-state problem, we use timestepping to move
from one pair to the next - in the example above, we use nine timesteps.

### Global Parameters

Parameter | Location | Type | Default | Description
----------|----------|------|---------|--
Anode     | Simulation | Integer Array (size N) | - | This is a list of boundary IDs, one for each timestep, indicating the erstwhile anode boundary (req)
Cathode   | Simulation | Integer Array (size N) | - | This is a list of boundary IDs, one for each timestep, indicating the erstwhile cathode boundary (req)
Potential Consecutive Values   | Simulation | Real Array (size 2xN) | - | The first row lists potential values on the anode, the second those on the cathode

The Anode, Cathode and Potential Consecutive Values arrays must be the same
length, with the PCV array having two rows.

## AlternatingBCSolver (libnuma-ire)

This is an upstream version of [NumaAlternating](numa-alternating.md), working with two
lists of embedded boundaries, switching between a Dirichlet condition and no
condition at all. It searches for boundary conditions with an `Alternating Boundary
Condition` parameter set to `TRUE` and turns them on when the BC's `Body Id`
matches the current Anode or Cathode value.

## AlternatingBoundaryCondition

This is a user-defined function that returns the potential Dirichlet condition
value based on whether the boundary element to which it is being applied is an
anode or a cathode (zero otherwise, but when used in conjunction with the
AlternatingBCSolver, this will be ignored).

## CoverageCurveOutputSolver

The coverage curve output solver calculates, for each energy deposition level
$ E $, the cumulative volume of tumour which has experienced maximum
deposition above that level. This is a common way of reporting IRE efficacy as,
for a chosen death threshold, the lesion size can be read off a plot.

At each timestep, it outputs a file, "coverage_TTT.txt", where TTT is the
timestep. Each line has the format: `Threshold (V/cm), Fraction of tumour volume`.
This solver contains a simple example of succinctly integrating a function over
a domain, which may be a useful starting point for other authors.

Parameter | Location | Type | Default | Description
----------|----------|------|---------|--
E | - | Variable | - | Most recent energy deposition at a point (req)
Max E | - | Variable | - | Maximum energy deposition at a point (req)
Minimum Coverage | Solver | Real | - | Lowest energy level to calculate (req)
Maximum Coverage | Solver | Real | - | Higher energy level to calculate (req)
Divisions | Solver | Integer | - | Number of energy levels (req)
Tumour | Material | Logical | FALSE | Whether or not this cell should be counted as tumour tissues, that is, whether it contributes to the totals

## MaxESolver

Provides the maximum energy deposition over previous time as a variable (MaxE), along
with energy deposition at present time (E), electric conductivity based on
energy deposition (Electric Conductivity) and survival, based on the [Garcia et
al, 2014](http://lbk.fe.uni-lj.si/pdfs/plos2014.pdf) model relating pulse number
to death threshold. Note that the constants here are not based on in vivo human
studies, so improved values should be used when available.

Parameter | Location | Type | Default | Description
----------|----------|------|---------|--
Joule Heating | - | Variable | - | Output Joule Heating of, say, Elmer's electric potential solver
Pulse Number | Solver | Integer | - | Number of pulses at each protocol step (req)
E0 | Solver | Real | 399600.0 | Modelling parameter in Garcia et al, 2014
A0 | Solver | Real | 144100.0 | Modelling parameter in Garcia et al, 2014
K1 | Solver | Real | 0.03 | Modelling parameter in Garcia et al, 2014
K2 | Solver | Real | 0.06 | Modelling parameter in Garcia et al, 2014
