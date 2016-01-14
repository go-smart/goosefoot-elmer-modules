# NumaProgress Module

Produces the `libnuma-progress.so` module.

## NumaProgressSolver

This solver connects to a socket called `percentage.sock` and sends messages of
the form:

    PERCT||Elmer in progress: TIME

where PERCT is the percentage progress and TIME is the current time variable.

Parameter | Location | Type | Default | Description
----------|----------|------|---------|--
Time | - | Variable | - |
Percentage Progress | Solver | Real | - | Use this to set the progress to be reported, via, say, a MATC function
