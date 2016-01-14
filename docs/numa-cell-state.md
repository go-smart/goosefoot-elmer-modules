# NumaCellState Module

This is an implementation of a [three-state cell death
model](http://www.ncbi.nlm.nih.gov/pubmed/20924678), an evolution equation with
Alive, Vulnerable and Dead states. All three transition to the next, based on
temperature, while Vulnerable is additionally able to transition to Alive.

This builds to `libnuma-cellstate.so`.

## NumaCellStateSolver

As no spatial derivatives are involved, this solver does not require an FE
matrix solution. Instead it uses RK4 timestepping with the secant method for
obtaining an iterative series of approximations.

### Parameters

Parameter | Location | Type | Default | Description
----------|----------|------|---------|--
Temperature | - | Variable | - | Used for temperature-dependent coefficients (req)
Time | - | Variable | - | Simulation time (req)
Nonlinear System Max Iterations | Solver | Integer | 1 | Maximum internal iterations
Nonlinear System Abort Not Converged | Solver | Logical | TRUE | Stop if not converged
Nonlinear System Convergence Tolerance | Solver | Real | 3.0 | Stop when error is below this
Nonlinear System Relaxation Factor | Solver | Real | 1.0 | Relaxation factor
Forward Rate | Solver | Real | 0.00333 | $ k_f $
Backward Rate | Solver | Real | 0.00777 | $ k_b $
Exponential Rate | Solver | Real | 40.5 | $ T_k $
Enforce Variable Bounds | Solver | Logical | FALSE | Max/min pointwise values at theoretical max/min (0.0/1.0)
