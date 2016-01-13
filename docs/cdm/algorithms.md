# Go-Smart Simulation Algorithms

These are members of the [CDM](overview.md) that provide something akin to lambda
calculus to Go-Smart. They are database defined, have textual body in an
unspecified language, return a parameter as a *Result* and take named arguments
that must be supplied by the numerical model as a condition of forming a valid
*Combination*.

## Definition

They can be defined in [GSSA-XML](../gssa-xml.md), within the `<algorithms />` node
as follows:

```xml
    <algorithm result="RESULT">
        <arguments>
            <argument name="ARGNAME" />
        </arguments>
        <content>
            ALGORITHMDEFINITION
        </content>
    </algorithm>
```

The `ALGORITHMDEFINITION` may be defined in any language the simulation family
will understand (or, more accurately, the ultimate third-party simulation tool).
Examples include MATC (for [GSSF](../gssf/overview.md)) and Python (for
[FEniCS](../docker/fenics.md)). The `RESULT` is conventionally an upper-case,
underscore-spaced slug that can be used in place of a conventional "constant"
parameter. Arguments are specified solely by name, not type (as we do not even
know the applicable language). They are expected to be supplied by the
simulation procedure at run-time.

While this is no less secure than the numerical
model running on the Docker instance from an administrator perspective, it should
be noted that the provenance of
the algorithm may well be different, and if the author of the numerical model (or
Docker instance) is
concerned about data leakage from their instance, they should take adequate
precaution. Within the Python Docker module, the algorithm will not be called
except as requested by the numerical module.
