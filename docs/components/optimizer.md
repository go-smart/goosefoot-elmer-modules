# Optimizer GSSF Component

This is an extremely basic component, wrapping [GMSH](http://gmsh.info) for mesh optimization.

## Configuration

The second-level [GSSF-XML](../xml.md) element is as follows:

```xml
   <optimizer [ method="(gmsh|netgen):gmsh" ] />
```

The `method` corresponds to either the `-optimize` or `-optimize_netgen`
optimization flag of GMSH.
