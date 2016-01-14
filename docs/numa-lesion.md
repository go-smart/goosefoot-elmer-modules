# NumaLesion Module

Produces `libnuma-lesion.so` module.

## LesionExtract

Returns the lowest temperature pointwise over time. It should be used as a body force UDF. This allows
an isovolume to be taken at the last timestep that includes any cell passing a
lethal point at any point in time. Primarily useful for cryoablation.

Parameter | Location | Type | Default | Description
----------|----------|------|---------|--
LesionPrevious | - | Variable | - | Stores previous value of this variable
Freezable | Material | Logical | TRUE | If a material is explicitly marked unfreezable, the return value will the max of its temperature and 0C (in K)

TODO: refactor
