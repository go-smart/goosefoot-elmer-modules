# Go-Smart Simulation Framework Family in GSSA

GSSA [families](https://go-smart.github.io/gssa/families/) prepare configuration for running a solver, in
this case GSSF. This is in the form of a Python class extending
`gssa.family.Family`. There are actually two components comprising the GSSF
Family.

## Elmer-libnuma

This refers to Elmer run in conjunction with some
[NUMA](http://www.numa.ie)-written libraries, and more generally to GSSF, which
is centred around this pairing.

## MesherGSSFMixin

This is a mixin, used in GSSF and elsewhere for setting up a volumetric mesh
using [GSSF-XML](xml.md). In the Elmer-libnuma case, it only prepares the
relevant part of the XML file, which is completed and run by the Elmer-libnuma
family. In other cases, the volumetric entries only are used and run by
[go-smart-launcher](go-smart-launcher.md), before whichever component into which
this is mixed in takes the output MSH file for its own simulation.

### Parameters used

Parameter name | Description (effect on output GSSF-XML)
---------------|--------------------------------
`NEEDLE_TIP_LOCATION` | Used to find the offset for each needle. If the `CENTRE_LOCATION` has not been provided, this is used to provide the `geometry` centre
`NEEDLE_ENTRY_LOCATION` | With each needle tip, this is used to find the needle axis and the first needle is used to provide the `geometry` section's `needleaxis`
`NEEDLE_ACTIVE_LENGTH` | Size of active length sphere for specific needle, as used in the `active` section in the relevant `mesher` section `needle`
`SETTING_SOLID_NEEDLES` | Indicates that all needle geometries should be meshed solidly, prompting the `zones` attribute of `needlelibrary` and `zone_boundaries` of `mesher` to be set
`CONSTANT_GLOBAL_ACTIVE_LENGTH` | Size of default active length, to be used in the `active` section in the relevant `mesher` section `needle`
`CENTRE_LOCATION` | Default `geometry` centre in GSSF-XML. May be a JSON float triple or `first-needle` (to use tip of first needle) or `centroid-of-tips` to calculate the combined centre of all needle tips
`CENTRE_OFFSET` | This adds an offset to the `geometry` centre, without altering the needle location
`SIMULATION_SCALING` | Becomes the `geometry` section `simulationscaling`
`SETTING_ORGAN_AS_SUBDOMAIN` | Switches the `organ` from an `organ` in the `mesher` to a `zone`
`SETTING_AXISYMMETRIC_INNER` | Adds an `inner` to the `mesher` of the given template
`SETTING_AXISYMMETRIC_INNER_COARSE` | Adds a coarse `inner` to the `mesher` of the given template
`SIMULATION_DOMAIN_RADIUS` | Gives a bounding radius for use in the `mesher` section `extent`
`RESOLUTION_HIGH` | Triggers an approximately double resolution set of characteristic length parameters compared to the default
`RESOLUTION_FIELD_NEAR`, `RESOLUTION_FIELD_FAR`, `RESOLUTION_FIELD_ZONE`, `RESOLUTION_FIELD_NEEDLE_ZONE` | Override specific characteristic length field parameters in the `mesher` section
