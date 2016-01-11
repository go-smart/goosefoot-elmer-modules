# Regions and Geometry in GSSF

## Geometry

A specific simulation workflow has certain overarching geometrical information.
These are provided in the `<geometry>` section of the [GSSF-XML](xml.md) file.
In particular, there are:

Property (Type) | Entry in `geometry` dictionary | Description
-|-|-
Centre *(x, y, z)* | `centre` | Focal point of the simulation. Often a needle tip. Other locations generally expressed as an offset from this location. Default centre of simulation extent
Needle axis *(x, y, z)* | `needleaxis` | Primary/default axis for the simulation. May or may not be an actual axis of a needle, but will be used as a fallback if a needle axis is not provided.
Simulation scaling factor *(r)* | `simulationscaling` | The (multiplicative) ratio going from input surface length scales (e.g. mm) to simulation length scales (e.g. m)

These may be referred to, and assumed to exist, in any component.

## Regions
Regions are geometric entities. They may be either 2D subdomains (boundary
subdomains) or 3D subdomains. They are specified in the [GSSF-XML](xml.md) by
one of the three tags:

```xml
    <surface name="REGIONNAME" input="INPUTLOCATION" groups="GROUP1; GROUP2;..." />
    <zone name="REGIONNAME" input="INPUTLOCATION" groups="GROUP1; GROUP2;..." />
    <both name="REGIONNAME" input="INPUTLOCATION" groups="GROUP1; GROUP2;..." />
```

The `REGIONNAME` may be any string used to refer to the region elsewhere in the
configuration (such as the [mesher](meshing.md)) and the `INPUTLOCATION` is
usually the filename of the STL surface defining the region. This may be given
relative to the working directory. Generally, STL files are kept in an `input/`
subdirectory.

The [Needle Library](needle-library.md) may add regions representing needles, as
`NEEDLE_[NEEDLEID]` (and `NEEDLE_[NEEDLEID]_ACTIVE`,
`NEEDLE_[NEEDLEID]_INACTIVE` if appropriate).

### Labelling

Regions will be labelled by integers in the final mesh, which are used in the
Elmer SIF file to assign properties, boundary conditions and so forth. The
relationship between labels and regions is hard to predict if possible at all,
as it is dependent on the volumetric meshing, number of subdomains appearing in
the simulation domain and ordering of assignment.

As such, the [parameter dictionary](constants.md) accessible in the SIF template
contains region entries. For example:

    <regions>
      <surface name="organ" input... />
      ...

is used in the SIF template as follows:

    !===================
    Boundary Condition 3
    !===================
    Target Boundary = Integer {{ p.REGION_ORGAN }}
    Temperature = {{ p.CONSTANT_BODY_TEMPERATURE|typed }}
    ...

The conversion from region name to corresponding constant name uses
[slugify](utilities.md#slugify).

#### Groups

As several regions can all effectively represent the same subdomain, or related
subdomains, *groups* allow you to indicate this in GSSF-XML. This is by far the
more common way to include mesh indexes in the SIF template, as it future proofs
against multiple related region entries, even if you currently only use one.

Groups are notated, for example:

    <surface name="organ" input="input/organ.stl" groups="organs; outer-surfaces" />

In this case, entries `BOUNDARIES_ORGANS` and `BOUNDARIES_OUTER_SURFACES` will
be created in the global parameter dictionary. These actually include the whole
target boundary line in the SIF file, so if they are not present, the line is
omitted and no syntax error occurs.

    !===================
    Boundary Condition 3
    !===================
    {{ p.BOUNDARIES_ORGANS }}
    Temperature = {{ p.CONSTANT_BODY_TEMPERATURE|typed }}
    ...

Similarly, this is true for the needles in their parameter dictionaries, with
`needles[id].BOUNDARIES_NEEDLE_ACTIVE` and `needles[id].BOUNDARIES_NEEDLE_INACTIVE`.
