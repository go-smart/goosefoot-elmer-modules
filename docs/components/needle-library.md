# Go-Smart Needle Library - STEP manipulation and geometry generation

This tool uses [PythonOCC](http://www.pythonocc.org/) to generate triangulated
surfaces from STEP files, for representing needles and other features. It uses
PythonOCC also for geometric transformation, allowing a single reference STEP to
be placed into a mesh in any location or of any scaling.

With the appropriate GSSA-XML extensions, this will allow a user to provide
their own STEP geometry as the definition of a surface or surfaces in the mesh.
The present implementation provides some support for splitting an STEP file,
with the first listed component being output as an 'active' part, the second as
an 'inactive' part, both in STL format.

## go-smart-needle-library

Management tool for Go-Smart ablation needle geometry library

    go-smart-needle-library [-h] [--logfile LOGFILENAME] [--logfile-addpid]
                               [--silent] [--debug] [--output OUTFILE]
                               [--output-extent EXTENTFILE]
                               ...

### Positional arguments

Argument            | Description
------------------- | ------------------
  configfilenames   |   Locations of configuration file (latter override former)

### Optional arguments

Argument                      | Description
----------------------------- | ------------------
  -h, --help                  | show this help message and exit
  --logfile LOGFILENAME       | name of the log file
  --logfile-addpid            | whether the PID should be appended to the given
                              | logfile name
  --silent                    | prevent wrapper output
  --debug                     | additional, debug output (overridden by --silent)
  --output OUTFILE            | destination STL file for needle
  --output-extent EXTENTFILE  | destination STL file for extent

## Needle CAD geometries

These should lie along the z-axis, with tip at the origin and be in STEP format.
One or two roots (shapes) may exist. If two, the STEP will be split into two STL
surfaces, with the first marked as the 'active' surface, the second as the
'inactive'. Interpretation is left to the modelling components of the workflow.
If multiple shapes are provided, they should be output in STLs suffixed by the
ordinal position in the STEP (1 &rarr; n). This should work for n &gt; 2 but has
not been fully tested.

## GSSF component

The corresponding GSSF component forms a fairly transparent wrapper for
[go-smart-needle-library](#go-smart-needle-library).

### Configuration

The second-level [GSSF-XML](../xml.md) tag is as follows:

```xml
    <needlelibrary [ skip="BOOL:false" ]>
        <needle id="ID" [ name="NAME" ] [ axis="AX AY AZ" ] [ offset="OX OY OZ" ]/>
      [ <target x="TX" y="TY" z="TZ" /> ]
      [ <extent x="TX" y="TY" z="TZ" /> ]
    </needlelibrary>
```

The `id` is the library name of the required template. It is recommended to
write this as `id="stock:ID"` for future compatibility. The name is a human
readable name to use to refer to this needle elsewhere in the simulation. If not
provided, the ordinal index of the `needle` node will be used. The `axis`
overrides the global `axis` from the `geometry` section of [GSSF-XML][xml.md]
for this needle. The `offset` indicates the offset of this needle from the
target of the needle-set, by default the geometry centre.

The `target` allows the user to specify an offset of the needle-set from the
centre of simulation as defined in the `geometry` section.
The `extent`, if provided, instructs the `needlelibrary` to generate an STL
simulation boundary. This may be intersected later with the organ boundary to
generate an outer boundary of the simulation domain.
