# Go-Smart Simulation Architecture XML Format (GSSA-XML)

**Note**: this is distinct from [GSSF-XML](gssf/xml.md).

GSSA-XML is documented as an XML Schema in the `schema` directory of the source
tree. However, to provide a human-readable resource, it is described also here:

```xml
<simulationDefinition>
    <transferrer class="(http|tmp)">
        <!-- See Transferrers documentation -->
    </transferrer>
    <algorithms>
        <!-- See Algorithms documentation -->
    </algorithms>
    <parameters>
        <!-- See Parameters#concrete-parameters documentation -->
    </parameters>
    <numericalModel>
        <needles>
            <needle index="NIX" class="NCL" file="NFILE">
                <parameters>
                    <!-- See Parameters#concrete-parameters documentation -->
                </parameters>
            </needle>
            ...
        </needles>
        <regions>
            <region id="RID" name="RNAME" format="RFORMAT:(zone|surface|both)" input="RINPUT" groups="RGROUPS" />
            ...
        </regions>
        <definition name="DFAMILY" [ file="DFILE" ]>
            <!-- Family-specific content (see documentation for each family) -->
        </definition>
    </numericalModel>
</simulationDefinition>
```

The needle index, `NIX`, indicates the name by which this needle should be
referred to. They should be integers, ordering the needles contiguously. The
class, `NCL`, identifies how a needle should be incorporated into the simulation
and should either be `solid-boundary` or `boundary`. The exact needle definition
is supplied by `NFILE`, which should be of the form `library:LIBRARYTYPE` or
`file:INPUTFILENAME.stl`. Within each needle node is another list of
needle-specific parameters.

Each region should have an `RID` that uniquely refers to them within the
simulation. The `RNAME` is a context-providing name that may be used, such as
`organ` or `tumour`. A simple way to generate IDs is to add an ordinal index to
the name, e.g., `tumour-1`. The `RFORMAT` indicates how the region should be
treated, as a zone, surface or both (volumetric subdomain, boundary, or embedded
boundary). The `RINPUT` is the filename of the STL or VTP input file defining the
surface. The collection of region groups of which this region is a member should
be specified in the `RGROUPS` field, joined by "`; `".

The definition `DFAMILY` selects the family that should handle the definition.
Note that it may be provided as a TAR.GZ (allowing, for instance, Python
modules) using the `DFILE` attribute, but is otherwise the body of the
`definition` tag itself.
