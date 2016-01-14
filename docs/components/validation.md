# Go-Smart Simulation Framework - Validation Component

This component manages the validation step, comparing the output STL ablation
lesion surface and a separately provided segmented validation STL surface.

**Note**: this component is configured to wrap the (closed source) Aalto
Validation Tool. For further details, please contact [Aalto
University](http://becs.aalto.fi/en/personnel/staff/pollari_mika.html).

## Configuration

This tool is configured as follows:

```xml
    <validation reference="REFERENCEREGION" [ registration="BOOL:true" ] />
```

The `REFERENCEREGION` should be defined  in the `regions` section of
[GSSF-XML](../xml.md) and provides the surface to which the lesion output by the
[lesion](lesion.md) component will be compared. If `registration` is `true`, the
comparison will be made after an error minimizing rigid transformation,
effectively providing post-operative registration. If `false`, it assumes
registration of post- to pre-operative images has already occurred.
