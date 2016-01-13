# Go-Smart Simulation Parameters

Parameters, within a [CDM](overview.md) context, are entities uniquely
defined by their name, representing a value to be passed for simulation. This
could be a solver setting, a physical constant, a representation of clinician
behaviour over time, or essentially anything that can be encoded in a JSON
object. Interpretation, on the simulation side, is the responsibility of the
[family](../families.md) and/or the simulation tool. On the client side, it is the
responsibility of the UI engine to take the value and the widget definition and form a
useful interface.

While a Parameter has a specific value, type and UI widget when concretized, that is,
prepared for
a specific simulation, it may, in the abstract, have many *Parameter Attributions*
each attributing it to a different valid Combination, potentially with a different value,
type and UI widget each.

## Parameter types

The Parameter type is specified in a string field. It should be lowercase,
holding the name of a basic type (e.g. `string`, `integer`, `boolean`, `float`)
or, if an array, it should be written `array(BASICTYPE)` where `TYPE` is
the type of elements of the array. If it is an array of tuples, it should be
written `array(BASICTYPE1, BASICTYPE2)`, etc.

## Parameter Attributions

A *Parameter Attribution*
is an attachment record, linking a Parameter to another CDM entity, such as a
Numerical Model or Needle. A Parameter Attribution has similar columns to a
Parameter, although not a name, instead referencing a ParameterId.

Parameter Attributions are matched to components in a limiting fashion. That is, a
Parameter Attribution can apply to a specific Numerical Model, or a specific
(Numerical Model, Protocol) pair, or a specific (Numerical Model, Context, Needle)
triple, and so forth. A Parameter Attribution always applies to a Combination of
entities *unless* at least one of the entities referenced in the Parameter
Attribution is different
to those named in the Combination. Consequently, a Parameter Attribution
having no entity field filled in, will attribute a Parameter to all Combinations. (This may
be the case for universal constants, but is otherwise not especially useful).

Each Parameter Attribution only be attributed to one specific component of
each type. This does not prevent multiple otherwise identical Parameter
Attributions from coexisting, each applying to a different Context, say.

### Placeholders

A Parameter Attribution with a null value is referred to as a Placeholder. When
it applies to a given Combination, this
indicates to the Combination-forming procedure that another Parameter
Attribution applying to that Combination **must** provide a non-null value for that
Parameter, or the whole Combination is invalid. This is used by a Numerical
Model, for instance, to require a value for an essential Parameter; it could
provide a Placeholder entry for `SETTING_FINAL_TIMESTEP`, thereby ensuring any
Combination incorporating it must have `SETTING_FINAL_TIMESTEP` provided by
another component, say, a Protocol.

It is possible for a Parameter Attribution to specify slightly more flexible
behaviour in this case - this is defined by a Parameter Attribution enum field.
Instead of barring the formation of the Combination, it could prompt the
simulating user with an interactive widget when the Combination is used. Bear in
mind, particularly in a medical setting, that this may be a clinician and not a
technical user.

### Concrete Parameters

When a Simulation is to be run, all of the Parameter Attributions applying to
its Combination
are grouped. A hierarchical algorithm picks one Parameter Attribution for each
Parameter that appears. Any Parameters that has a placeholder but no
value-supplying Parameter Attributions must be given values by the end-user. For
this purpose, Parameter Attributions (so the placeholder) have a `widget` field.
The exact choices in rendering this are the responsibility of the UI.

This *attribution selection* creates an entity, a variation of
Parameter tied to a Simulation with a unique set of field values provided by the
winning Parameter Attribution (or user) - this is refered to as a *Concrete
Parameter* and corresponds to the Parameter concept as seen by the [simulation
server](../server/overview.md).

The Concrete Parameters for a simulation appear in the [GSSA-XML](../gssa-xml.md)
in the following format:

```xml
<parameters>
    <parameter name="PARAMNAME" value="PARAMATTRVALUE" type="PARAMATTRTYPE" />
    ...
</parameters>
```

The `PARAMATTRTYPE` defines the type of `PARRAMATTRVALUE` and should be a type
as described in the [Parameter types](#parameter-types) section.
