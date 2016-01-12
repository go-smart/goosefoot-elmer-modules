# Go-Smart Simulation Model Families

Families represent groups of workflows that are simulated in approximately the
same way, or at least by using the same tools. This is slightly more specific
than a workflow as, for example, the Docker workflow has a separate family for
each Docker image. It corresponds more accurately to a single configuration
system. This comprises a pair: a subclass of `gssa.family.Family`
that contains instructions for interpreting [GSSA-XML](gssa-xml.md) for this
type of simulation and writing any necessary tool-specific configuration files,
and the simulation tools themselves.

Two examples are [GSSF](gssf/overview.md), which is also a fully fledged
workflow, and [FEniCS](docker/fenics.md), which depends on pre-meshing by
the [GSSF CGAL mesher component](gssf/mesher.md).

To find families, the `gssa.family` module scans for files in the directory of
the `gssa.families` module. **TODO**: extend this to user-configurable locations.

**Note**: family modules are run as part of the Python server - this means they
are privileged and should only be included from highly *trusted* sources, as
with other codes running as `www-data`. They are not sandboxed or intended to be
user-supplied, but constitute a basic plug-in framework. The longer-term
intention is to migrate to a proper plug-in framework, where community developed
families may be added in, running as an unprivileged user. Of course, this would
still require a higher level of trust than given to external user code, which
only runs sandboxed in Docker.
