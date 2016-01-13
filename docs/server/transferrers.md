# Transferrers

These are classes that handle moving of input files (other than GSSA-XML) back
and forward from the simulation server to the client. This is configured using a
second-level `<transferrer/>` section in the [GSSA-XML](../gssa-xml.md). The
current set of transferrers assume that the medium is secure (network or local
machine) - this will be
partially improved with the reinstating of SFTPTransferrer, but alternative
methods should be added.

## HTTP Transferrer

This class defines means for transferring input files to and from the client
using HTTP.

### Configuration

The `HTTPTransferrer` class is configured as follows:

```xml
    <transferrer method="http">
        <url>SOURCEURL</url>
      [ <output>DESTINATIONURL</output> ]
    </transferrer>
```

If the `output` node is not present, the system will write output to `/tmp`.
This is primarily useful for debugging purposes.

## Tmp Transferrer

This class, primarily useful for debugging, allows transfer of files through
`/tmp`. It is particularly useful in conjunction with
[go-smart-simulation-client](executables.md#go-smart-simulation-client).

### Configuration

The `TmpTransferrer` class is configured as follows:

```xml
    <transferrer method="tmp">
        <input location="LOCATION" />
    </trasferrer>
```

The `LOCATION` should be an absolute path to a TAR.GZ archive of the input
files, normally in `/tmp`. If it is not specified, the transferrer assumes the
relevant files are unzipped under `/tmp/{remote_root}`.

## SFTP Transferrer

**TODO**: this has become out-of-date and needs updated before it is usable.
