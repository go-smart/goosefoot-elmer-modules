# Comparison of GSSA-XML documents

This tool allows diff-ing of two XML documents based on their conceptual content
and returning a human-readable comparison.

## go-smart-comparator

This is a simple wrapper script taking two GSSA-XML files and outputting the
difference in human-readable format.

### Usage

The script takes only two arguments: the files to be compared.

    go-smart-comparator file1.xml file2.xml

## Methodology

The ``gssa.comparator`` module loads each file into a
``SimulationDefinition`` object, which has a [CDM](cdm/overview.md)-based understanding
of its content. Moreover, it is capable of diffing itself against another
``SimulationDefinition``. The ``SimulationDefinition`` of the first file passed
to the ``Comparator`` object (the first file argument to
``go-smart-comparator``) is given the ``SimulationDefinition`` of the second. It
returns the description of the difference referring to itself as *left* or
*this* and the other as *right* or *that*.

Note that the comparator is unable to account for non-embedded detail, such as
geometry files of the same name, differences of solver version and numerical
models with a separately passed definition.
