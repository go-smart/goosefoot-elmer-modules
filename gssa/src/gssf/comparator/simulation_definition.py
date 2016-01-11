# This file is part of the Go-Smart Simulation Architecture (GSSA).
# Go-Smart is an EU-FP7 project, funded by the European Commission.
#
# Copyright (C) 2013-  NUMA Engineering Ltd. (see AUTHORS file)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

from munkres import Munkres
from difflib import Differ
from .. import parameters

# CDM: Clinical Domain Model (see documentation)


# Abstract definition of a GSSA simulation. Ideally, this would be incorporated
# into GSSA itself as a nicer way of representing the GSSA-XML content for
# processing. It also contains an understanding of diffing.
# TODO: incorporate SimulationDefinition into server code
class SimulationDefinition:
    # This represents an argument to an algorithm (more generally, DB-defined lambda
    # function) [see CDM]
    class Argument:
        name = ""

        def __init__(self, name):
            self.name = name

        # An argument is defined up to equivalence by its name
        def diff(self, other):
            messages = []

            if self.name != other.name:
                messages += ["Argument: names differ %s // %s" % (self.name, other.name)]

            return messages

        def __eq__(self, other):
            return self.diff(other)

    # This is a percutaneous needle (more generally, one of a set of possible
    # implements used in a procedure, possibly with repetition) [see CDM]
    class Needle:
        index = ""
        cls = ""
        file = ""
        parameters = None

        def __init__(self, index, cls, file, parameters):
            self.index = index
            self.cls = cls
            self.file = file
            self.parameters = dict((p[0], SimulationDefinition.Parameter(*p)) for p in parameters)

        # Needles are defined by their class, ID/file and their parameters (inc.
        # location) [see CDM]
        def diff(self, other):
            messages = []

            string_comparisons = {
                "cls": (self.cls, other.cls),
                "file": (self.file, other.file),
            }
            for field, pair in string_comparisons.items():
                if pair[0] != pair[1]:
                    messages += ["Needle: for index %s, %s fields differ %s // %s" % (self.index, field, pair[0], pair[1])]

            all_parameters = set().union(self.parameters.keys(), other.parameters.keys())
            for name in all_parameters:
                if name not in self.parameters:
                    messages += ["Needle: this (%s) has no parameter %s" % (self.index, name)]
                elif name not in other.parameters:
                    messages += ["Needle: that (%s) has no parameter %s" % (other.index, name)]
                else:
                    messages += self.parameters[name].diff(other.parameters[name])

            return messages

        def __eq__(self, other):
            return self.diff(other) == []

    # Regions are geometric subdomains (2D/3D) [see CDM]
    class Region:
        id = ""
        name = ""
        format = ""
        input = ""
        groups = None

        def __init__(self, id, name, format, input, groups):
            self.id = id
            self.name = name
            self.format = format
            self.input = input
            self.groups = groups

        # A region's definition is, strictly, in the separate geometry file
        # describing it (usually), but the GSSA-XML should be able to provide
        # enough information to tie it down
        def diff(self, other):
            messages = []

            string_comparisons = {
                "id": (self.id, other.id),
                "name": (self.name, other.name),
                "format": (self.format, other.format),
                "input": (self.input, other.input),
            }
            for field, pair in string_comparisons.items():
                if pair[0] != pair[1]:
                    messages += ["Region: for ID %s, %s fields differ %s // %s" % (self.id, field, pair[0], pair[1])]

            all_groups = set().union(self.groups, other.groups)
            for name in all_groups:
                if name not in self.groups:
                    messages += ["Region: this (%s) has no group %s" % (self.id, name)]
                elif name not in other.groups:
                    messages += ["Region: that (%s) has no group %s" % (other.id, name)]

            return messages

        def __eq__(self, other):
            return self.diff(other) == []

    # An algorithm is a DB-defined lambda function that takes simulation-time
    # arguments, such as Time or CurrentNeedleLength, and returns a
    # Parameter-like value. In the GSSF case, these are generally MATC functions
    # [see CDM]
    class Algorithm:
        result = ""
        arguments = None
        content = ""

        def __init__(self, result, arguments, content):
            self.result = result
            self.arguments = dict((a, SimulationDefinition.Argument(a)) for a in arguments)
            self.content = content

        # An Algorithm is defined by its result (parameter), arguments (above) and content (textual)
        def diff(self, other):
            messages = []

            if self.result != other.result:
                messages += ["Algorithm: results differ %s // %s" % (self.result, other.result)]

            all_arguments = set().union(self.arguments.keys(), other.arguments.keys())
            for name in all_arguments:
                if name not in self.arguments:
                    messages += ["Algorithm: %s has no argument %s" % (self.result, name)]
                elif name not in other.arguments:
                    messages += ["Algorithm: %s has no argument %s" % (other.result, name)]
                else:
                    messages += self.arguments[name].diff(other.arguments[name])

            if self.content != other.content:
                messages += ["Algorithm: %s content differs" % (self.result,)]

            return messages

        def __eq__(self, other):
            return self.diff(other) == []

    # The Numerical Model is a template or, say, a Python code using the
    # helper Go-Smart module to define run-time GSSA parameters. The code is its
    # definition, along with the regions, needles, parameters and so forth needed
    # to complete it [see CDM]
    class NumericalModel:
        definition = ""
        regions = None
        needles = None

        def __init__(self, definition, regions, needles):
            self.definition = definition
            self.regions = dict((r[0], SimulationDefinition.Region(*r)) for r in regions)
            self.needles = dict((n[0], SimulationDefinition.Needle(*n)) for n in needles)

        def diff(self, other):
            messages = []

            # Note that this can only effectively compare embedded definitions
            if self.definition != other.definition:
                if not self.definition:
                    messages += ["Numerical Model: this has no definition"]
                elif not other.definition:
                    messages += ["Numerical Model: that has no definition"]
                else:
                    messages += ["Numerical Model: definitions differ"]
                    d = Differ()
                    messages += d.compare(self.definition, other.definition)

            all_regions = set().union(self.regions.keys(), other.regions.keys())
            for id in all_regions:
                if id not in self.regions:
                    messages += ["Numerical Model: this has no region %s" % id]
                elif id not in other.regions:
                    messages += ["Numerical Model: that has no region %s" % id]
                else:
                    messages += self.regions[id].diff(other.regions[id])

            diff_matrix = []
            this_keys = list(self.needles.keys())
            that_keys = list(other.needles.keys())

            if len(this_keys) != len(that_keys):
                messages += ["Numerical Model: this has different needle count than that"]

            if len(this_keys) > 0 and len(that_keys) > 0:
                for this_key in this_keys:
                    diff_row = []
                    for that_key in that_keys:
                        needle_messages = self.needles[this_key].diff(other.needles[that_key])
                        diff_row.append(len(needle_messages))
                    diff_matrix.append(diff_row)

                m = Munkres()
                indexes = m.compute(diff_matrix)
                for row, column in indexes:
                    messages += self.needles[this_keys[row]].diff(other.needles[that_keys[column]])

            return messages

        def __eq__(self, other):
            return self.diff(other) == []

    # This is the fundamental class representing an arbitrary-type attribute of
    # a simulation [see CDM]
    class Parameter:
        value = None
        typ = ""
        name = ""

        def __init__(self, name, value, typ):
            self.name = name
            self.typ = typ
            self.value = parameters.convert_parameter(value, typ)

        def diff(self, other):
            messages = []

            if self.name != other.name:
                messages += ["Parameter: names differ - %s // %s" % (self.name, other.name)]
            else:
                if self.typ != other.typ:
                    messages += ["Parameter %s: types differ - %s // %s" % (self.name, self.typ, other.typ)]
                if self.value != other.value:
                    messages += ["Parameter %s: values differ - %s // %s" % (self.name, str(self.value), str(other.value))]

            return sorted(messages)

        def __eq__(self, other):
            return self.diff(other) == []

    # This is not part of the CDM, being a setting indicating how the simulation
    # server should receive or send separate files, however it is a key
    # component of GSSA-XML
    class Transferrer:
        url = ""
        cls = ""

        def __init__(self, cls, url):
            self.url = url
            self.cls = cls

        def __eq__(self, other):
            return self.diff(other) == []

        def diff(self, other):
            messages = []

            if self.url != other.url:
                messages += ["Transferrer: URLs differ - %s // %s" % (self.url, other.url)]
            if self.cls != other.cls:
                messages += ["Transferrer: classes differ - %s // %s" % (self.cls, other.cls)]

            return sorted(messages)

    transferrer = None
    parameters = None
    algorithms = None
    numerical_model = None
    name = "This"

    def __init__(self, name):
        self.parameters = {}
        self.algorithms = {}
        self.name = name

    def add_parameter(self, name, value, typ):
        self.parameters[name] = self.Parameter(name, value, typ)

    def add_algorithm(self, result, arguments, content):
        self.algorithms[result] = self.Algorithm(result, arguments, content)

    def set_transferrer(self, cls, url):
        self.transferrer = self.Transferrer(cls, url)

    def set_numerical_model(self, definition, regions, needles):
        self.numerical_model = self.NumericalModel(definition, regions, needles)

    # Produce a series of human-readable messages describing the
    # non-equivalences between this and another ("that") definition
    def diff(self, other):
        messages = []

        # At each step we check whether the relevant component is present in one
        # or both definitions, then request a diff for it

        if self.transferrer or other.transferrer:
            if not self.transferrer:
                messages += ["%s definition has no transferrer" % self.name]
            elif not other.transferrer:
                messages += ["%s other definition has no transferrer" % other.name]
            else:
                messages += self.transferrer.diff(other.transferrer)

        if self.algorithms or other.algorithms:
            if not self.algorithms:
                messages += ["%s definition has no algorithms" % other.name]
            elif not other.algorithms:
                messages += ["%s definition has no algorithms" % other.name]
            else:
                all_algorithms = set().union(self.algorithms.keys(), other.algorithms.keys())
                for name in all_algorithms:
                    if name not in self.algorithms:
                        messages += ["%s definition has no algorithm '%s'" % (self.name, name)]
                    elif name not in other.algorithms:
                        messages += ["%s definition has no algorithm '%s'" % (other.name, name)]
                    else:
                        messages += self.algorithms[name].diff(other.algorithms[name])

        if self.parameters or other.parameters:
            if not self.parameters:
                messages += ["%s definition has no parameters" % self.name]
            elif not other.parameters:
                messages += ["%s definition has no parameters" % other.name]
            else:
                # For comparing parameters, we first check the keys match, then
                # compare type/value-wise
                all_parameters = set().union(self.parameters.keys(), other.parameters.keys())
                for name in all_parameters:
                    if name not in self.parameters:
                        messages += ["%s definition has no parameter '%s'" % (self.name, name)]
                    elif name not in other.parameters:
                        messages += ["%s definition has no parameter '%s'" % (other.name, name)]
                    else:
                        messages += self.parameters[name].diff(other.parameters[name])

        if self.numerical_model or other.numerical_model:
            if not self.numerical_model:
                messages += ["%s definition has no numerical model" % self.name]
            elif not other.numerical_model:
                messages += ["%s definition has no numerical model" % other.name]
            else:
                messages += self.numerical_model.diff(other.numerical_model)

        # Messages are sorted for readability
        return sorted(messages)

    def __eq__(self, other):
        return self.diff(other) == []
