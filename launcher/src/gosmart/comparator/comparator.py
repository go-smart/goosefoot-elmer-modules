from lxml import etree as ET
from munkres import Munkres
from difflib import Differ
import json
from gosmart.server import parameters


class SimulationDefinition:
    class Argument:
        name = ""

        def __init__(self, name):
            self.name = name

        def diff(self, other):
            messages = []

            if self.name != other.name:
                messages += ["Argument: names differ %s // %s" % (self.name, other.name)]

            return messages

        def __eq__(self, other):
            return self.diff(other)

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

    class Algorithm:
        result = ""
        arguments = None
        content = ""

        def __init__(self, result, arguments, content):
            self.result = result
            self.arguments = dict((a, SimulationDefinition.Argument(a)) for a in arguments)
            self.content = content

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

    def diff(self, other):
        messages = []

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

        print(messages)
        print(sorted(messages))
        return sorted(messages)

    def __eq__(self, other):
        return self.diff(other) == []


class Comparator:
    left_text = None
    right_text = None

    def __init__(self, left_text, right_text):
        self.left = ET.fromstring(bytes(left_text, 'utf-8'))
        self.right = ET.fromstring(bytes(right_text, 'utf-8'))

    def diff(self):
        left_structure = self.__analyse(self.left, "Left")
        right_structure = self.__analyse(self.right, "Right")

        return left_structure.diff(right_structure)

    def equal(self):
        return self.diff() == []

    def __analyse(self, root, label):
        if root is None:
            raise RuntimeError("%s: No root tag" % label)

        if root.tag != "simulationDefinition":
            raise RuntimeError("%s: Incorrect top tag" % label)

        simulationDefinition = SimulationDefinition(label)

        transferrer = root.findall("transferrer")

        if len(transferrer) > 1:
            raise RuntimeError("%s: Too many transferrer nodes")
        elif len(transferrer) == 1:
            url = transferrer[0].find('url')
            cls = transferrer[0].get("class")
            simulationDefinition.set_transferrer(cls, url.text)

        algorithms = root.findall("algorithms")

        if len(algorithms) > 1:
            raise RuntimeError("%s: Too many algorithms nodes")
        elif len(algorithms) == 1:
            for algorithm in algorithms[0]:
                result = algorithm.get('result')

                if result is None:
                    raise RuntimeError("%s: An algorithm is missing a result")

                arguments = []
                content = None
                for node in algorithm:
                    if node.tag == 'content':
                        content = node.text
                    elif node.tag == 'arguments':
                        for argument in node:
                            name = argument.get('name')
                            if argument.get('name') is None or argument.tag != 'argument':
                                raise RuntimeError("%s: Algorithm %s has a malformed argument (tag %s)" % (result, argument.tag))
                            arguments.append(name)
                    else:
                        raise RuntimeError("%s: Algorithm %s has a rogue tag: %s" % (result, argument.tag))

                if content is None:
                    content = ""

                simulationDefinition.add_algorithm(result, arguments, content.strip())

        parameters = root.findall("parameters")

        if len(parameters) > 1:
            raise RuntimeError("%s: Too many parameters nodes" % label)
        elif len(parameters) == 1:
            for parameter in parameters[0]:
                simulationDefinition.add_parameter(parameter.get('name'), parameter.get('value'), parameter.get('type'))

        numericalModel = root.findall("numericalModel")

        if len(numericalModel) > 1:
            raise RuntimeError("%s: Too many numericalModel nodes" % label)
        elif len(numericalModel) == 1:
            needles = []
            regions = []
            definition = None

            for node in numericalModel[0]:
                if node.tag == 'needles':
                    for needle in node:
                        if needle.tag != 'needle':
                            raise RuntimeError("%s: Numerical model needles should only have needle nodes, not %s" %
                                               (label, needle.tag))
                        index = needle.get('index')
                        cls = needle.get('class')
                        file = needle.get('file')
                        if None in (index, cls, file):
                            raise RuntimeError("%s: Needle tag has not got all information: Index '%s', Class '%s', File '%s'" %
                                               label, index, cls, file)

                        parameters = []
                        if len(needle) > 1 or (len(needle) == 1 and needle[0].tag != 'parameters'):
                            raise RuntimeError("%s: Needle tag must have no children or one parameters tag" % label)
                        elif len(needle) == 1:
                            for parameter in needle[0]:
                                parameters.append((parameter.get('name'), parameter.get('value'), parameter.get('type')))

                        needles.append((index, cls, file, parameters))
                elif node.tag == 'regions':
                    for region in node:
                        if region.tag != 'region':
                            raise RuntimeError("%s: Regions node should only have region children, not %s" %
                                               (label, region.tag))

                        region_id = region.get('id')
                        name = region.get('name')
                        format = region.get('format')
                        input = region.get('input')

                        try:
                            groups = json.loads(region.get('groups'))
                        except TypeError:
                            raise RuntimeError("%s: Could not read region groups" % label)  # RMV-reinclude: from e

                        region_tuple = (region_id, name, format, input, groups)
                        if None in region_tuple:
                            raise RuntimeError("%s: Region tag has not got all information: Id '%s', Name '%s', Format '%s', Input '%s', Groups '%s'" %
                                               (label, region_id, name, format, input, groups))

                        regions.append(region_tuple)
                elif node.tag == 'definition':
                    if node.text is None:
                        raise RuntimeError("%s: Numerical model 'definition' tag exists but is empty" % label)
                    definition = node.text.strip()
                else:
                    raise RuntimeError("%s: Unknown node in numerical model: %s" % (label, node.tag))

            simulationDefinition.set_numerical_model(definition, regions, needles)

        return simulationDefinition
