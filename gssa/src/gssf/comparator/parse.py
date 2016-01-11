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

from .simulation_definition import SimulationDefinition
import json


# This turns GSSA-XML into a definition
# TODO: use this implementation for the whole server
# NB: it will need extended to include non-diff-relevant elements/fields
def gssa_xml_to_definition(root, label):
    # We must have a simulationDefinition root
    if root is None:
        raise RuntimeError("%s: No root tag" % label)

    if root.tag != "simulationDefinition":
        raise RuntimeError("%s: Incorrect top tag" % label)

    simulationDefinition = SimulationDefinition(label)

    # If there is a transferrer, that is the basis of a comparison
    transferrer = root.findall("transferrer")

    if len(transferrer) > 1:
        raise RuntimeError("%s: Too many transferrer nodes")
    elif len(transferrer) == 1:
        url = transferrer[0].find('url')
        cls = transferrer[0].get("class")
        simulationDefinition.set_transferrer(cls, url.text)

    # Start adding in algorithms
    algorithms = root.findall("algorithms")

    if len(algorithms) > 1:
        raise RuntimeError("%s: Too many algorithms nodes")
    elif len(algorithms) == 1:
        for algorithm in algorithms[0]:
            # Every algorithm has a result
            result = algorithm.get('result')

            if result is None:
                raise RuntimeError("%s: An algorithm is missing a result")

            arguments = []
            content = None
            # We should have a context, representing the algorihtm body, and an arguments node
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

    # The parameters, normally the largest number of elements, are parsed
    # (although not to their types)
    parameters = root.findall("parameters")

    if len(parameters) > 1:
        raise RuntimeError("%s: Too many parameters nodes" % label)
    elif len(parameters) == 1:
        for parameter in parameters[0]:
            simulationDefinition.add_parameter(parameter.get('name'), parameter.get('value'), parameter.get('type'))

    # Define the numerical model - this incorporates more than the CDM numerical
    # model, strictly, as number/type of needles is specified, and so forth
    numericalModel = root.findall("numericalModel")

    if len(numericalModel) > 1:
        raise RuntimeError("%s: Too many numericalModel nodes" % label)
    elif len(numericalModel) == 1:
        needles = []
        regions = []
        definition = None

        for node in numericalModel[0]:
            # The numerical model should contain the needles (in GSSA-XML, at
            # present)
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

                    # Needles can each have their own parameters
                    parameters = []
                    if len(needle) > 1 or (len(needle) == 1 and needle[0].tag != 'parameters'):
                        raise RuntimeError("%s: Needle tag must have no children or one parameters tag" % label)
                    elif len(needle) == 1:
                        for parameter in needle[0]:
                            parameters.append((parameter.get('name'), parameter.get('value'), parameter.get('type')))

                    needles.append((index, cls, file, parameters))

            # Region indicates the geometric subdomains and their definitions
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
                        raise RuntimeError("%s: Could not read region groups" % label)

                    region_tuple = (region_id, name, format, input, groups)
                    if None in region_tuple:
                        raise RuntimeError("%s: Region tag has not got all information: Id '%s', Name '%s', Format '%s', Input '%s', Groups '%s'" %
                                           (label, region_id, name, format, input, groups))

                    regions.append(region_tuple)

            elif node.tag == 'definition':
                if node.text is None:
                    raise RuntimeError("%s: Numerical model 'definition' tag exists but is empty [TODO: add support for external definitions]" % label)
                # TODO: implement family comparison, as well as definition
                # content
                definition = node.text.strip()
            else:
                raise RuntimeError("%s: Unknown node in numerical model: %s" % (label, node.tag))

        simulationDefinition.set_numerical_model(definition, regions, needles)

    return simulationDefinition
