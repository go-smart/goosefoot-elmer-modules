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

from .parameters import read_parameters


# This extracts basic information, common to all families, from GSSA-XML
class GoSmartSimulationTranslator:
    def __init__(self):
        self._files_required = {}

    def get_files_required(self):
        return self._files_required

    def translate(self, xml):
        parameters = {}
        parameters_node = xml.find('parameters')

        # The parameters should always be processed
        if parameters_node is not None:
            parameters = read_parameters(parameters_node)

        algorithms = {}
        algorithms_node = xml.find('algorithms')
        # Algorithms are always defined here (if any)
        if algorithms_node is not None:
            for algorithm in algorithms_node:
                arguments = []
                arguments_node = algorithm.find('arguments')
                if arguments_node is not None:
                    for argument in arguments_node:
                        arguments.append(argument.get('name'))

                algorithms[algorithm.get('result')] = {
                    "content": algorithm.find('content').text,
                    "arguments": arguments
                }

        # The numerical model node contains all the information for the family
        # specific set-up, but all we need now is the name of the family (and
        # the definition to pass to it)
        numerical_model_node = xml.find('numericalModel')
        if numerical_model_node is None:
            raise RuntimeError("Numerical model missing")

        definition = numerical_model_node.find('definition')
        if definition is None:
            raise RuntimeError("Missing model definition")

        family = definition.get('family')

        return family, numerical_model_node, parameters, algorithms
