import asyncio
import traceback
import os
import math
import json
from lxml import etree as ET
import sys
import shutil
import yaml

from gosmart.server.parameters import convert_parameter

from gosmart.server.families.gssf_arguments import GoSmartSimulationFrameworkArguments


class MesherGSSFMixin:
    _mesher_xml = None
    _nonmeshing_groups = set(['segmented-lesions'])

    def init_mesher(self, files_required):
        self._needles = {}
        self._needle_order = {}
        self._files_required = files_required
        self._args = GoSmartSimulationFrameworkArguments(configfilenames=["settings.xml"])

    @asyncio.coroutine
    def mesh(self, working_directory):
        try:
            translated_xml = self.to_mesh_xml()
        except RuntimeError as e:
            traceback.print_exc(file=sys.stderr)
            raise e

        tree = ET.ElementTree(translated_xml)

        with open(os.path.join(working_directory, "settings.xml"), "wb") as f:
            tree.write(f, pretty_print=True)

        args = ["go-smart-launcher"] + self._args.to_list()

        task = yield from asyncio.create_subprocess_exec(
            *[a for a in args if a not in ('stdin', 'stdout', 'stderr')],
            cwd=working_directory
        )

        yield from task.wait()

        msh_input = os.path.join(working_directory, "mesher", "elmer_libnuma.msh")
        mesh_labelling_yaml = os.path.join(working_directory, "mesher", "mesh_labelling.yml")
        shutil.copyfile(msh_input, os.path.join(working_directory, "input", "input.msh"))
        self._files_required["input.msh"] = msh_input

        with open(mesh_labelling_yaml, "r") as f:
            mesh_labelling = yaml.load(f)

        regions = mesh_labelling.copy()
        regions.update(self._regions)
        for k, v in regions.items():
            if k in mesh_labelling:
                v.update(mesh_labelling[k])
        self._regions = regions

        return task.returncode == 0

    def to_mesh_xml(self):
        root = ET.Element('gosmart')
        root.set('name', 'elmer_libnuma')
        root.set('version', '1.0.1')

        geometry = ET.Element('geometry')
        root.append(geometry)

        centre_location = self.get_parameter("CENTRE_LOCATION")
        if centre_location is None or centre_location == "first-needle":
            if self._needles:
                centre_location = self.get_needle_parameter(0, "NEEDLE_TIP_LOCATION")
        elif centre_location == "centroid-of-tips":
            if self._needles:
                needle_tips = [self.get_needle_parameter(i, "NEEDLE_TIP_LOCATION") for i in range(len(self._needles))]
                needle_tips = zip(*needle_tips)
                centre_location = [sum(tips) / len(self._needles) for tips in needle_tips]

        if self._needles:
            needle_axis_node = ET.Element('needleaxis')
            tip_location = self.get_needle_parameter(0, "NEEDLE_TIP_LOCATION")
            entry_location = self.get_needle_parameter(0, "NEEDLE_ENTRY_LOCATION")
            norm = 0
            vec = []
            for c, vt, ve in zip(('x', 'y', 'z'), tip_location, entry_location):
                needle_axis_node.set(c, str(ve - vt))
                vec.append(ve - vt)
                norm += (ve - vt) * (ve - vt)
            geometry.append(needle_axis_node)

            offset = self.get_parameter("CENTRE_OFFSET")
            if offset is not None:
                for c, v in enumerate(centre_location):
                    centre_location[c] = v + offset * vec[c] / math.sqrt(norm)

        centre_location_node = ET.Element("centre")
        for c, v in zip(('x', 'y', 'z'), centre_location):
            centre_location_node.set(c, str(v))
        geometry.append(centre_location_node)

        if self.get_parameter("SIMULATION_SCALING") is not None:
            ET.SubElement(geometry, "simulationscaling") \
                .set("ratio",
                     str(self.get_parameter("SIMULATION_SCALING")))

        regions = ET.SubElement(root, "regions")
        for name, region in self._regions.items():
            regionNode = ET.SubElement(regions, region["format"])
            regionNode.set("name", name)
            regionNode.set("input", os.path.join("input/", region["input"]))
            regionNode.set("groups", "; ".join(region["groups"]))

        parameters = ET.SubElement(root, "constants")
        for key, parameterPair in self._parameters.items():
            parameter, typ = parameterPair
            parameterNode = ET.SubElement(parameters, "parameter")
            parameterNode.set("name", key)
            p = convert_parameter(parameter, typ)
            parameterNode.set("value", json.dumps(p))
            if typ is not None:
                parameterNode.set("type", typ)

        name_needle_regions = False

        needlelibrary = ET.SubElement(root, 'needlelibrary')
        solid_needles = self.get_parameter("SETTING_SOLID_NEEDLES")
        if solid_needles is not None:
            needlelibrary.set("zones", "true" if solid_needles is True else "false")
            name_needle_regions = True

        mesher = ET.SubElement(root, "mesher")
        mesher.set('type', 'CGAL')
        if self.get_parameter("SETTING_SOLID_NEEDLES") is True or self.get_parameter("SETTING_ZONE_BOUNDARIES") is True:
            mesher.set("zone_boundaries", "true")

        mesher_inner = self.get_parameter("SETTING_AXISYMMETRIC_INNER")
        if mesher_inner is not None:
            innerNode = ET.SubElement(mesher, "inner")
            innerNode.set("type", "axisymmetric")
            innerNode.set("template", mesher_inner)

        mesher_inner_coarse = self.get_parameter("SETTING_AXISYMMETRIC_INNER_COARSE")
        if mesher_inner_coarse is not None:
            innerNode = ET.SubElement(mesher, "inner")
            innerNode.set("type", "axisymmetric")
            innerNode.set("name", "coarse")
            innerNode.set("template", mesher_inner_coarse)

        extent = ET.SubElement(mesher, 'extent')
        radius = self.get_parameter("SIMULATION_DOMAIN_RADIUS")
        if radius is not None:
            extent.set('radius', str(radius))
        else:
            extent.set('radius', '50')  # TODO: This should be done in the parameters!!!!

        ET.SubElement(mesher, 'centre')

        for idx, region in self._regions.items():
            if region['meaning'] == 'organ':
                if self.get_parameter('SETTING_ORGAN_AS_SUBDOMAIN'):
                    ET.SubElement(mesher, 'zone').set('region', idx)
                else:
                    ET.SubElement(mesher, 'organ').set('region', idx)
            elif region['format'] == 'zone' and not (set(region['groups']) & self._nonmeshing_groups):
                ET.SubElement(mesher, 'zone').set('region', idx)
            elif 'vessels' in region['groups'] or 'bronchi' in region['groups']:
                ET.SubElement(mesher, 'vessel').set('region', idx)

        lengthscales = ET.SubElement(mesher, 'lengthscales')

        if self.get_parameter('RESOLUTION_HIGH'):
            lengthscale_settings = [
                ('nearfield', '1.0'), ('farfield', '2.0'), ('zonefield', '1.0'),
                ('vessels', 'far')
            ]
        else:
            lengthscale_settings = [
                ('nearfield', '2.0'), ('farfield', '5.0'), ('zonefield', '2.0'),
                ('vessels', 'far')
            ]

        nearfield = self.get_parameter('RESOLUTION_FIELD_NEAR')
        needlezonefield = self.get_parameter('RESOLUTION_FIELD_NEEDLE_ZONE')
        farfield = self.get_parameter('RESOLUTION_FIELD_FAR')
        zonefield = self.get_parameter('RESOLUTION_FIELD_ZONE')

        if nearfield:
            lengthscale_settings[0] = ('nearfield', nearfield)
        if farfield:
            lengthscale_settings[1] = ('farfield', farfield)
        if zonefield:
            lengthscale_settings[2] = ('zonefield', zonefield)

        for k, v in lengthscale_settings:
            lengthscales.set(k, str(v))
        if needlezonefield:
            lengthscales.set("needlezonefield", str(needlezonefield))

        ET.SubElement(root, 'optimizer')
        ET.SubElement(root, 'elmergrid')
        l = 0
        globalNeedlesNode = ET.SubElement(root, "needles")
        for ix, needle in self._needles.items():
            globalNeedleNode = ET.SubElement(globalNeedlesNode, "needle")
            l += 1
            globalNeedleNode.set("name", str(l))

            if needle['class'] in ('solid-boundary', 'boundary'):
                location = needle['file'].split(':', 1)

                needle_mesh = None
                if location[0] in ('surface', 'zone', 'both'):
                    needleNode = ET.SubElement(regions, location[0])
                    needleNode.set("name", str(l))
                    needleNode.set("input", os.path.join("input/", location[1]))
                    needleNode.set("groups", "needles")

                    needle_mesh = ET.SubElement(mesher, 'needle')
                    needle_mesh.set('region', str(l))
                    if needlezonefield and location[0] != 'surface':
                        needle_mesh.set("characteristic_length", needlezonefield)
                else:
                    needleNode = ET.SubElement(needlelibrary, 'needle')

                    if name_needle_regions:
                        needleNode.set("name", str(l))

                    if location[0] == 'library':
                        needleNode.set("id", location[1])
                    else:
                        needleNode.set("name", location[1])

                    tip_location = self.get_needle_parameter(ix, "NEEDLE_TIP_LOCATION")
                    entry_location = self.get_needle_parameter(ix, "NEEDLE_ENTRY_LOCATION")
                    needleNode.set("offset", " ".join(map(lambda c: str(c[1] - c[0]), zip(tip_location, centre_location))))
                    needleNode.set("axis", " ".join(map(lambda c: str(c[0] - c[1]), zip(entry_location, tip_location))))

                    parameters = ET.SubElement(globalNeedleNode, "parameters")
                    for key, parameterPair in needle["parameters"].items():
                        parameter, typ = parameterPair
                        parameterNode = ET.SubElement(parameters, "constant")
                        parameterNode.set("name", key)
                        parameterNode.set("value", str(convert_parameter(parameter, typ)))

                needle_active_length = self.get_needle_parameter(ix, "NEEDLE_ACTIVE_LENGTH")
                if needle_active_length is not None:
                    if needle_mesh is None:
                        needle_mesh = ET.SubElement(mesher, 'needle' if solid_needles else 'solid')
                        needle_mesh.set('region', 'needle-' + str(l))
                    activity = ET.SubElement(needle_mesh, 'activity')
                    tip_location = self.get_needle_parameter(ix, "NEEDLE_TIP_LOCATION")
                    for c, vt in zip(('x', 'y', 'z'), tip_location):
                        activity.set(c, str(vt))
                    activity.set('r', str(needle_active_length))

        self._mesher_xml = root

        return self._mesher_xml
