import asyncio
import traceback
import os
import math
import json
from lxml import etree as ET
import sys
import shutil
import yaml

from ..parameters import convert_parameter

from ..families.gssf_arguments import GoSmartSimulationFrameworkArguments


# This mixin augments a family with mesher-cgal volumetric meshing support,
# using GSSF tools
class MesherGSSFMixin:
    _mesher_xml = None
    # Region groups whose members should not automatically appear in the <mesher/> section
    _nonmeshing_groups = set(['segmented-lesions'])

    # Our own mixin-specific init
    def init_mesher(self, files_required):
        self._needles = {}
        self._needle_order = {}
        self._files_required = files_required
        self._args = GoSmartSimulationFrameworkArguments(configfilenames=["settings.xml"])

    # Execute the mesher (this routine is not necessary for GSSF, which executes
    # the whole workflow in its own simulation call)
    @asyncio.coroutine
    def mesh(self, working_directory):
        # Prepare the GSSF-XML
        try:
            translated_xml = self.to_mesh_xml()
        except RuntimeError as e:
            traceback.print_exc(file=sys.stderr)
            raise e

        # The output we expect from the mesher for input into the rest of the
        # mixed-in family. If this found in the input folder, we take it as
        # input instead of running the mesher
        input_msh = os.path.join(working_directory, "input", "input.msh")
        labelling_yaml = os.path.join(working_directory, "input", "mesh_labelling.yml")

        success = False

        # If we have an uploaded MSH, often used for testing, skip the meshing
        # section
        uploaded_msh = os.path.join(working_directory, "input", "mesh-0.msh")
        if os.path.exists(uploaded_msh):
            shutil.copyfile(uploaded_msh, input_msh)
            print("Found uploaded msh")
            success = True

        # If we do not, then prepare the mesher-cgal configuration
        if not os.path.exists(input_msh):
            tree = ET.ElementTree(translated_xml)

            # Write out the GSSF-XML file
            with open(os.path.join(working_directory, "settings.xml"), "wb") as f:
                tree.write(f, pretty_print=True)

            # Set up the arguments for GSL
            args = ["go-smart-launcher"] + self._args.to_list()

            # Launch
            task = yield from asyncio.create_subprocess_exec(
                *[a for a in args if a not in ('stdin', 'stdout', 'stderr')],
                cwd=working_directory
            )

            # Hold off until meshing is complete
            yield from task.wait()

            # Pick out the relevant mesher output
            msh_input = os.path.join(working_directory, "mesher", "elmer_libnuma.msh")
            mesh_labelling_yaml = os.path.join(working_directory, "mesher", "mesh_labelling.yml")
            shutil.copyfile(msh_input, input_msh)
            shutil.copyfile(mesh_labelling_yaml, labelling_yaml)

            # Check for success from GSSF mesher-cgal
            success = (task.returncode == 0)

            # Update the regions based on this regions file
            with open(labelling_yaml, "r") as f:
                mesh_labelling = yaml.load(f)

            regions = mesh_labelling.copy()
            regions.update(self._regions)
            for k, v in regions.items():
                if k in mesh_labelling:
                    v.update(mesh_labelling[k])
            self._regions = regions

        self._files_required["input.msh"] = input_msh

        return success

    # Use the GSSA-XML to produce only the meshing-relevant parts (most of)
    # GSSF-XML
    def to_mesh_xml(self):
        root = ET.Element('gssf')
        root.set('name', 'elmer_libnuma')
        root.set('version', '1.0.2')

        # Start by creating a geometry section
        geometry = ET.Element('geometry')
        root.append(geometry)

        # We get the location of the simulation centre from the parameters
        centre_location = self.get_parameter("CENTRE_LOCATION")

        # If it isn't there, then the centre will be the first needle tip, or
        # if we have been given "centroid-of-tips" instead of a set of
        # coordinates, calculate the centroid of all the tips
        if centre_location is None or centre_location == "first-needle":
            if self._needles:
                centre_location = self.get_needle_parameter(0, "NEEDLE_TIP_LOCATION")
        elif centre_location == "centroid-of-tips":
            if self._needles:
                needle_tips = [self.get_needle_parameter(i, "NEEDLE_TIP_LOCATION") for i in range(len(self._needles))]
                needle_tips = zip(*needle_tips)
                centre_location = [sum(tips) / len(self._needles) for tips in needle_tips]

        # If we have a needle, then use it to set the `needleaxis`
        if self._needles:
            needle_axis_node = ET.Element('needleaxis')

            # Get the entry and tip of the first needle
            tip_location = self.get_needle_parameter(0, "NEEDLE_TIP_LOCATION")
            entry_location = self.get_needle_parameter(0, "NEEDLE_ENTRY_LOCATION")
            norm = 0
            vec = []
            for c, vt, ve in zip(('x', 'y', 'z'), tip_location, entry_location):
                needle_axis_node.set(c, str(ve - vt))
                vec.append(ve - vt)
                norm += (ve - vt) * (ve - vt)
            # Based on the calculated axis, add this to the geometry
            geometry.append(needle_axis_node)

            # FIXME: is this supposed to be inside this if??
            # Use the CENTRE_OFFSET parameter to shift the geometry if required
            offset = self.get_parameter("CENTRE_OFFSET")
            if offset is not None:
                for c, v in enumerate(centre_location):
                    centre_location[c] = v + offset * vec[c] / math.sqrt(norm)

        # After all the calculations above, use the finally chosen centre in the
        # geometry section
        centre_location_node = ET.Element("centre")
        for c, v in zip(('x', 'y', 'z'), centre_location):
            centre_location_node.set(c, str(v))
        geometry.append(centre_location_node)

        # If we have a simulation scaling parameter, that goes into the geometry
        # section also
        if self.get_parameter("SIMULATION_SCALING") is not None:
            ET.SubElement(geometry, "simulationscaling") \
                .set("ratio",
                     str(self.get_parameter("SIMULATION_SCALING")))

        # Each region goes into the regions section, fairly intuitively
        regions = ET.SubElement(root, "regions")
        for name, region in self._regions.items():
            regionNode = ET.SubElement(regions, region["format"])
            regionNode.set("name", name)
            regionNode.set("input", os.path.join("input/", region["input"]))
            regionNode.set("groups", "; ".join(region["groups"]))

        # Add the parameters wholesale
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

        # The needlelibrary needs to know if we have solid needles
        needlelibrary = ET.SubElement(root, 'needlelibrary')
        solid_needles = self.get_parameter("SETTING_SOLID_NEEDLES")
        if solid_needles is not None:
            needlelibrary.set("zones", "true" if solid_needles is True else "false")
            name_needle_regions = True

        # The outer mesh, as far as we are concerned, is always CGAL
        mesher = ET.SubElement(root, "mesher")
        mesher.set('type', 'CGAL')
        # RMV: should this be reinserted?
        # if self.get_parameter("SETTING_SOLID_NEEDLES") is True or self.get_parameter("SETTING_ZONE_BOUNDARIES") is True:
        mesher.set("zone_boundaries", "true")

        # If we have an inner mesh, add it to the mesher
        mesher_inner = self.get_parameter("SETTING_AXISYMMETRIC_INNER")
        if mesher_inner is not None:
            innerNode = ET.SubElement(mesher, "inner")
            innerNode.set("type", "axisymmetric")
            innerNode.set("template", mesher_inner)

        # Coarse inner, similarly
        mesher_inner_coarse = self.get_parameter("SETTING_AXISYMMETRIC_INNER_COARSE")
        if mesher_inner_coarse is not None:
            innerNode = ET.SubElement(mesher, "inner")
            innerNode.set("type", "axisymmetric")
            innerNode.set("name", "coarse")
            innerNode.set("template", mesher_inner_coarse)

        # The extent we assume is a sphere of radius in parameters
        extent = ET.SubElement(mesher, 'extent')
        radius = self.get_parameter("SIMULATION_DOMAIN_RADIUS")
        if radius is not None:
            extent.set('radius', str(radius))
        else:
            extent.set('radius', '50')

        # Adding the empty centre element tells the mesher we want a denser
        # centre than the boundaries
        ET.SubElement(mesher, 'centre')

        # Start going through the length scales
        lengthscales = ET.SubElement(mesher, 'lengthscales')

        # Two sets of fairly sensible defaults for the usual meshing case
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

        # Allow them to be overridden
        nearfield = self.get_parameter('RESOLUTION_FIELD_NEAR')
        needlezonefield = self.get_parameter('RESOLUTION_FIELD_NEEDLE_ZONE')
        if not needlezonefield:
            needlezonefield = self.get_parameter('RESOLUTION_NEEDLE_ZONE_FIELD')
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

        farfield = lengthscale_settings[1][1]
        zonefield = lengthscale_settings[2][1]

        # Each region may need to be added to the mesher section
        for idx, region in self._regions.items():
            # If we have an organ, it should appear as a zone or organ
            if region['meaning'] == 'organ':
                if self.get_parameter('SETTING_ORGAN_AS_SUBDOMAIN'):
                    zone = ET.SubElement(mesher, 'zone')
                    zone.set('region', idx)
                    zone.set('priority', '100')
                    zone.set('characteristic_length', farfield)
                else:
                    ET.SubElement(mesher, 'organ').set('region', idx)
            # If we have a zone, not excluded from meshing, then it goes in too
            elif region['format'] == 'zone' and not (set(region['groups']) & self._nonmeshing_groups):
                zone = ET.SubElement(mesher, 'zone')
                zone.set('region', idx)
                zone.set('priority', '1')
                zone.set('characteristic_length', zonefield)
            # If we have vessels, they get added also
            elif 'vessels' in region['groups'] or 'bronchi' in region['groups']:
                # FIXME: surely this should be a surface/vessel tag?
                zone = ET.SubElement(mesher, 'zone')
                zone.set('region', idx)
                zone.set('priority', '2')
                zone.set('characteristic_length', zonefield)

        # These are standard entries
        ET.SubElement(root, 'optimizer')
        ET.SubElement(root, 'elmergrid')

        # The register of needles must be filled in
        globalNeedlesNode = ET.SubElement(root, "needles")

        if not needlezonefield:
            needlezonefield = zonefield

        for ix, needle in self._needles.items():
            # Add a needle node and set the name to be our index (if we have
            # been given, say, 'needle-3' as an index, it becomes '3')
            globalNeedleNode = ET.SubElement(globalNeedlesNode, "needle")
            l = int(ix.replace('needle', ''))
            globalNeedleNode.set("name", str(l))

            # If this needle is a boundary type (the only type for the
            # moment)...
            if needle['class'] in ('solid-boundary', 'boundary'):
                # The 'file' attribute in GSSA should be a colon-separated pair
                # indicating what type of definition it is and the specifics
                # required
                location = needle['file'].split(':', 1)

                needle_mesh = None
                # If we aren't using a library type, then we need to get the
                # region
                if location[0] in ('surface', 'zone', 'both'):
                    needleNode = ET.SubElement(regions, location[0])
                    needleNode.set("name", str(l))
                    needleNode.set("input", os.path.join("input/", location[1]))
                    needleNode.set("groups", "needles")

                    # TODO: surely this might be a surface?
                    needle_mesh = ET.SubElement(mesher, 'zone')
                    needle_mesh.set('region', str(l))
                    needle_mesh.set('characteristic_length', str(needlezonefield))
                    needle_mesh.set('priority', '0')
                else:
                    needleNode = ET.SubElement(needlelibrary, 'needle')

                    if name_needle_regions:
                        needleNode.set("name", str(l))

                    # If this is a library type, set its ID
                    if location[0] == 'library':
                        needleNode.set("id", location[1])
                    needleNode.set("name", str(l))

                    # Calculate the offset and axis for this needle
                    tip_location = self.get_needle_parameter(ix, "NEEDLE_TIP_LOCATION")
                    entry_location = self.get_needle_parameter(ix, "NEEDLE_ENTRY_LOCATION")
                    needleNode.set("offset", " ".join(map(lambda c: str(c[0] - c[1]), zip(tip_location, centre_location))))
                    needleNode.set("axis", " ".join(map(lambda c: str(c[0] - c[1]), zip(entry_location, tip_location))))

                    # Add any needle-specific parameters
                    parameters = ET.SubElement(globalNeedleNode, "parameters")
                    for key, parameterPair in needle["parameters"].items():
                        parameter, typ = parameterPair
                        parameterNode = ET.SubElement(parameters, "constant")
                        parameterNode.set("name", key)
                        parameterNode.set("value", str(convert_parameter(parameter, typ)))

                # Set active region if needs be
                needle_active_length = self.get_needle_parameter(ix, "NEEDLE_ACTIVE_LENGTH")
                global_active_length = self.get_needle_parameter(ix, "CONSTANT_GLOBAL_ACTIVE_LENGTH")
                if needle_active_length is None:
                    needle_active_length = global_active_length
                if needle_active_length is not None:
                    if needle_mesh is None:
                        needle_mesh = ET.SubElement(mesher, 'zone')
                        needle_mesh.set('characteristic_length', str(needlezonefield))
                        needle_mesh.set('priority', '0')
                        needle_mesh.set('region', 'needle-' + str(l))
                    activity = ET.SubElement(needle_mesh, 'activity')
                    tip_location = self.get_needle_parameter(ix, "NEEDLE_TIP_LOCATION")
                    for c, vt, vc in zip(('x', 'y', 'z'), tip_location, centre_location):
                        activity.set(c, str(vt - vc))
                    activity.set('r', str(needle_active_length))

        self._mesher_xml = root

        return self._mesher_xml
