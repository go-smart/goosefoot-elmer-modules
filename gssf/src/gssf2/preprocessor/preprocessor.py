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
import vtk
import subprocess

delta = 1


class Preprocessor:
    def __init__(self, runname, input_file, output_file, radius, x, y, z):
        self.runname = runname

        reader = vtk.vtkPolyDataReader()
        reader.SetFileName(input_file)
        reader.Update()

        planes = vtk.vtkPlaneCollection()

        top = vtk.vtkPlane()
        top.SetNormal(0, 0, -1)
        top.SetOrigin(x, y, z + radius)
        planes.AddItem(top)

        bottom = vtk.vtkPlane()
        bottom.SetNormal(0, 0, 1)
        bottom.SetOrigin(x, y, z - radius)
        planes.AddItem(bottom)

        front = vtk.vtkPlane()
        front.SetNormal(0, -1, 0)
        front.SetOrigin(x, y + radius, z)
        planes.AddItem(front)

        back = vtk.vtkPlane()
        back.SetNormal(0, 1, 0)
        back.SetOrigin(x, y - radius, z)
        planes.AddItem(back)

        right = vtk.vtkPlane()
        right.SetNormal(1, 0, 0)
        right.SetOrigin(x - radius, y, z)
        planes.AddItem(right)

        left = vtk.vtkPlane()
        left.SetNormal(-1, 0, 0)
        left.SetOrigin(x + radius, y, z)
        planes.AddItem(left)

        clipper = vtk.vtkClipClosedSurface()
        clipper.SetClippingPlanes(planes)
        clipper.SetInputConnection(reader.GetOutputPort())

        connectivity = vtk.vtkPolyDataConnectivityFilter()
        connectivity.SetExtractionModeToAllRegions()
        connectivity.SetInputConnection(clipper.GetOutputPort())
        connectivity.Update()

        specified = vtk.vtkPolyDataConnectivityFilter()
        specified.SetExtractionModeToSpecifiedRegions()
        specified.SetInputConnection(clipper.GetOutputPort())
        tester = vtk.vtkPolyDataConnectivityFilter()
        tester.SetExtractionModeToSpecifiedRegions()
        tester.SetInputConnection(clipper.GetOutputPort())

        dims = {}
        for i in range(connectivity.GetNumberOfExtractedRegions()):
            tester.AddSpecifiedRegion(i)
            tester.Update()
            bounds = tester.GetOutput().GetBounds()

            if bounds[0] + radius - delta < x or bounds[1] - radius + delta > x or \
               bounds[2] + radius - delta < y or bounds[3] - radius + delta > y or \
               bounds[4] + radius - delta < z or bounds[5] - radius + delta > z:
                    mindim = min(bounds[1] - bounds[0], bounds[3] - bounds[2], bounds[5] - bounds[4])
                    dims[i] = mindim

            tester.DeleteSpecifiedRegion(i)

        for i in sorted(dims.iteritems(), key=lambda i: i[1], reverse=True)[0:5]:
            print i
            specified.AddSpecifiedRegion(i[0])

        #cleaner = vtk.vtkCleanPolyData()
        #cleaner.SetInputConnection(specified.GetOutputPort())
        #cleaner.SetTolerance(1e-5)

        #orientator = vtk.vtkPolyDataNormals()
        #orientator.SetInputConnection(clipper.GetOutputPort())
        #orientator.ConsistencyOn()
        #orientator.AutoOrientNormalsOn()

        writer = vtk.vtkSTLWriter()
        writer.SetFileName(output_file + 'tmp1.stl')
        writer.SetInputConnection(clipper.GetOutputPort())
        writer.Write()

        with open("/tmp/test.mlx", "w") as f:
            f.write("""
<FilterScript>
 <filter name="Remove Zero Area Faces"/>
 <filter name="Re-Orient all faces coherentely"/>
 <filter name="Quadric Edge Collapse Decimation">
  <Param type="RichInt" value="30000" name="TargetFaceNum"/>
  <Param type="RichFloat" value="0" name="TargetPerc"/>
  <Param type="RichFloat" value="0.3" name="QualityThr"/>
  <Param type="RichBool" value="false" name="PreserveBoundary"/>
  <Param type="RichBool" value="false" name="PreserveNormal"/>
  <Param type="RichBool" value="true" name="PreserveTopology"/>
  <Param type="RichBool" value="true" name="OptimalPlacement"/>
  <Param type="RichBool" value="false" name="PlanarQuadric"/>
  <Param type="RichBool" value="false" name="QualityWeight"/>
  <Param type="RichBool" value="true" name="AutoClean"/>
  <Param type="RichBool" value="false" name="Selected"/>
 </filter>
</FilterScript>
           """)
        subprocess.call(["meshlabserver", "-i", output_file + 'tmp1.stl', "-o", output_file + '.off', "-s", "/tmp/test.mlx"])

        #reader = vtk.vtkSTLReader()
        #reader.SetFileName(output_file + 'tmp2.stl')
        #reader.Update()

        writer = vtk.vtkSTLWriter()
        writer.SetFileName(output_file)
        writer.SetInputConnection(specified.GetOutputPort())
        writer.Write()

        vtk_writer = vtk.vtkXMLPolyDataWriter()
        vtk_writer.SetFileName(output_file + ".vtu")
        vtk_writer.SetInputConnection(specified.GetOutputPort())
        vtk_writer.Write()
