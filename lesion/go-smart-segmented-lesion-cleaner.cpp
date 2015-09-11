/**
 * This file is part of the Go-Smart Simulation Architecture (GSSA).
 * Go-Smart is an EU-FP7 project, funded by the European Commission.
 *
 * Copyright (C) 2013-  NUMA Engineering Ltd. (see AUTHORS file)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <boost/program_options.hpp>
#include <string>

// CONVERTED FROM VTK : http://www.vtk.org/Wiki/VTK/Examples/Cxx/PolyData/ThresholdCells
#include <vtkVersion.h>
#include <vtkDataSetSurfaceFilter.h>
#include <vtkXMLPolyDataWriter.h>
#include <vtkXMLPolyDataReader.h>
#include <vtkPolyData.h>
#include <vtkUnstructuredGridWriter.h>
#include <vtkIntArray.h>
#include <vtkCellData.h>
#include <vtkTriangle.h>
#include <vtkCellArray.h>
#include <vtkPoints.h>
#include <vtkPolyData.h>
#include <vtkXMLPUnstructuredGridReader.h>
#include <vtkXMLUnstructuredGridReader.h>
#include <vtkXMLUnstructuredGridWriter.h>
#include <vtkUnstructuredGrid.h>
#include <vtkDataSet.h>
#include <vtkPointData.h>
#include <vtkSmartPointer.h>
#include <vtkThreshold.h>
#include <vtkMeshQuality.h>
#include <vtkTetra.h>
#include <vtkSubdivideTetra.h>
#include <vtkIdTypeArray.h>
#include <vtkSelectionNode.h>
#include <vtkSelection.h>
#include <vtkExtractSelectedIds.h>
#include <vtkTransform.h>
#include <vtkTransformFilter.h>
#include <vtkWindowedSincPolyDataFilter.h>
#include <vtkPolyDataConnectivityFilter.h>
#include <vtkMultiBlockDataSet.h>
#include <vtkTable.h>
#include <vtkDescriptiveStatistics.h>
#include <vtkDataObject.h>
#include <vtkGeometryFilter.h>

/* REQUIRES PARAVIEW HEADERS
#include "vtkIsoVolume.h"
*/

#include <tinyxml2.h>

namespace po = boost::program_options;

int main(int argc, char *argv[])
{
    vtkSmartPointer<vtkXMLPolyDataReader> reader =
        vtkSmartPointer<vtkXMLPolyDataReader>::New();
    reader->SetFileName("refdata.vtp");
    reader->Update();

    vtkSmartPointer<vtkPolyData> polydata = reader->GetOutput();

    vtkSmartPointer<vtkPolyDataConnectivityFilter> connectivity =
    vtkSmartPointer<vtkPolyDataConnectivityFilter>::New();

    connectivity->SetInput(polydata);
    connectivity->SetExtractionModeToLargestRegion();
    connectivity->Update();
    polydata = connectivity->GetOutput();

    vtkSmartPointer<vtkXMLPolyDataWriter> writer =
      vtkSmartPointer<vtkXMLPolyDataWriter>::New();

    writer->SetFileName("refdata-clean.vtp");
    writer->SetInput(polydata);
    writer->Write();

    return EXIT_SUCCESS;
}
