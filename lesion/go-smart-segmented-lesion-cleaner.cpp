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

#include <CGAL/Exact_integer.h>
#include <CGAL/Homogeneous.h>
#include <CGAL/Polyhedron_3.h>
#include <CGAL/Polyhedral_mesh_domain_3.h>
#include <CGAL/IO/Polyhedron_iostream.h>
#include <CGAL/Nef_polyhedron_3.h>
#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Exact_predicates_exact_constructions_kernel.h>
#include <CGAL/IO/Nef_polyhedron_iostream_3.h>
#include <iostream>

typedef CGAL::Exact_predicates_inexact_constructions_kernel K;
typedef CGAL::Polyhedron_3<K> Polyhedron;
typedef CGAL::Exact_predicates_exact_constructions_kernel Exact_Kernel;
typedef CGAL::Polyhedron_3<Exact_Kernel> Exact_polyhedron;
typedef CGAL::Polyhedral_mesh_domain_3<Exact_polyhedron, Exact_Kernel> Exact_Mesh_domain;
typedef CGAL::Nef_polyhedron_3<Exact_Kernel,
			       CGAL::SNC_indexed_items,
			       bool> Nef_polyhedron; 

typedef Exact_Kernel::Point_3 Exact_Point;
typedef Exact_polyhedron::HalfedgeDS Exact_HalfedgeDS;

#include "PolyhedronUtils.h"

/* REQUIRES PARAVIEW HEADERS
#include "vtkIsoVolume.h"
*/

#include <tinyxml2.h>

namespace po = boost::program_options;

int main(int argc, char *argv[])
{
    std::string input_vtp("refdata.vtp"), output_vtp("refdata_clean.vtp"), organ_vtp;
    bool connected_component = false;

    po::options_description options_description("Allowed options");
    options_description.add_options()
      ("help,h,?", "produce help message")
      ("input,i", po::value<std::string>(&input_vtp), "input segmented ablation zone (lesion) VTP")
      ("connectivity,c", po::value(&connected_component)->zero_tokens(), "extract largest connected component of thresholded surface")
      ("organ,O", po::value<std::string>(&organ_vtp), "organ VTP; indicates intersection with organ should be taken")
      ("output,o", po::value<std::string>(&output_vtp), "output segmented ablation zone (lesion) VTP");
    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, options_description), vm);
    po::notify(vm);

    if (vm.count("help")) {
      std::cout << "NUMA Segmented Lesion Cleaner - 0.1" << std::endl;
      std::cout << options_description << std::endl;
      return 1;
    }

    std::cout << "Outputting to " << output_vtp << std::endl;

    /*
    Exact_polyhedron P, Q;
    PolyhedronUtils::readSurfaceFile(organ_vtp.c_str(), P);
    PolyhedronUtils::readSurfaceFile(input_vtp.c_str(), P);
    if(P.is_closed()) {
        Nef_polyhedron N1(P);
        Nef_polyhedron N2(Q);
        N1 *= N2;
        if(N1.is_simple()) {
          N1.convert_to_polyhedron(P);
          std::cout << P;
        }
        else {
          std::cerr << "N1 is not a 2-manifold." << std::endl;
        }
    }
*/
    vtkSmartPointer<vtkXMLPolyDataReader> vtp_reader =
        vtkSmartPointer<vtkXMLPolyDataReader>::New();
    vtp_reader->SetFileName(input_vtp.c_str());
    vtp_reader->Update();

    vtkSmartPointer<vtkPolyData> polydata = vtp_reader->GetOutput();

    vtkSmartPointer<vtkPolyDataConnectivityFilter> connectivity =
    vtkSmartPointer<vtkPolyDataConnectivityFilter>::New();

    if (connected_component)
    {
        connectivity->SetInput(polydata);
        connectivity->SetExtractionModeToLargestRegion();
        connectivity->Update();
        polydata = connectivity->GetOutput();
    }

    vtkSmartPointer<vtkXMLPolyDataWriter> vtp_writer =
      vtkSmartPointer<vtkXMLPolyDataWriter>::New();

    vtp_writer->SetFileName(output_vtp.c_str());
    vtp_writer->SetInput(polydata);
    vtp_writer->Write();

    return EXIT_SUCCESS;
}
