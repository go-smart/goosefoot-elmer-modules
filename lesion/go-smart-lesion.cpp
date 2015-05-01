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

/* REQUIRES PARAVIEW HEADERS
#include "vtkIsoVolume.h"
*/

#include <tinyxml2.h>

namespace po = boost::program_options;

int main(int argc, char *argv[])
{
  float threshold_lower, threshold_upper, scaling_value;
  bool connected_component = false, subdivide = false, parallel = false, using_upper = false, using_lower = false,
       threshold_not_isovolume = true;
  int smoothing_iterations = 0;
  std::string input_vtu("in.vtu"), output_vtk("out.vtk"), field("dead"), analysis_xml("analysis.xml");

  tinyxml2::XMLDocument doc;
  tinyxml2::XMLElement* rootNode = doc.NewElement("gosmartAnalysis");
  doc.InsertEndChild(rootNode);

  po::options_description options_description("Allowed options");
  options_description.add_options()
    ("help,h,?", "produce help message")
    ("threshold-lower,t", po::value<float>(&threshold_lower), "threshold for chosen variable (remove cells with values below this limit)")
    ("threshold-upper,t", po::value<float>(&threshold_upper), "threshold for chosen variable (remove cells with values above this limit)")    
    ("scale,S", po::value<float>(&scaling_value)->default_value(1), "pre-scaling of results; default 1")
    ("field,f", po::value<std::string>(&field), "field to threshold on")
    ("parallel,p", po::value(&parallel)->zero_tokens(), "assume input data is PVTU not VTU")
    /*("threshold,p", po::value(&threshold_not_isovolume)->zero_tokens(), "switch from using an IsoVolume to using a Threshold")*/
    ("connectivity,c", po::value(&connected_component)->zero_tokens(), "extract largest connected component of thresholded surface")
    ("subdivide,s", po::value(&subdivide)->zero_tokens(), "subdivide before thresholding")
    ("smoothing-iterations,i", po::value<int>(&smoothing_iterations)->default_value(0), "number of iterations in smoother (0 to skip)")
    ("input,i", po::value<std::string>(&input_vtu), "input volume mesh file")
    ("analysis,a", po::value<std::string>(&analysis_xml), "analysis output file")
    ("output,o", po::value<std::string>(&output_vtk), "output file");

  po::variables_map vm;
  po::store(po::parse_command_line(argc, argv, options_description), vm);
  po::notify(vm);

  if (vm.count("help")) {
    std::cout << "NUMA Lesion Cutter - 0.1" << std::endl;
    std::cout << options_description << std::endl;
    return 1;
  }

  using_upper = vm.count("threshold-upper") > 0;
  using_lower = vm.count("threshold-lower") > 0;

  std::cout << input_vtu << std::endl;

  tinyxml2::XMLElement* inputVtuNode = doc.NewElement("inputVtu");
  tinyxml2::XMLText* inputVtuText = doc.NewText(input_vtu.c_str());
  rootNode->InsertEndChild(inputVtuNode);
  inputVtuNode->InsertEndChild(inputVtuText);

  vtkSmartPointer<vtkUnstructuredGrid> grid;

  if (parallel) {
      vtkSmartPointer<vtkXMLPUnstructuredGridReader> reader =
          vtkSmartPointer<vtkXMLPUnstructuredGridReader>::New();

      reader->SetFileName(input_vtu.c_str());
      reader->Update();

      grid = reader->GetOutput();
  
  } else {
      vtkSmartPointer<vtkXMLUnstructuredGridReader> reader =
          vtkSmartPointer<vtkXMLUnstructuredGridReader>::New();

      reader->SetFileName(input_vtu.c_str());
      reader->Update();

      grid = reader->GetOutput();
  
  }

  std::cout << "There are " << grid->GetNumberOfCells() 
            << " cells before thresholding." << std::endl;

  tinyxml2::XMLElement* totalCellsNode = doc.NewElement("totalCells");
  tinyxml2::XMLText* totalCellsText = doc.NewText(std::to_string(grid->GetNumberOfCells()).c_str());
  rootNode->InsertEndChild(totalCellsNode);
  totalCellsNode->InsertEndChild(totalCellsText);

  vtkSmartPointer<vtkTransform> transform =
      vtkSmartPointer<vtkTransform>::New();
  transform->Scale(scaling_value, scaling_value, scaling_value);

  vtkSmartPointer<vtkTransformFilter> transformfilter =
	  vtkSmartPointer<vtkTransformFilter>::New();
  transformfilter->SetInput(grid);
  transformfilter->SetTransform(transform);
  transformfilter->Update();

  vtkSmartPointer<vtkUnstructuredGrid> grid_scaled = transformfilter->GetUnstructuredGridOutput();
  
  vtkSmartPointer<vtkDescriptiveStatistics> statisticsfilter =
    vtkSmartPointer<vtkDescriptiveStatistics>::New();
  vtkTable* statisticsinput = vtkTable::New();
  statisticsinput->Update();
  statisticsfilter->SetInput(statisticsinput);
  std::map<vtkIdType, std::string> xml_variables;
  if (grid_scaled->GetPointData()->GetArray("dead"))
  {
      statisticsinput->AddColumn(grid_scaled->GetPointData()->GetArray("dead"));
      statisticsfilter->AddColumn("dead");
      xml_variables[xml_variables.size()] = "dead";
  }
  if (grid_scaled->GetPointData()->GetArray("temperature"))
  {
      statisticsinput->AddColumn(grid_scaled->GetPointData()->GetArray("temperature"));
      statisticsfilter->AddColumn("temperature");
      xml_variables[xml_variables.size()] = "temperature";
  }
  statisticsfilter->Update();

  vtkMultiBlockDataSet* statisticsmultiblock = vtkMultiBlockDataSet::SafeDownCast(statisticsfilter->GetOutputDataObject(vtkStatisticsAlgorithm::OUTPUT_MODEL));
  vtkTable* statisticstable_prim = vtkTable::SafeDownCast(statisticsmultiblock->GetBlock(0));
  statisticstable_prim->Dump();
  vtkTable* statisticstable_deri = vtkTable::SafeDownCast(statisticsmultiblock->GetBlock(1));
  statisticstable_deri->Dump();

  std::map<std::string, std::string> primary_statistics;
  primary_statistics["minimum"] = "Minimum";
  primary_statistics["maximum"] = "Maximum";
  primary_statistics["mean"] = "Mean";
  std::map<std::string, std::string> secondary_statistics;
  secondary_statistics["standardDeviation"] = "Standard Deviation";
  secondary_statistics["kurtosis"] = "Kurtosis";
  tinyxml2::XMLElement* variableNode;
  tinyxml2::XMLElement* dataNode;
  tinyxml2::XMLText* dataText;

  for (std::map<vtkIdType, std::string>::const_iterator it = xml_variables.begin(); it != xml_variables.end(); it++) {
      variableNode = doc.NewElement(it->second.c_str());
      for (std::map<std::string, std::string>::const_iterator it2 = primary_statistics.begin(); it2 != primary_statistics.end(); it2++) {
          dataNode = doc.NewElement(it2->first.c_str());
          const vtkVariant& var = statisticstable_prim->GetValueByName(it->first, it2->second.c_str());
          dataText = doc.NewText(var.ToString());
          dataNode->InsertEndChild(dataText);
          variableNode->InsertEndChild(dataNode);
      }
      for (std::map<std::string, std::string>::const_iterator it2 = secondary_statistics.begin(); it2 != secondary_statistics.end(); it2++) {
          dataNode = doc.NewElement(it2->first.c_str());
          const vtkVariant& var = statisticstable_deri->GetValueByName(it->first, it2->second.c_str());
          dataText = doc.NewText(var.ToString());
          dataNode->InsertEndChild(dataText);
          variableNode->InsertEndChild(dataNode);
      }
      rootNode->InsertEndChild(variableNode);
  }

  vtkSmartPointer<vtkUnstructuredGrid> thresholded_grid;
  if (threshold_not_isovolume)
  {
      vtkSmartPointer<vtkThreshold> threshold = 
        vtkSmartPointer<vtkThreshold>::New();
      threshold->SetInput(grid_scaled);

      if (using_upper && using_lower)
      {
          threshold->ThresholdBetween(threshold_lower, threshold_upper);
          std::cout << "Thresholded between limits : " << threshold_lower << " and " << threshold_upper << std::endl;
      }
      else if (using_upper)
      {
          threshold->ThresholdByLower(threshold_upper); /* Yes, but this is the only way ThresholdBetween also makes sense */
          std::cout << "Thresholded by upper limit : " << threshold_upper << std::endl;
      }
      else if (using_lower)
      {
          threshold->ThresholdByUpper(threshold_lower);
          std::cout << "Thresholded by lower limit : " << threshold_lower << std::endl;
      }
      threshold->SetInputArrayToProcess(0, 0, 0, vtkDataObject::FIELD_ASSOCIATION_POINTS, field.c_str());
      threshold->Update();

      thresholded_grid = threshold->GetOutput();
  }
  else
  {
      /*
      vtkSmartPointer<vtkIsoVolume> isoVolume = 
        vtkSmartPointer<vtkIsoVolume>::New();
      isoVolume->SetInput(grid_scaled);

      if (using_upper && using_lower)
          isoVolume->ThresholdBetween(threshold_lower, threshold_upper);
      else if (using_upper)
          isoVolume->ThresholdBetween(-1e20, threshold_upper);
      else if (using_lower)
          isoVolume->ThresholdBetween(threshold_lower, 1e20);
      isoVolume->SetInputArrayToProcess(0, 0, 0, vtkDataObject::FIELD_ASSOCIATION_POINTS, field.c_str());
      isoVolume->Update();

      thresholded_grid = vtkUnstructuredGrid::SafeDownCast(isoVolume->GetOutput());
      */
  }

  // doesn't work because the array is not added as SCALARS, i.e. via SetScalars
  // threshold->SetInputArrayToProcess(0, 0, 0, vtkDataObject::FIELD_ASSOCIATION_CELLS, vtkDataSetAttributes::SCALARS);
  // use like this:
  std::cout << "There are " << thresholded_grid->GetNumberOfCells() 
            << " cells after thresholding." << std::endl;

  tinyxml2::XMLElement* thresholdCellsNode = doc.NewElement("thresholdCells");
  tinyxml2::XMLText* thresholdCellsText = doc.NewText(std::to_string(grid->GetNumberOfCells()).c_str());
  rootNode->InsertEndChild(thresholdCellsNode);
  thresholdCellsNode->InsertEndChild(thresholdCellsText);

  vtkIdType cell_ct = grid->GetNumberOfCells(), curr_cell = 0;
  vtkTetra *tetra;

  vtkSmartPointer<vtkSelectionNode> selectionNode =
	  vtkSmartPointer<vtkSelectionNode>::New();
  selectionNode->SetFieldType(vtkSelectionNode::CELL);
  selectionNode->SetContentType(vtkSelectionNode::INDICES);

  vtkSmartPointer<vtkIdTypeArray> selectionArray =
	  vtkSmartPointer<vtkIdTypeArray>::New();
  selectionArray->SetNumberOfComponents(1);

  double vol = 0;
  //double pt0[3] = {0., 0., 1.};
  //double pt1[3] = {0., 1., 0.};
  //double pt2[3] = {1., 0., 0.};
  //double pt3[3] = {0., 0., 0.};
  double pt0[3] = {0, 0, 0};
  double pt1[3] = {0, 0, 0};
  double pt2[3] = {0, 0, 0};
  double pt3[3] = {0, 0, 0};
  for ( vtkIdType i = 0 ; i < cell_ct ; i++ ) {
	  tetra = vtkTetra::SafeDownCast(grid->GetCell(i));
	  if (!tetra) {
		  continue;
	  }
	  selectionArray->InsertNextValue(i);
  }

  cell_ct = thresholded_grid->GetNumberOfCells(), curr_cell = 0;
  for ( vtkIdType i = 0 ; i < cell_ct ; i++ ) {
	  tetra = vtkTetra::SafeDownCast(thresholded_grid->GetCell(i));
	  if (!tetra) {
		  continue;
	  }

	  thresholded_grid->GetPoint(tetra->GetPointId(0), pt0);
	  thresholded_grid->GetPoint(tetra->GetPointId(1), pt1);
	  thresholded_grid->GetPoint(tetra->GetPointId(2), pt2);
	  thresholded_grid->GetPoint(tetra->GetPointId(3), pt3);
	  vol += vtkTetra::ComputeVolume(pt0, pt1, pt2, pt3);
  }
  selectionNode->SetSelectionList(selectionArray);

  std::cout << "The volume of the thresholded region is" << std::endl << "LVOL: " << vol << std::endl;

  if (subdivide) {
	  vtkSmartPointer<vtkSelection> selection =
		  vtkSmartPointer<vtkSelection>::New();
	  selection->AddNode(selectionNode);

	  vtkSmartPointer<vtkExtractSelectedIds> extractSelectedIds =
		  vtkSmartPointer<vtkExtractSelectedIds>::New();
	  extractSelectedIds->SetInput(0, grid);
	  extractSelectedIds->SetInput(1, selection);
	  extractSelectedIds->Update();

	  vtkSmartPointer<vtkSubdivideTetra> subdivided =
		  vtkSmartPointer<vtkSubdivideTetra>::New();
	  subdivided->SetInput(extractSelectedIds->GetOutput());

	  vtkSmartPointer<vtkUnstructuredGrid> subdivided_grid = subdivided->GetOutput();
	  subdivided_grid->Update();

	  vtkSmartPointer<vtkThreshold> threshold_subdivided = 
	    vtkSmartPointer<vtkThreshold>::New();
	  threshold_subdivided->SetInput(subdivided_grid);
      if (using_upper && using_lower)
          threshold_subdivided->ThresholdBetween(threshold_lower, threshold_upper);
      else if (using_upper)
          threshold_subdivided->ThresholdByUpper(threshold_upper);
      else if (using_lower)
          threshold_subdivided->ThresholdByLower(threshold_lower);
	  // doesn't work because the array is not added as SCALARS, i.e. via SetScalars
	  // threshold->SetInputArrayToProcess(0, 0, 0, vtkDataObject::FIELD_ASSOCIATION_CELLS, vtkDataSetAttributes::SCALARS);
	  // use like this:
	  threshold_subdivided->SetInputArrayToProcess(0, 0, 0, vtkDataObject::FIELD_ASSOCIATION_POINTS, field.c_str());
	  threshold_subdivided->Update();
	  vtkSmartPointer<vtkUnstructuredGrid> threshold_subdivided_grid = threshold_subdivided->GetOutput();
	  vtkSmartPointer<vtkXMLUnstructuredGridWriter> ugw =
		  vtkSmartPointer<vtkXMLUnstructuredGridWriter>::New();
	  ugw->SetInput(threshold_subdivided_grid);
	  ugw->SetFileName("subdivided.vtu");
	  ugw->Write();

	  vol = 0;
	  cell_ct = threshold_subdivided_grid->GetNumberOfCells();

	  for ( vtkIdType i = 0 ; i < cell_ct ; i++ ) {
		  tetra = vtkTetra::SafeDownCast(threshold_subdivided_grid->GetCell(i));
		  if (!tetra) {
			  continue;
		  }

		  threshold_subdivided_grid->GetPoint(tetra->GetPointId(0), pt0);
		  threshold_subdivided_grid->GetPoint(tetra->GetPointId(1), pt1);
		  threshold_subdivided_grid->GetPoint(tetra->GetPointId(2), pt2);
		  threshold_subdivided_grid->GetPoint(tetra->GetPointId(3), pt3);
		  vol += fabs(vtkTetra::ComputeVolume(pt0, pt1, pt2, pt3));
	  }

	  std::cout << "The volume of the subdivided region is" << std::endl << "LVOL2: " << vol << std::endl;

	  //vtkSmartPointer<vtkSubdivideTetra> subdivided_2 =
	  //        vtkSmartPointer<vtkSubdivideTetra>::New();
	  //subdivided_2->SetInput(subdivided_grid);

	  //vtkSmartPointer<vtkUnstructuredGrid> subdivided_grid_2 = subdivided_2->GetOutput();
	  //subdivided_grid_2->Update();

	  //vtkSmartPointer<vtkThreshold> threshold_subdivided_2 =
	  //  vtkSmartPointer<vtkThreshold>::New();
	  //threshold_subdivided_2->SetInput(subdivided_grid_2);
	  //threshold_subdivided_2->ThresholdByUpper(threshold_value);
	  //// doesn't work because the array is not added as SCALARS, i.e. via SetScalars
	  //// threshold->SetInputArrayToProcess(0, 0, 0, vtkDataObject::FIELD_ASSOCIATION_CELLS, vtkDataSetAttributes::SCALARS);
	  //// use like this:
	  //threshold_subdivided_2->SetInputArrayToProcess(0, 0, 0, vtkDataObject::FIELD_ASSOCIATION_POINTS, field.c_str());
	  //threshold_subdivided_2->Update();
	  //vtkSmartPointer<vtkUnstructuredGrid> threshold_subdivided_grid_2 = threshold_subdivided_2->GetOutput();
	  //vtkSmartPointer<vtkXMLUnstructuredGridWriter> ugw_2 =
	  //        vtkSmartPointer<vtkXMLUnstructuredGridWriter>::New();
	  //ugw_2->SetInput(threshold_subdivided_grid_2);
	  //ugw_2->SetFileName("subdivided2.vtu");
	  //ugw_2->Write();

	  //vol = 0;
	  //cell_ct = threshold_subdivided_grid_2->GetNumberOfCells();

	  //for ( vtkIdType i = 0 ; i < cell_ct ; i++ ) {
	  //        tetra = vtkTetra::SafeDownCast(threshold_subdivided_grid_2->GetCell(i));
	  //        if (!tetra) {
	  //      	  continue;
	  //        }

	  //        threshold_subdivided_grid_2->GetPoint(tetra->GetPointId(0), pt0);
	  //        threshold_subdivided_grid_2->GetPoint(tetra->GetPointId(1), pt1);
	  //        threshold_subdivided_grid_2->GetPoint(tetra->GetPointId(2), pt2);
	  //        threshold_subdivided_grid_2->GetPoint(tetra->GetPointId(3), pt3);
	  //        vol += fabs(vtkTetra::ComputeVolume(pt0, pt1, pt2, pt3));
	  //}

	  //std::cout << "The volume of the subdivided region is" << std::endl << "LVOL3: " << vol << std::endl;

	  thresholded_grid = threshold_subdivided_grid;

  }
  vtkSmartPointer<vtkDataSetSurfaceFilter> surface_filter =
      vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
  surface_filter->SetInput(thresholded_grid);

  vtkSmartPointer<vtkPolyData> polydata;

  if (smoothing_iterations > 0) {
      std::cout << "Smoothing for " << smoothing_iterations << std::endl;
      vtkSmartPointer<vtkWindowedSincPolyDataFilter> smoother =
          vtkSmartPointer<vtkWindowedSincPolyDataFilter>::New();
      smoother->SetInput(surface_filter->GetOutput());
      smoother->SetNumberOfIterations(10);
      smoother->FeatureEdgeSmoothingOff();
      smoother->SetFeatureAngle(120.0);
      smoother->SetPassBand(0.001);
      smoother->NonManifoldSmoothingOn();
      smoother->NormalizeCoordinatesOn();
      smoother->Update();

      polydata =
          smoother->GetOutput();
  } else {
      std::cout << "Skipping smoother" << std::endl;
      polydata = surface_filter->GetOutput();
  }

  if (connected_component) {
	  vtkSmartPointer<vtkPolyDataConnectivityFilter> connectivity =
		  vtkSmartPointer<vtkPolyDataConnectivityFilter>::New();

	  connectivity->SetInput(polydata);
	  connectivity->SetExtractionModeToLargestRegion();
	  connectivity->Update();
	  polydata = connectivity->GetOutput();
  }

  vtkSmartPointer<vtkXMLPolyDataWriter> writer =
      vtkSmartPointer<vtkXMLPolyDataWriter>::New();

  writer->SetFileName(output_vtk.c_str());
  writer->SetInput(polydata);
  writer->Write();

  doc.SaveFile(analysis_xml.c_str());
  
  return EXIT_SUCCESS;
}
