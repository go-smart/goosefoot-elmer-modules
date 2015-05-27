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
/**************************************************************************/
/* 28/05/2013
/* 																																				*/
/* 	This program computes the maximum distance from the nodes of predicted*/ 
/*  lesion surface to segmented lesion surface 														*/	
/* 																																				*/
/* 	The surfaces are defined in polydata format.                          */
/* 																																				*/
/* 	The user has to enter the names of each file (segmented and predicted)*/
/* 																																				*/
/* 																																				*/
/* To compile this file:																									*/
/*    Go to the Path																											*/
/* 		cmake .																															*/
/* 		make																																*/
/* 		sudo make install or make install 																	*/
/*    If it is not with sudo make install, export the path of             */
/*    surface-deviation                                                   */
/*                                                                        */
/* Execution: 																														*/
/*    surface-deviation segmentedlesion predictedlesion                   */
/*    Please do not give the .vtk extension at the end of the file.       */
/**************************************************************************/


#include <vtkSmartPointer.h>
#include <vtkSphereSource.h>
#include <vtkCellLocator.h>

#include "vtkPolyData.h"
#include "vtkPolyDataReader.h"
#include "vtkPointData.h"

#include <vtkFloatArray.h>
#include <vtkPolyDataWriter.h>

using namespace std;

int main(int argc,const char *argv[])
{
	
	/*********************************************************************************/ 
	// Read info about segmented lesion:
	/*********************************************************************************/

	// Get the name of the vtk file for segmented lesion:
//	cout << "=======================================" << endl;
//	cout << "Name of the vtk file for segmented lesion? (without extension)" << endl;
	string segfn;
  segfn=argv[1];
//	cin >> segfn;
	segfn += ".vtk";
	const char *segfilename;
	segfilename = segfn.c_str();
	cout << "Name of the vtk file for segmented lesion:" << segfn << endl;
//	 Check that the vtk file is present:
	ifstream segtestfile(segfilename, ios::in);
	segtestfile.close();
	if(segtestfile.fail()){
		cout << "vtk file for segmented lesion not found. Check name!" << endl;
		cout << "=======================================" << endl;
		cout << "Default vtk file (p42-rfa-lesion1.vtk) used for segmented lesion" << endl;
		segfn = "p42-rfa-lesion1.vtk";
		segfilename = segfn.c_str();
		//return(0);
	}
	// Create polydata reader for file:
	vtkPolyDataReader *segpdreader=vtkPolyDataReader::New();
	// Create a polydata object for file:
	vtkPolyData *segpdata =vtkPolyData::New();
	//Set the polydata selection file as the file to be read: 
	segpdreader->SetFileName(segfilename);
	segpdreader->Update();	
	// Get the data in polydata object:
	segpdata = segpdreader->GetOutput();

  // Create the tree
  vtkSmartPointer<vtkCellLocator> cellLocator = vtkSmartPointer<vtkCellLocator>::New();
  cellLocator->SetDataSet(segpdreader->GetOutput());
  cellLocator->BuildLocator();

	/*********************************************************************************/ 
	// Read info about predicted lesion:
	/*********************************************************************************/

	// Get the name of the vtk file for predicted lesion:
//	cout << "=======================================" << endl;
//	cout << "Name of the vtk file for predicted lesion? (without extension)" << endl;
	string numfn;
  numfn=argv[2];
//	cin >> numfn;
	numfn += ".vtk";
	const char *numfilename;
	numfilename = numfn.c_str();
	cout << "Name of the vtk file for predicted lesion:" << numfn << endl;
	// Check that the vtk file is present:
	ifstream numtestfile(numfilename, ios::in);
	numtestfile.close();
	if(numtestfile.fail()){
		cout << "vtk file for predicted lesion not found. Check name!" << endl;
		cout << "=======================================" << endl;
		return(0);
	}

	// Create polydata reader for file:
	vtkPolyDataReader *numpdreader=vtkPolyDataReader::New();
	// Create a polydata object for file:
	vtkPolyData *numpdata =vtkPolyData::New();
	//Set the polydata selection file as the file to be read: 
	numpdreader->SetFileName(numfilename);
	numpdreader->Update();	
	// Get the data in polydata object:
	numpdata = numpdreader->GetOutput();

	//Create a points object:
	vtkPoints* numpoints = vtkPoints::New();
	long nbnumPts; // number of nodes
	int i=0;

	//Get the points (nodes) in polydata object:
	numpoints = numpdata->GetPoints();
	nbnumPts = numpoints->GetNumberOfPoints();

	/*********************************************************************************/
	// Compute the maximum distance between a point of predicted lesion and segmented lesion
	/*********************************************************************************/

  double testnumPoint[3];//the coordinates of the point to test
  double closestsegPoint[3];//the coordinates of the closest point will be returned here
  double closestPointDist2; //the squared distance to the closest point will be returned here
  vtkIdType cellId; //the cell id of the cell containing the closest point will be returned here
  int subId; //this is rarely used (in triangle strips only, I believe)
  double maxclosestPointDist2=0;//maximum squared distance from a point to the closest point on segmented surface
  double maxnumPoint[3];//the coordinates of the point for which the distance is max
  double maxclosestsegPoint[3];//the coordinates of the closest point from maxpoint

  double minclosestPointDist2=1000;//minimum squared distance from a point to the closest point on segmented surface
  double minnumPoint[3];//the coordinates of the point for which the distance is min
  double minclosestsegPoint[3];//the coordinates of the closest point from minpoint

  // Add distances to each point for plot
  vtkSmartPointer<vtkFloatArray> distances = vtkSmartPointer<vtkFloatArray>::New();
  distances->SetNumberOfComponents(1);
  distances->SetName("Distances");

	//Go through the points of predicted surface:
	for(int i = 0; i < nbnumPts; i++)
	{
		//Get the point coordinates:
		numpoints->GetPoint(i,testnumPoint);
		
		//Test for translated segmented lesion:
		//testnumPoint[0] = testnumPoint[0];
		//testnumPoint[1] = testnumPoint[1]+5;
		//testnumPoint[2] = testnumPoint[2]-5;

		//Compute the closest point on segmented surface abd distance to it:
  	cellLocator->FindClosestPoint(testnumPoint, closestsegPoint, cellId, subId, closestPointDist2);
 
  	//cout << "Coordinates of closest point: " << closestsegPoint[0] << " " << closestsegPoint[1] << " " << closestsegPoint[2] << endl;
  	//cout << "Squared distance to closest point: " << closestPointDist2 << endl;
  	//cout << "CellId: " << cellId << endl;

		//Compare distance to min distance:
		if(closestPointDist2 > maxclosestPointDist2){ 
			maxclosestPointDist2=closestPointDist2;
			maxnumPoint[0]=testnumPoint[0];
			maxnumPoint[1]=testnumPoint[1]; //test for translated segmented lesion!
			maxnumPoint[2]=testnumPoint[2]; //test for translated segmented lesion!
			maxclosestsegPoint[0]=closestsegPoint[0];
			maxclosestsegPoint[1]=closestsegPoint[1];
			maxclosestsegPoint[2]=closestsegPoint[2];
		}

		//Compare distance to min distance:
		if(closestPointDist2 < minclosestPointDist2){ 
			minclosestPointDist2=closestPointDist2;
			minnumPoint[0]=testnumPoint[0];
			minnumPoint[1]=testnumPoint[1]; //test for translated segmented lesion!
			minnumPoint[2]=testnumPoint[2]; //test for translated segmented lesion!
			minclosestsegPoint[0]=closestsegPoint[0];
			minclosestsegPoint[1]=closestsegPoint[1];
			minclosestsegPoint[2]=closestsegPoint[2];
		}

		//Save distance for plot
		distances->InsertNextValue(sqrt(closestPointDist2));
	}
	
	//Print max distance:
	cout << "Max distance to closest point: " << sqrt(maxclosestPointDist2) << endl;
	cout << "Max distance obtained for point: " << maxnumPoint[0] << " " << maxnumPoint[1] << " " << maxnumPoint[2] << endl;
	cout << "Closest point on surface: " << maxclosestsegPoint[0] << " " << maxclosestsegPoint[1] << " " << maxclosestsegPoint[2] << endl;

	//Print min distance:
	cout << "Min distance to closest point: " << sqrt(minclosestPointDist2) << endl;
	cout << "Min distance obtained for point: " << minnumPoint[0] << " " << minnumPoint[1] << " " << minnumPoint[2] << endl;
	cout << "Closest point on surface: " << minclosestsegPoint[0] << " " << minclosestsegPoint[1] << " " << minclosestsegPoint[2] << endl;

	//Computes the average distance:
	double aveDistance = 0.0;
	for(int i = 0; i < distances->GetNumberOfTuples(); i++) aveDistance = aveDistance + distances->GetValue(i);
	aveDistance = aveDistance/distances->GetNumberOfTuples();
	cout << "Average deviation = " << aveDistance << endl;


 	//Create a new vtk file with the distance to segmented lesion for each point:
  	numpdata->GetPointData()->AddArray(distances);
	vtkSmartPointer<vtkPolyDataWriter> numpwriter = vtkSmartPointer<vtkPolyDataWriter>::New();
  	numpwriter->SetInput(numpdata);
	const char *distnumfilename;
//	numfn = "dist-"+numfn;
//	distnumfilename = numfn.c_str();
// 	numpwriter->SetFileName(distnumfilename);
//  	numpwriter->Write();


	/*********************************************************************************/
  // Delete the created objects:
	/*********************************************************************************/
	segpdreader->Delete();
	numpdreader->Delete();
	/*********************************************************************************/
	// Finish the program:
	/*********************************************************************************/
	cout << "=======================================" << endl;
	cout << "End of program." << endl;
	cout << "=======================================" << endl;
  return 0;
}
