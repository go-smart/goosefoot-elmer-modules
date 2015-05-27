! /**
!  * This file is part of the Go-Smart Simulation Architecture (GSSA).
!  * Go-Smart is an EU-FP7 project, funded by the European Commission.
!  *
!  * Copyright (C) 2013-  NUMA Engineering Ltd. (see AUTHORS file)
!  *
!  * This program is free software: you can redistribute it and/or modify
!  * it under the terms of the GNU Affero General Public License as
!  * published by the Free Software Foundation, either version 3 of the
!  * License, or (at your option) any later version.
!  *
!  * This program is distributed in the hope that it will be useful,
!  * but WITHOUT ANY WARRANTY; without even the implied warranty of
!  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  * GNU General Public License for more details.
!  *
!  * You should have received a copy of the GNU Affero General Public License
!  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
!  */
! 
!------------------------------------------------------------------------------
!
!   Definition of time/space dependent Boundary Conditions, 
!   Initial Conditions, and source terms
!------------------------------------------------------------------------------
! *****************************************************************************/
! *  12/11/09: 
! *  - Obsolete subroutines deleted
! *****************************************************************************/
! *****************************************************************************/
! * List of subroutines:
! *****************************************************************************/
!       - NumaElectricPower
!       - NumaSourceSink
! *****************************************************************************/
! *****************************************************************************/
! *  18/12/09: 
! *  - Obsolete subroutines deleted
! *  - Function NumaReadElectricTips() added
! *  - Function NumaReadSourceSink() added
! *****************************************************************************/
! *****************************************************************************/
! *  08/04/10: 
! *  - Use of Fortran constant Pi in NumaReadElectricTips() and NumaReadSourceSink()
! *  - In NumaReadElectricTips(), concatenation of TipsFile with ".txt" extension 
! *  (the name of the file must be entered without extension in input file)
! *****************************************************************************/
! *****************************************************************************/
! *  19/04/10: 
! *  - Modification of NumaReadElectricTips() to apply different electric power 
! *    at tips ends and middles 
! *  - Addition of functions NumaReadEndElectricTips() and NumaReadMiddleElectricTips()
! *****************************************************************************/
! *****************************************************************************/
! *  14/05/10: 
! *  - Modification of NumaReadElectricTips(),NumaReadEndElectricTips() and  
! *    NumaReadMiddleElectricTips() to catch End Of File exception
! *****************************************************************************/
! *****************************************************************************/
! *  24/05/10: 
! *  - Modification of NumaReadElectricTips(): check if we use an interpolated line 
! *    source and return 0 in this case
! *  - Modification of NumaReadElectricTips(), NumaReadEndElectricTips() and  
! *    NumaReadMiddleElectricTips(): read file name from keyword "Electric 
! *    Tips Filename Root" in input file
! *  - Modification of NumaReadElectricTips(), NumaReadEndElectricTips(),  
! *    NumaReadMiddleElectricTips(), and NumaReadSourceSink(): if text files not 
! *    found or name not specified, stop the simulation and print error messages
! *****************************************************************************/
! *****************************************************************************/
! *  11/06/10: 
! *  - Modification of NumaReadElectricTips(), NumaReadEndElectricTips(),   
! *    NumaReadMiddleElectricTips(), and NumaReadSourceSink(): set z=0 and  
! *    modify coefficient of gaussian for 2D
! *****************************************************************************/
! *****************************************************************************/
! *  09/07/10: 
! *  - Addition of fcts NumaReadElectricTipsVTK(), NumaReadEndElectricTipsVTK(),   
! *    NumaReadMiddleElectricTipsVTK(), to read source points in VTK format 
! *    
! *****************************************************************************/
! *****************************************************************************/
! *  02/12/10: 
! *  - Modification of
! *         -NumaReadElectricTips()   
! *         -NumaReadEndElectricTips()  
! *         -NumaReadMiddleElectricTips()  
! *         -NumaReadElectricTipsVTK()  
! *         -NumaReadEndElectricTipsVTK()  
! *         -NumaReadMiddleElectricTipsVTK()  
! *   to treat variable location of tips points   
! *****************************************************************************/
! *****************************************************************************/
! *  24/01/11: 
! *  - New routine NumaReadElectricPower()
! *         -Read in a text file the values "Time Power" (only the first timestep)   
! *         -Save these values in arrays  
! *         -Compute at each timestep the power by linear interpolation (call of 
! *         InterpolateCurve() (GeneralUtils.src)  
! *         -The name of the text file is specified in Simulation section of input file
! *****************************************************************************/
! *****************************************************************************/
! *  25/01/11: 
! *  - Default value of Sigma variable set to 3.0
! *  - Default value of InterpolatedLineSource set to .FALSE.
! *  - Default value of MultiPower set to .TRUE.
! *****************************************************************************/
! *****************************************************************************/
! *  15/02/11: 
! *  - Modification of
! *         -NumaReadElectricTips()   
! *         -NumaReadEndElectricTips()  
! *         -NumaReadMiddleElectricTips()  
! *         -NumaReadElectricTipsVTK()  
! *         -NumaReadEndElectricTipsVTK()  
! *         -NumaReadMiddleElectricTipsVTK()  
! *    variable TipsLocationTimesArray set as allocatable and deallocated after use
! *****************************************************************************/
! *****************************************************************************/
! *  28/06/11: 
! *  - New routine NumaReadPerfusionMap():
! *    - Allow perfusion coeff variation in spherical zones
! *    - Center coordinates, radius (sigma), and amplitude (coeff) of the 
! *      variation are specified in a text file
! *    - Gaussian variation is used when sigma>0, and a sharp variation is used 
! *      sigma<0
! *    -The variation is added to the average value specified in input file
! *****************************************************************************/
! *****************************************************************************/
! *  04/07/11: 
! *  - Modification to allow non uniform power distribution between the tips:
! *  - In NumaReadElectricTips(), NumaReadEndElectricTips(), NumaReadMiddleElectricTips(): 
! *    - New variable NonUniformPower (logical) read in input file
! *    - CoeffTip(:) (real), values are read in text files containing tips coordinates,
! *      used to multily the power source
! *****************************************************************************/
