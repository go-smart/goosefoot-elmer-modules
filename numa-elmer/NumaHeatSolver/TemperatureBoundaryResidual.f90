! *****************************************************************************/
! *  This file is based on the file "HeatSolve.f90" of Elmer (Finite Element 
! *  Software for Multiphysical Problems)
! *  Authors: Juha Ruokolainen
! *  Copyright 1st April 1995 - , CSC - Scientific Computing Ltd., Finland
! *  The original file is relicensed, as permitted by the authors, as GPLv3
! *****************************************************************************/
! /**
!  * Modifications to this file are part of the Go-Smart Simulation Architecture (GSSA).
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
!  *
!  * These alterations may be re-licensed under the terms of the GNU General
!  * Public License version 2 (or later version) ONLY for inclusion in the
!  * publicly available Elmer project by CSC, except by express consent of
!  * NUMA Engineering Services Ltd.
!  */
! 


!------------------------------------------------------------------------------
FUNCTION TemperatureBoundaryResidual( Model, Edge, Mesh, Quant, Perm,Gnorm, NDOFs ) RESULT( Indicator )
!------------------------------------------------------------------------------
!******************************************************************************
!
! Compute an indicator concerning boundary edges for remeshing process
!
! ARGUMENTS:
!
!  TYPE(Model_t) :: Model,  
!     INPUT: All model information (mesh,materials,BCs,etc...)
!
!  TYPE(Element_t), POINTER :: Edge
!       INPUT: Structure describing the element (dimension,nof nodes,
!               interpolation degree, etc...)
!
!  TYPE( Mesh_t ), POINTER :: Mesh
!     INPUT: Current mesh
!
!   REAL(KIND=dp) :: Quant(:)
!       INPUT: Quantity computed on the mesh
!
!   INTEGER :: Perm(:)
!       INPUT: Element local numbering
!
!   REAL(KIND=dp) :: Gnorm
!       OUTPUT: volumic force norm
!
!   REAL(KIND=dp) :: Indicator(2)
!       OUTPUT: Remeshing criterion
!
!  INTEGER :: NDOFs
!   INPUT: Degrees of freedom of the solution Quant
!******************************************************************************
    USE ElementDescription
    IMPLICIT NONE
    !------------------------------------------------------------------------------
    TYPE(Model_t) :: Model
    INTEGER :: Perm(:), NDOFs
    REAL(KIND=dp) :: Quant(:), Indicator(2), Gnorm
    TYPE( Mesh_t ), POINTER    :: Mesh
    TYPE( Element_t ), POINTER :: Edge
    !------------------------------------------------------------------------------
    Gnorm = 0.0
    Indicator = 0.0
!------------------------------------------------------------------------------
END FUNCTION TemperatureBoundaryResidual
!------------------------------------------------------------------------------
