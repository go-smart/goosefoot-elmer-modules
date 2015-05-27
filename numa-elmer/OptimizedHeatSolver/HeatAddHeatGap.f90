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
MODULE HeatAddHeatGap
  USE HeatFindGapIndexes
 CONTAINS
  SUBROUTINE AddHeatGap( Solver, Element, ElementNodes, STIFF, TempSol, n)
    USE MaterialModels
    USE DefUtils
    USE NumaAdaptive
    IMPLICIT NONE

!----------------------------------------------------------------------------
    TYPE(Solver_t) :: Solver
    TYPE(Nodes_t) :: ElementNodes
    TYPE(Variable_t) :: TempSol
    REAL(KIND=dp) :: STIFF(:,:)
    INTEGER :: n
    TYPE(Element_t) :: Element
!----------------------------------------------------------------------------
    INTEGER :: i,j,k,l, Ind(n)
    INTEGER, POINTER :: TempPerm(:)
    CHARACTER(LEN=MAX_NAME_LEN) :: VarName
!----------------------------------------------------------------------------
    TempPerm => TempSol % Perm
    VarName = GetVarName( TempSol ) 
    CALL FindGapIndexes( Solver, Element, ElementNodes, Ind, n )
    DO i=1,n
      DO j=1,n
        k = TempPerm( Element % NodeIndexes(i) )
        l = TempPerm( Ind(j) )
        IF ( k > 0 .AND. l > 0 ) THEN
          CALL AddToMatrixElement( Solver % Matrix,k,l,-STIFF(i,j) )
        END IF
      END DO
    END DO
!----------------------------------------------------------------------------
  END SUBROUTINE AddHeatGap
END MODULE HeatAddHeatGap
