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
MODULE HeatAddGlobalTime
 CONTAINS
  SUBROUTINE AddGlobalTime(Solver, dt)
       USE DefUtils
       USE MaterialModels
       USE HeatPhaseDefs
       IMPLICIT NONE

       TYPE(Solver_t) :: Solver
       REAL(KIND=dp) :: dt
  !------------------------------------------------------------------------------
       INTEGER :: i,j,k,n
       REAL(KIND=dp) :: FORCE(1)
       REAL(KIND=dp), POINTER :: SaveValues(:) => NULL()
       SAVE STIFF, MASS, X
       REAL(KIND=dp), ALLOCATABLE :: STIFF(:,:),MASS(:,:), X(:,:)

       IF ( .NOT.ASSOCIATED(Solver % Variable % Values, SaveValues) ) THEN
          IF ( ALLOCATED(STIFF) ) DEALLOCATE( STIFF,MASS,X )
          n = 0
          DO i=1,Solver % Matrix % NumberOfRows
            n = MAX( n,Solver % Matrix % Rows(i+1)-Solver % Matrix % Rows(i) )
          END DO
          k = SIZE(Solver % Variable % PrevValues,2)
          ALLOCATE( STIFF(1,n),MASS(1,n),X(n,k) )

          SaveValues => Solver % Variable % Values
       END IF

       DO i=1,Solver % Matrix % NumberOFRows
         n = 0
         DO j=Solver % Matrix % Rows(i),Solver % Matrix % Rows(i+1)-1
           n=n+1
           STIFF(1,n) = Solver % Matrix % Values(j)
           MASS(1,n)  = Solver % Matrix % MassValues(j)
           X(n,:) = Solver % Variable % PrevValues(Solver % Matrix % Cols(j),:)
         END DO
         FORCE(1) = Solver % Matrix % RHS(i)
         Solver % Matrix % Force(i,1) = FORCE(1)
         k = MIN( Solver % DoneTime, Solver % Order )
         CALL BDFLocal( n, dt, MASS, STIFF, FORCE, X, k )

         n = 0
         DO j=Solver % Matrix % Rows(i),Solver % Matrix % Rows(i+1)-1
            n=n+1
           Solver % Matrix % Values(j) = STIFF(1,n)
         END DO
         Solver % Matrix % RHS(i) = FORCE(1)
       END DO
  !------------------------------------------------------------------------------
  END SUBROUTINE AddGlobalTime
END MODULE HeatAddGlobalTime
