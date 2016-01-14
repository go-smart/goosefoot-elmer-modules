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
MODULE HeatAssemblyNeumann
  USE HeatAddHeatFluxBC
 CONTAINS
  SUBROUTINE NeumannAssembly(Solver, Model, ForceVector, ForceHeater, &
          NewtonLinearization, TransientHeaterControl, &
          IsRadiation, TempSol, TransientAssembly, HeaterControlLocal, &
          ConstantBulk, dt)

      USE MaterialModels
      USE DefUtils
      USE NumaAdaptive
      IMPLICIT NONE

      TYPE(Solver_t) :: Solver
      TYPE(Model_t) :: Model
      TYPE(Variable_t) :: TempSol
      REAL(KIND=dp), ALLOCATABLE :: ForceHeater(:)
      REAL(KIND=dp), POINTER :: ForceVector(:)
      LOGICAL :: HeaterControlLocal, TransientAssembly, ConstantBulk
      REAL(KIND=dp) :: dt

      TYPE(Nodes_t) :: ElementNodes
      LOGICAL :: NewtonLinearization, IsRadiation, AllocationsDone = .FALSE., &
          Found, HeatGapBC, HeatFluxBC, &
          TransientHeaterControl
      CHARACTER(LEN=MAX_NAME_LEN) :: VarName
      TYPE(ValueList_t), POINTER :: BC
      TYPE(Element_t), POINTER :: Element
      INTEGER, ALLOCATABLE :: SaveIndexes(:), Indexes(:)
      INTEGER, POINTER :: TempPerm(:)
      INTEGER :: t, nd, n

      SAVE Indexes, SaveIndexes, AllocationsDone

      IF ( .NOT. AllocationsDone .OR. Solver % Mesh % Changed ) THEN
          CALL Allocations
      END IF

      VarName = GetVarName( TempSol )
      TempPerm => TempSol % Perm

      DO t=1, Solver % Mesh % NumberOfBoundaryElements
        Element => GetBoundaryElement(t)
        IF ( .NOT. ActiveBoundaryElement() ) CYCLE

        n = GetElementNOFNodes()

        ! Check that the dimension of element is suitable for fluxes
        IF( .NOT. PossibleFluxElement(Element) ) CYCLE

        BC => GetBC()
        IF ( .NOT. ASSOCIATED(BC) ) CYCLE

        ! This checks whether there are any Dirichlet conditions on the
        ! smart heater boundary. If there are the r.h.s. must be zero as
        ! there can possibly not be any effect on temperature.
        !-----------------------------------------------------------------
        IF ( HeaterControlLocal .AND. .NOT. TransientHeaterControl) THEN
          IF( ListCheckPresent(BC, Varname) ) THEN
             nd = GetElementDOFs(Indexes)
             ForceHeater(TempPerm(Indexes(1:nd))) = 0.0_dp
          END IF
        END IF

        HeatFluxBC = GetLogical( BC, 'Heat Flux BC', Found )
        IF ( Found .AND. .NOT. HeatFluxBC ) CYCLE
!        WRITE(*,*) 'HeatFluxBC', 
        HeatGapBC = ListGetLogical( BC, 'Heat Gap', Found )

        CALL AddHeatFluxBC(Model, Solver, Element, ElementNodes, ForceVector, ConstantBulk, &
           NewtonLinearization, HeatGapBC, n, IsRadiation, TempSol, TransientAssembly, dt)

        IF ( HeatGapBC ) THEN
          CALL FindGapIndexes( Solver, Element, ElementNodes, Indexes, n )
          SaveIndexes(1:n) = Element % NodeIndexes
          Element % NodeIndexes = Indexes(1:n)
          CALL AddHeatFluxBC(Model, Solver, Element, ElementNodes, ForceVector, ConstantBulk, &
             NewtonLinearization, HeatGapBC, n, IsRadiation, TempSol, TransientAssembly, dt)
          Element % NodeIndexes = SaveIndexes(1:n)
        END IF

      END DO   ! Neumann & Newton BCs
    CONTAINS
        SUBROUTINE Allocations
            INTEGER :: N, istat

            N = Solver % Mesh % MaxElementDOFs

            IF (AllocationsDone) THEN
                DEALLOCATE( &
                    Indexes, &
                    SaveIndexes, &
                )
            END IF

            ALLOCATE( &
                Indexes(N), &
                SaveIndexes(N), &
                STAT=istat &
            )

            AllocationsDone = .TRUE.

            IF ( istat /= 0 ) THEN
              CALL Fatal( 'HeatSolve', 'Memory allocation error' )
            END IF

            AllocationsDone = .TRUE.
        END SUBROUTINE Allocations
  END SUBROUTINE NeumannAssembly
END MODULE HeatAssemblyNeumann
