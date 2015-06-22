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
SUBROUTINE AlternatingBCSolver( Model,Solver,Timestep,TransientSimulation)
      USE DefUtils

      IMPLICIT None

      TYPE(Model_t) :: Model
      TYPE(Solver_t) :: Solver
      REAL(KIND=dp) :: TimeStep
      LOGICAL :: TransientSimulation

      TYPE(Element_t), POINTER :: BoundaryElement
      TYPE(ValueList_t), POINTER :: BC, Simulation
      INTEGER :: i, j, maxN, step, tag, partn, bndry, left, right, &
          nodes(MAX_ELEMENT_NODES), eio_info, bodyid
      REAL(KIND=dp), POINTER :: coord(:,:)
      INTEGER, POINTER :: anode(:), cathode(:)
      LOGICAL :: AllocationsDone = .FALSE., Found, AlternatingBoundary

      IF (.NOT. AllocationsDone) THEN
          maxN = Model % Solver % Mesh % MaxElementNodes
          ALLOCATE(anode(maxN), cathode(maxN))

          AllocationsDone = .TRUE.
      END IF

      Simulation => GetSimulation()

      anode => ListGetIntegerArray(Simulation, 'Anode', Found)
      cathode => ListGetIntegerArray(Simulation, 'Cathode', Found)

      IF (SIZE(anode) /= SIZE(cathode)) THEN
          CALL Fatal('AlternatingBCSolver', &
              'Number of alternations unclear - &
              &anode and cathode counts differ')
      END IF

      step = MIN(SIZE(anode), GetTimestep())

      DO i = Solver % Mesh % NumberOfBulkElements + 1, &
             Solver % Mesh % NumberOfBulkElements + &
             Solver % Mesh % NumberOfBoundaryElements

        BoundaryElement => Solver % Mesh % Elements(i)
        bndry = BoundaryElement % BodyId

        DO j = 1, Model % NumberOfBCs
          AlternatingBoundary = GetLogical(Model % BCs(j) % Values, &
                     'Alternating Boundary Condition', Found )
          IF ( Found .AND. AlternatingBoundary ) THEN
              bodyid = ListGetInteger(Model % BCs(j) % Values, &
                         'Body Id', Found)
              IF ( .NOT. Found ) THEN
                  CALL Fatal('AlternatingBCSolver', &
                      'Alternating boundary condition must have a body id')
              END IF
              IF ( bndry == bodyid ) THEN
                  IF (anode(step) == bndry .OR. cathode(step) == bndry) THEN
                      BoundaryElement % BoundaryInfo % Constraint = j
                  ELSE
                      BoundaryElement % BoundaryInfo % Constraint = 0
                  END IF
              END IF
          END IF
        END DO
      END DO
END SUBROUTINE AlternatingBCSolver
