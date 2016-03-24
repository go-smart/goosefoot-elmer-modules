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
!  Author: Panchatcharam Mariappan
!  Author: Phil Weir
SUBROUTINE AlternatingBCSolver( Model,Solver,Timestep,TransientSimulation)
      USE DefUtils

      IMPLICIT None

      TYPE(Model_t) :: Model
      TYPE(Solver_t) :: Solver
      REAL(KIND=dp) :: TimeStep, condition
      REAL(KIND=dp), POINTER :: anode(:)
      LOGICAL :: TransientSimulation,FLAG

      TYPE(Element_t), POINTER :: BoundaryElement
      TYPE(ValueList_t), POINTER :: BC, Simulation
      INTEGER :: i, j, k,ks,maxN, time, tag, partn, bndry, left, right, &
          nodes(MAX_ELEMENT_NODES), eio_info, TYPE
      REAL(KIND=dp), POINTER :: coord(:,:)
      INTEGER, POINTER :: body_list(:)
      LOGICAL :: Found, AlternatingBoundary
      LOGICAL, POINTER :: anodeLogic(:)

      Simulation => GetSimulation()

      DO j = 1, Model % NumberOfBCs
          condition = GetConstReal(Model % BCs(j) % Values, &
              'Alternating Boundary Condition', Found)
          IF (Found) THEN
              body_list => ListGetIntegerArray(Model % BCs(j)%Values, "Body Id")
              DO i = Solver % Mesh % NumberOfBulkElements + 1, &
                        Solver % Mesh % NumberOfBulkElements + &
                        Solver % Mesh % NumberOfBoundaryElements

                  BoundaryElement => Solver % Mesh % Elements(i)
                  ! The BodyId is set in MeshUtils and (for a boundary)
                  ! is /dictated/ by the Body Id in the Boundary Condition
                  ! that applies to it.
                  bndry = BoundaryElement % BodyId
                  k = SIZE(body_list)
                  DO ks = 1, k
                    IF (body_list(ks) == bndry) THEN
                        IF (condition > 0.0) THEN
                          BoundaryElement % BoundaryInfo % Constraint = j
                        ELSE
                          BoundaryElement % BoundaryInfo % Constraint = 0
                        END IF
                    END IF
                  END DO
              END DO
          END IF
      END DO

END SUBROUTINE AlternatingBCSolver
