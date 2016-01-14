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
FUNCTION AlternatingBoundaryCondition(Model, n, time) RESULT(potential)
      USE DefUtils

      IMPLICIT None

      TYPE(Model_t) :: Model
      INTEGER :: n
      REAL(KIND=dp) :: time, potential

      TYPE(Element_t), POINTER :: BoundaryElement
      TYPE(ValueList_t), POINTER :: BC, Simulation
      INTEGER :: bc_id, nboundary, maxN, step
      LOGICAL :: Found
      INTEGER, POINTER :: anode(:), cathode(:)
      REAL(KIND=dp), POINTER :: potential_values(:,:)

      BoundaryElement => Model % CurrentElement
      BC => GetBC()
      Simulation => GetSimulation()

      bc_id = BoundaryElement % BodyId
      nboundary = GetElementNOFNodes(BoundaryElement)

      anode => ListGetIntegerArray(Simulation, 'Anode', Found)
      cathode => ListGetIntegerArray(Simulation, 'Cathode', Found)
      potential_values => ListGetConstRealArray(Simulation, &
            'Potential Consecutive Values', Found)

      ! Check the current step
      step = MIN(SIZE(potential_values, 2), GetTimestep())
      IF (step < 1) THEN
          potential = 0
          RETURN
      END IF

      ! If this boundary element is an anode, we
      ! take from the PCV top row, if a cathode
      ! then the bottom row
      IF (bc_id == anode(step)) THEN
          potential = potential_values(1, step)
      ELSE IF (bc_id == cathode(step)) THEN
          potential = potential_values(2, step)
      END IF

END FUNCTION AlternatingBoundaryCondition
