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
SUBROUTINE MaxESolver( Model,Solver,Timestep,TransientSimulation)
      USE DefUtils

      IMPLICIT None

      TYPE(Model_t) :: Model
      TYPE(Solver_t) :: Solver
      REAL(KIND=dp) :: TimeStep
      LOGICAL :: TransientSimulation

      TYPE(Element_t), POINTER :: Element
      TYPE(Variable_t), POINTER :: JouleHeatingVar, ElectricConductivityVar
      TYPE(ValueList_t), POINTER :: Material
      REAL(KIND=dp), POINTER :: JouleHeating(:), &
          MaxE(:), MaxEPrev(:)
      REAL(KIND=dp), ALLOCATABLE :: ElectricConductivity(:)
      REAL(KIND=dp), POINTER :: ElectricConductivityVector(:), E(:)
      REAL(KIND=dp) :: jh, cond
      INTEGER, POINTER :: JouleHeatingPerm(:), ElectricConductivityPerm(:), &
          MaxEPerm(:)
      INTEGER, ALLOCATABLE :: ElementNodes(:)
      INTEGER :: i, N, NodeCount, k
      LOGICAL :: AllocationsDone = .FALSE., Found

      SAVE ElectricConductivity, E, ElectricConductivityVector, ElementNodes

      JouleHeatingVar => VariableGet(Solver % Mesh % Variables, "Joule Heating")
      JouleHeating => JouleHeatingVar % Values
      JouleHeatingPerm => JouleHeatingVar % Perm

      MaxE => Solver % Variable % Values
      MaxEPerm => Solver % Variable % Perm
      MaxEPrev => Solver % Variable % PrevValues(:,1)

      N = SIZE(MaxEPerm)

      IF (.NOT. AllocationsDone) THEN
          ALLOCATE(ElectricConductivity(1:Solver % Mesh % MaxElementNodes))
          ALLOCATE(ElectricConductivityVector(1:N))
          ALLOCATE(E(1:N))
          ALLOCATE(ElementNodes(1:Solver % Mesh % MaxElementNodes))

          CALL VariableAdd(Solver % Mesh % Variables, Solver % Mesh, &
              Solver, 'Electric Conductivity', 1, &
              ElectricConductivityVector, MaxEPerm)

          CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
              Solver, 'E', 1, &
              E, MaxEPerm )

          AllocationsDone = .TRUE.
      END IF

      DO i=1,Solver % NumberOfActiveElements
        Element => GetActiveElement(i)
        NodeCount = GetElementNOFNodes()
        Material => GetMaterial()

        ! Inefficient but clear.
        ElectricConductivity = ListGetReal(Material, "Electric Conductivity", &
            NodeCount, Element % NodeIndexes, Found)

        DO k=1,NodeCount
            ElectricConductivityVector(MaxEPerm(Element % NodeIndexes(k))) = ElectricConductivity(k)
        END DO
      END DO

      DO i=1, N
        jh = JouleHeating(JouleHeatingPerm(i))
        cond = ElectricConductivityVector(MaxEPerm(i))
        E(MaxEPerm(i)) = SQRT(jh / cond)
        MaxE(MaxEPerm(i)) = MAX(MaxE(MaxEPerm(i)), MaxEPrev(MaxEPerm(i)), E(MaxEPerm(i)))
      END DO
END SUBROUTINE
