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
      TYPE(Variable_t), POINTER :: JouleHeatingVar, ElectricConductivityVar, &
          SurvivalVar
      TYPE(ValueList_t), POINTER :: Material
      REAL(KIND=dp), POINTER :: JouleHeating(:), &
          MaxE(:), MaxEPrev(:)
      REAL(KIND=dp), ALLOCATABLE :: ElectricConductivity(:)
      REAL(KIND=dp), POINTER :: ElectricConductivityVector(:), E(:), S(:)
      REAL(KIND=dp) :: jh, cond, E0, A0, K1, K2
      INTEGER, POINTER :: JouleHeatingPerm(:), ElectricConductivityPerm(:), &
          MaxEPerm(:)
      INTEGER, ALLOCATABLE :: ElementNodes(:)
      INTEGER :: i, N, NodeCount, k, j, PulseNumber
      LOGICAL :: AllocationsDone = .FALSE., Found

      SAVE ElectricConductivity, E, S, ElectricConductivityVector, ElementNodes

      ! Get the Joule Heating from the, say, electric potential solver
      JouleHeatingVar => VariableGet(Solver % Mesh % Variables, "Joule Heating")
      JouleHeating => JouleHeatingVar % Values
      JouleHeatingPerm => JouleHeatingVar % Perm

      ! MaxE is our own variable
      MaxE => Solver % Variable % Values
      MaxEPerm => Solver % Variable % Perm
      MaxEPrev => Solver % Variable % PrevValues(:,1)

      ! We also output a survival variable, based on the pulse number
      SurvivalVar => VariableGet(Solver % Mesh % Variables, "Survival")
      S => SurvivalVar % Values

      N = SIZE(MaxEPerm)
      PulseNumber = GetInteger(Solver % Values, 'Pulse Number', Found)

      ! From Garcia et al, 2014 (http://lbk.fe.uni-lj.si/pdfs/plos2014.pdf)
      ! Note that these numbers are not based on human in vivo tissue, and
      ! better numbers should be used when research is available
      E0 = GetConstReal(Solver % Values, 'E0', Found)
      IF (.NOT. Found) THEN
          E0 = 399600.0
      END IF

      A0 = GetConstReal(Solver % Values, 'A0', Found)
      IF (.NOT. Found) THEN
          A0 = 144100.0
      END IF

      K1 = GetConstReal(Solver % Values, 'K1', Found)
      IF (.NOT. Found) THEN
          K1 = 0.03
      END IF

      K2 = GetConstReal(Solver % Values, 'K2', Found)
      IF (.NOT. Found) THEN
          K2 = 0.06
      END IF

      IF (.NOT. AllocationsDone) THEN
          ALLOCATE(ElectricConductivity(1:Solver % Mesh % MaxElementNodes))
          ALLOCATE(ElectricConductivityVector(1:N))
          ALLOCATE(E(1:N), S(1:N))
          ALLOCATE(ElementNodes(1:Solver % Mesh % MaxElementNodes))

          ! Output updated electric conducitivity
          CALL VariableAdd(Solver % Mesh % Variables, Solver % Mesh, &
              Solver, 'Electric Conductivity', 1, &
              ElectricConductivityVector, MaxEPerm)

          ! Also output the present energy deposition
          CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
              Solver, 'E', 1, &
              E, MaxEPerm )

          AllocationsDone = .TRUE.
      END IF

      ! For each element, update the element-wise conductivity
      DO i=1,Solver % NumberOfActiveElements
        Element => GetActiveElement(i)
        NodeCount = GetElementNOFNodes()
        Material => GetMaterial()

        ! Inefficient but clear.
        ElectricConductivity = ListGetReal(Material, "Electric Conductivity", &
            NodeCount, Element % NodeIndexes, Found)

        DO k=1,NodeCount
            j = Element % NodeIndexes(k)

            ElectricConductivityVector(MaxEPerm(j)) = ElectricConductivity(k)
        END DO
      END DO

      ! Now go through nodes, and update the non-material-dependent variables nodally
      DO j = 1, SIZE(MaxEPerm)
        jh = JouleHeating(MaxEPerm(j))
        cond = ElectricConductivityVector(MaxEPerm(j))
        E(MaxEPerm(j)) = SQRT(jh / cond)
        ! Perhaps should be this: MaxE(MaxEPerm(j)) = MAX(MaxE(MaxEPerm(j)), MAXEPrev(MaxEPerm(j)), E(MaxEPerm(j)))
        ! but I'm not sure we want (as prev model seems to, by my reading) take the max over the convergence iterations also
        MaxE(MaxEPerm(j)) = MAX(MAXEPrev(MaxEPerm(j)), E(MaxEPerm(j)))

        !A Numerical Investigation of the Electric and Thermal
        !Cell Kill Distributions in Electroporation-Based Therapies
        !in Tissue (PLOS ONE 2014)
        !Paulo A. Garcia, Rafael V. Davalos, Damijan Miklavcic
        S(MaxEPerm(j)) = MIN(S(MaxEPerm(j)), &
            1 / (1 + EXP((MaxE(MaxEPerm(j)) - E0 * EXP(-K1 * PulseNumber)) / (A0 * EXP(-K2 * PulseNumber)))))
      END DO

END SUBROUTINE
