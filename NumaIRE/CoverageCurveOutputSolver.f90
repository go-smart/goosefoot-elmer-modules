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

SUBROUTINE CoverageCurveOutputSolver(Model, Solver, Timestep, TransientSimulation)
      USE DefUtils

      IMPLICIT None

      TYPE(Model_t) :: Model
      TYPE(Solver_t) :: Solver
      REAL(KIND=dp) :: TimeStep
      LOGICAL :: TransientSimulation

      TYPE(Element_t), POINTER :: Element
      TYPE(Variable_t), POINTER :: MaxEVar, EVar
      TYPE(ValueList_t), POINTER :: SolverParams, Material
      TYPE(GaussIntegrationPoints_t) :: IntegStuff
      TYPE(Nodes_t) :: ElementNodes
      INTEGER :: i, j, t, divisions, N, out_unit
      REAL(KIND=dp) :: thresholdBottom, thresholdTop, threshold, &
          SqrtMetric, Metric(3,3), Symb(3,3,3), dSymb(3,3,3,3), &
          Basis(Model % MaxElementNodes), SqrtElementMetric, &
          x, y, z, contribution, volume, s, U, V, W, &
          tumour_volume, integ_E
      CHARACTER(LEN=100) :: filename
      INTEGER, POINTER :: MaxEPerm(:), NodeIndexes(:)
      REAL(KIND=dp), POINTER :: MaxE(:), E(:)
      REAL(KIND=dp), ALLOCATABLE :: thresholds(:), elim(:)
      LOGICAL :: AllocationsDone, stat, InTumour, Found

      SAVE AllocationsDone, thresholds, divisions, elim, &
          ElementNodes

      SolverParams => GetSolverParams()

      ! Set up the array of thresholds that must be accounted for
      IF (.NOT. AllocationsDone) THEN
          thresholdBottom = GetConstReal(SolverParams, "Minimum Coverage")
          thresholdTop = GetConstReal(SolverParams, "Maximum Coverage")
          divisions = GetInteger(SolverParams, "Divisions")

          N = Solver % Mesh % MaxElementNodes

          ALLOCATE(thresholds(divisions), elim(divisions))
          ALLOCATE( &
              ElementNodes % x(N), &
              ElementNodes % y(N), &
              ElementNodes % z(N)  &
          )

          DO i=1,divisions
            thresholds(i) = thresholdBottom + &
                (i - 1) * (thresholdTop - thresholdBottom) / (divisions - 1)
          END DO

          AllocationsDone = .TRUE.
      END IF

      ! This variable is the maximum energy deposition over all previous
      ! timesteps, at each point
      MaxEVar => VariableGet(Solver % Mesh % Variables, "Max_E")
      MaxE => MaxEVar % Values
      MaxEPerm => MaxEVar % Perm

      ! This variable is the present energy deposition at each point
      EVar => VariableGet(Solver % Mesh % Variables, "E")
      E => EVar % Values

      elim = 0.0
      tumour_volume = 0.0
      integ_E = 0.0

      ! Start counting over all bulk elements
      DO t=1,Solver % Mesh % NumberOfBulkElements
        Element => Solver % Mesh % Elements(t)
        Model % CurrentElement => Solver % Mesh % Elements(t)

        ! If this element is not a tet, skip it
        IF (Element % TYPE % ElementCode == 101) CYCLE

        j = GetInteger(Model % Bodies(Element % BodyId) % Values, "Material")
        Material => Model % Materials(j) % Values

        ! If this material is not tumour tissue, we do not count it
        InTumour = GetLogical(Material, "Tumour", Found)
        InTumour = Found .AND. InTumour
        IF (.NOT. InTumour) CYCLE

        ! Get the integration points
        IntegStuff = GaussPoints(Element)
        NodeIndexes => Element % NodeIndexes
        CALL GetElementNodes(ElementNodes)

        N = Element % TYPE % NumberOfNodes

        contribution = 0.0
        volume = 0.0

        ! Sum over the integration points
        DO j=1,IntegStuff % n
          U = IntegStuff % U(j)
          V = IntegStuff % V(j)
          W = IntegStuff % W(j)

          stat = ElementInfo(Element, ElementNodes, U, V, W, SqrtElementMetric, Basis)
          CALL CoordinateSystemInfo(Metric, SqrtMetric, Symb, dSymb, x, y, z)

          ! Get the integration factor
          s = SqrtMetric * SqrtElementMetric * IntegStuff % s(j)

          ! Integrate MaxE over this cell
          contribution = contribution + &
              s * SUM(MaxE(MaxEPerm(NodeIndexes(1:N))) * Basis(1:N))
          ! Integrate E over this cell
          integ_E = integ_E + &
              s * SUM(E(MaxEPerm(NodeIndexes(1:n))) * Basis(1:N))

          ! This is cell volume
          volume = volume + s

          ! This is total volume of tumour tissue
          tumour_volume = tumour_volume + s
        END DO

        ! For each division, add the cell's volume if its MaxE per unit vol is higher than the threshold
        DO j=1,divisions
          IF (thresholds(j) > contribution / volume) THEN
              !PRINT *, thresholds(j), j, contribution / volume, t, volume, elim(j)
              EXIT
          END IF
          elim(j) = elim(j) + volume
        END DO
      END DO

      WRITE(filename,10) GetTimestep()
10    FORMAT("coverage_", I0.3, ".txt")
      OPEN(UNIT=out_unit, FILE=filename, ACTION="WRITE", STATUS="REPLACE")

      ! Write out an easily plottable file
      DO i=1,divisions
        WRITE(out_unit, *) thresholds(i) / 100, elim(i) / tumour_volume
      END DO

      CLOSE(out_unit)

      PRINT *, "Integral of E: ", integ_E, thresholds(100), elim(100), tumour_volume
END SUBROUTINE CoverageCurveOutputSolver
