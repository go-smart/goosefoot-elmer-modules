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
MODULE HeatSolveSystem
 CONTAINS
  FUNCTION SolveHeatSystem(Solver, Model, Norm, TempSol, SmartHeaterNode, XX, YY, HeaterScaling, dt, ForceHeater, &
          PowerScaling, PowerTimeScale, NewtonLinearization, MeltPoint, IntegralHeaterControl, Relax, &
          SmartHeaterControl, SmartHeaterBC, SmartHeaterAverage, SmartTolReached, TransientHeaterControl, &
          SmartHeaters, IntegralHeaters, HeaterArea, HeaterSource, HeaterDensity, yave) RESULT(CheckConvergence)
        USE DefUtils
        IMPLICIT NONE

        TYPE(Solver_t) :: Solver
        TYPE(Model_t) :: Model
        REAL(KIND=dp) :: XX(:), YY(:)
        TYPE(Variable_t), POINTER :: TempSol
        INTEGER :: SmartHeaterBC, SmartHeaterNode, CheckConvergence
        LOGICAL, ALLOCATABLE :: SmartHeaters(:), IntegralHeaters(:)
        LOGICAL :: NewtonLinearization, IntegralHeaterControl, SmartHeaterAverage, &
            SmartHeaterControl, SmartTolReached, TransientHeaterControl
        REAL(KIND=dp), ALLOCATABLE :: HeaterScaling(:), ForceHeater(:), HeaterArea(:), &
            HeaterSource(:), HeaterDensity(:)
        REAL(KIND=dp) :: dt, yave, MeltPoint, Norm, PowerScaling, PowerTimeScale, Relax

        REAL(KIND=dp) :: xave, s
        TYPE(ValueList_t), POINTER :: SolverParams
        REAL(KIND=dp), POINTER :: Temperature(:)
        LOGICAL :: Converged, Found
        INTEGER :: i, j, k, l, LocalNodes
        INTEGER, POINTER :: TempPerm(:)
        TYPE(Element_t), POINTER :: Element

        TempPerm => TempSol % Perm
        Temperature => TempSol % Values
        LocalNodes = COUNT( TempPerm > 0 )
        CheckConvergence = 0

        SolverParams => GetSolverParams()

        IF(SmartHeaterControl .AND. NewtonLinearization .AND. SmartTolReached) THEN

          IF(.NOT. TransientHeaterControl) THEN

            CALL ListAddLogical(SolverParams, &
                'Skip Compute Nonlinear Change',.TRUE.)

            Relax = GetCReal( SolverParams, &
                'Nonlinear System Relaxation Factor', Found )

            IF ( Found .AND. Relax /= 1.0d0 ) THEN
              CALL ListAddConstReal( Solver % Values, &
                  'Nonlinear System Relaxation Factor', 1.0d0 )
            ELSE
              Relax = 1.0d0
            END IF

            CALL SolveSystem( Solver % Matrix, ParMatrix, &
                ForceHeater, XX, Norm, 1, Solver)

            CALL SolveSystem( Solver % Matrix, ParMatrix, &
                Solver % Matrix % RHS, YY, Norm, 1, Solver )

            CALL ListAddLogical(SolverParams,'Skip Compute Nonlinear Change',.FALSE.)
          ELSE
            CALL SolveSystem( Solver % Matrix, ParMatrix, &
                Solver % Matrix % RHS, Temperature, Norm, 1, Solver )
            YY = Temperature
          END IF

          IF(.NOT. SmartHeaterAverage) THEN
            xave = XX(TempPerm(SmartHeaterNode))
            yave = YY(TempPerm(SmartHeaterNode))
          ELSE
            xave = 0.0d0
            yave = 0.0d0
            j = 0

            DO k = Model % Mesh % NumberOfBulkElements + 1, &
                Model % Mesh % NumberOfBulkElements + Model % Mesh % NumberOfBoundaryElements

              Element => Model % Mesh % Elements(k)
              IF ( Element % BoundaryInfo % Constraint == SmartHeaterBC ) THEN
                l = Element % TYPE % NumberOfNodes
                j = j + l
                xave = xave + SUM( XX(TempPerm(Element % NodeIndexes)) )
                yave = yave + SUM( YY(TempPerm(Element % NodeIndexes)) )
              END IF
            END DO
            xave = xave / j
            yave = yave / j
            CALL ListAddConstReal(Model % Simulation,'res: Smart Heater Temperature',yave)
          END IF

          IF(.NOT. TransientHeaterControl) THEN
            IF ( ASSOCIATED(Solver % Variable % NonlinValues) ) THEN
              Solver % Variable % NonlinValues = Temperature
            END IF

            PowerScaling = (MeltPoint - yave) / xave
            Temperature = YY + PowerScaling * XX

            ! The change is computed separately for the controlled temperature field
            !-----------------------------------------------------------------------
            CALL ComputeChange(Solver,.FALSE.,LocalNodes,Temperature)
            Norm = Solver % Variable % Norm

          END IF

          IF(dt > PowerTimeScale) THEN
            IF ( Relax /= 1.0d0 ) THEN
              CALL ListAddConstReal( Solver % Values,  &
                  'Nonlinear System Relaxation Factor', Relax )
            END IF
          END IF
        ELSE
  !------------------------------------------------------------------------------
  !     Check stepsize for nonlinear iteration
  !------------------------------------------------------------------------------
          IF( DefaultLinesearch( Converged ) ) THEN
              CheckConvergence = 1
              RETURN
          END IF
          IF( Converged ) THEN
              CheckConvergence = 2
              RETURN
          END IF

          Norm = DefaultSolve()
        END IF


        IF( SmartHeaterControl .OR. IntegralHeaterControl) THEN

           CALL ListAddConstReal(Model % Simulation,'res: Heater Power Scaling',PowerScaling)

           CALL Info( 'HeatSolve', 'Heater Control Information', Level=4 )
           DO i=1,Model % NumberOfBodyForces
              IF( .NOT. (SmartHeaters(i) .OR. IntegralHeaters(i))) CYCLE
              IF( SmartHeaters(i) )  HeaterScaling(i) = PowerScaling

              WRITE( Message, '(A,T35,I15)' ) 'Heater for body: ', i
              CALL Info( 'HeatSolve', Message, Level=4 )
              IF(SmartHeaters(i)) WRITE( Message, '(A,T35,A)' ) 'Heater type:','Smart heater'
              IF(IntegralHeaters(i)) WRITE( Message, '(A,T35,A)' ) 'Heater type:','Integral heater'
              CALL Info( 'HeatSolve', Message, Level=4 )

              WRITE( Message,'(A,T35,ES15.4)' ) 'Heater Volume (m^3): ', HeaterArea(i)
              CALL Info( 'HeatSolve', Message, Level=4 )
              s = HeaterSource(i) * HeaterScaling(i)
              WRITE( Message,'(A,T35,ES15.4)' ) 'Heater Power (W): ', s
              CALL Info( 'HeatSolve', Message, Level=4 )

              WRITE( Message,'(A,T35,ES15.4)' ) 'Heater scaling: ', HeaterScaling(i)
              CALL Info( 'HeatSolve', Message, Level=4 )
              WRITE( Message, '(A,T35,ES15.4)' ) 'Heater Power Density (W/kg): ', s/(HeaterDensity(i) * HeaterArea(i))
              CALL Info( 'HeatSolve', Message, Level=4 )

              IF( SmartHeaters(i)) CALL ListAddConstReal(Model % Simulation,'res: Heater Power Density',&
                   s/(HeaterDensity(i) * HeaterArea(i)))
           END DO
        END IF
  END FUNCTION
END MODULE
