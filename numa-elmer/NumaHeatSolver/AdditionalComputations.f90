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

SUBROUTINE AdditionalComputations(Solver, Model, Temperature, TempPerm, IntegErrorToTargetTemperature, &
        DerivErrorToTargetTemperature, TargetTemperature, HeatSource, dt)

    USE MaterialModels
    USE DefUtils
    USE NumaAdaptive
    IMPLICIT NONE

    TYPE(Solver_t), TARGET :: Solver
    TYPE(Model_t) :: Model
    REAL(KIND=dp), POINTER :: HeatSource(:), Temperature(:)
    REAL(KIND=dp) :: dt, IntegErrorToTargetTemperature(:), DerivErrorToTargetTemperature(:), &
        TargetTemperature
    INTEGER, POINTER :: TempPerm(:)

    INTEGER :: LocalNodes, i, j, k, t, N, PowerControl_Integ_Length
    TYPE(Element_t), POINTER :: Element
    TYPE(Nodes_t)   :: ElementNodes
    REAL(KIND=dp) :: ErrorL0, ErrorH1, ErrorL2, MaxTemperature
    REAL(KIND=dp), POINTER :: HistoryErrorToTargetTemperatureVariable(:), HistoryErrorToTargetTemperature(:,:)
    REAL(KIND=dp), ALLOCATABLE :: DeltaTemperature(:), &
            LocalHeatSource(:)
    LOGICAL :: TemperatureControlledPower, ControlError, ControlMaxTemperature, Found, &
        VisualizeHeatSource, AllocationsDone = .FALSE.
    TYPE(ValueList_t), POINTER :: BodyForce, SolverParams

    REAL(KIND=dp), EXTERNAL :: AnalyticalSolution

    SAVE ElementNodes, AllocationsDone, LocalNodes, MaxTemperature, DeltaTemperature, LocalHeatSource, &
        ControlMaxTemperature, ControlError, VisualizeHeatSource, TemperatureControlledPower, &
        HistoryErrorToTargetTemperature, PowerControl_Integ_Length

    SolverParams => GetSolverParams()
    LocalNodes = COUNT( TempPerm > 0 )

    IF ( .NOT. AllocationsDone .OR. Solver % Mesh % Changed ) THEN
        CALL Allocations()
    END IF

    IF(ControlError) THEN
    !------------------------------------------------------------------------------         
    !   Compute L2 and H1 Errors between numerical and analytical temperature 
    !------------------------------------------------------------------------------   
        CALL ErrorCompute( Solver, Model, Temperature, TempPerm, ErrorH1, ErrorL2 )
        CALL Info( 'NumaHeatSolve', 'Errors between numerical and analytical solutions:', Level=4 )
        PRINT *,'ErrorL2= ',ErrorL2
        PRINT *,'ErrorH1= ',ErrorH1
    !------------------------------------------------------------------------------         
    !   Compute L-infinite Error between numerical and analytical temperature 
    !------------------------------------------------------------------------------
        ErrorL0 = 0.0D0
        DO i=1,LocalNodes
            DeltaTemperature(i)=ABS(Temperature(TempPerm(i))- &
                AnalyticalSolution( model % Nodes % x(i), model % Nodes % y(i), model % Nodes % z(i)))
            IF (DeltaTemperature(i)>ErrorL0) THEN
                ErrorL0 = DeltaTemperature(i)
            END IF
        END DO
        PRINT *,'ErrorL0= ',ErrorL0
    !------------------------------------------------------------------------------         
    !   Compute L2 Error with Elmer method:
    !------------------------------------------------------------------------------
        ErrorL2 = 0.0D0
        DO i=1,LocalNodes
            DeltaTemperature(i)=Temperature(TempPerm(i))- &
                AnalyticalSolution( model % Nodes % x(i), model % Nodes % y(i), model % Nodes % z(i))
            ErrorL2 = ErrorL2 + DeltaTemperature(i)**2
        END DO
        ErrorL2 = sqrt(ErrorL2 / LocalNodes)
        PRINT *,'ErrorL2 (Monte-Carlo method)= ',ErrorL2
    !------------------------------------------------------------------------------
    END IF
    !------------------------------------------------------------------------------         
    ! Compute Max temperature 
    !------------------------------------------------------------------------------         
    IF(ControlMaxTemperature) THEN
        MaxTemperature = 0.0d0
        DO i=1,LocalNodes
            IF (MaxTemperature < Temperature(i)) THEN
                MaxTemperature = Temperature(i)
            END IF
        END DO
    END IF

    !------------------------------------------------------------------------------         
    ! Compute variables for temperature-controlled power:
    !------------------------------------------------------------------------------         
    IF(TemperatureControlledPower) THEN
    !------------------------------------------------------------------------------ 
        ! Save error between target and current temperature and update history:
        !------------------------------------------------------------------------------ 
        DO j=1,PowerControl_Integ_Length-1
                HistoryErrorToTargetTemperature(PowerControl_Integ_Length-j+1,:) = &
                    HistoryErrorToTargetTemperature(PowerControl_Integ_Length-j,:)
        END DO
        HistoryErrorToTargetTemperature(1,:) = TargetTemperature-Temperature

        HistoryErrorToTargetTemperatureVariable => HistoryErrorToTargetTemperature(1,:)

        CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
        Solver, 'Error to target Temperature', 1, &
        HistoryErrorToTargetTemperatureVariable, TempPerm ) 
        !------------------------------------------------------------------------------ 
        ! Integral of error between target and current temperature:
        !------------------------------------------------------------------------------ 
        IF (PowerControl_Integ_Length /= 0) THEN
        !------------------------------------------------------------------------------
        !If specified length of integral, use history variable
        !------------------------------------------------------------------------------
            IntegErrorToTargetTemperature = 0.0D0
            DO j=1,PowerControl_Integ_Length
                IntegErrorToTargetTemperature(:) = IntegErrorToTargetTemperature(:) + &
                    HistoryErrorToTargetTemperature(j,:)*dt
            END DO
        !------------------------------------------------------------------------------
        ELSE
        !------------------------------------------------------------------------------
        !Else, make the integral over all time-steps
        !------------------------------------------------------------------------------             
            IntegErrorToTargetTemperature(:) = IntegErrorToTargetTemperature(:) + &
                    HistoryErrorToTargetTemperature(1,:)*dt
        !------------------------------------------------------------------------------
        END IF
        !------------------------------------------------------------------------------ 
        ! Derivative of error between target and current temperature:
        !------------------------------------------------------------------------------ 
        DerivErrorToTargetTemperature(:) = (HistoryErrorToTargetTemperature(1,:)- &
            HistoryErrorToTargetTemperature(2,:))/dt
    !------------------------------------------------------------------------------ 
    END IF !TemperatureControlledPower
    !------------------------------------------------------------------------------ 
    !   Add the heat source as a variable for visualization
    !------------------------------------------------------------------------------ 
    VisualizeHeatSource = GetLogical( SolverParams,'Heat Source Visualization',Found )
    IF ( .NOT.Found ) VisualizeHeatSource = .FALSE.
    !------------------------------------------------------------------------------
    IF(VisualizeHeatSource) THEN
    !------------------------------------------------------------------------------
            HeatSource = 0.0D0
    !------------------------------------------------------------------------------ 
    !   Go through bulk elements and get heat source:
    !------------------------------------------------------------------------------
            DO t=1,Solver % NumberOfActiveElements
    !------------------------------------------------------------------------------
                Element => GetActiveElement(t)
                N = GetElementNOFNodes()
                CALL GetElementNodes( ElementNodes )    

                BodyForce => GetBodyForce()
    !------------------------------------------------------------------------------
                IF ( ASSOCIATED( BodyForce ) ) THEN
    !------------------------------------------------------------------------------ 
                    !------------------------------------------------------------------------------
                    ! Read the body force value in the input file: 
                    !------------------------------------------------------------------------------
                    LocalHeatSource(1:n) = LocalHeatSource(1:n) + &
                            GetReal( BodyForce, 'Heat Source 1', Found )
    !--------------------------------------------------------------------------
                    DO i=1,n
    !--------------------------------------------------------------------------
                        k = TempPerm(Element % NodeIndexes(i))
                        HeatSource(k) = LocalHeatSource(i)
    !--------------------------------------------------------------------------
                    END DO ! i
    !------------------------------------------------------------------------------ 
                END IF
    !------------------------------------------------------------------------------ 
            END DO ! t
    !------------------------------------------------------------------------------ 
            CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
            Solver, 'Heat source', 1, &
            HeatSource, TempPerm ) 
    !------------------------------------------------------------------------------ 
    END IF

    CONTAINS
        SUBROUTINE Allocations()
            CHARACTER(LEN=100) :: MaxTemperatureFilename, char_MyPe, TestName
            INTEGER :: N

            N = Solver % Mesh % MaxElementNodes

            IF ( AllocationsDone ) THEN
                DEALLOCATE( &
                    DeltaTemperature,                       &
                    LocalHeatSource,             &
                    ElementNodes % x,                &
                    ElementNodes % y,                &
                    ElementNodes % z,                &
                )
            END IF
            ALLOCATE( &
                DeltaTemperature(LocalNodes),   &
                LocalHeatSource(N),             &
                ElementNodes % x( N ),                &
                ElementNodes % y( N ),                &
                ElementNodes % z( N ),                &
            )

            !------------------------------------------------------------------------------
            !       If specified in the input file, compute the error between numerical and 
            !       analytical solution
            !------------------------------------------------------------------------------ 
            ControlError = GetLogical( SolverParams,'Control Errors',Found )        
            IF ( .NOT.Found ) ControlError = .FALSE.
            !------------------------------------------------------------------------------ 
            !   Check if the electric power has to be controlled in function of temperature 
            !------------------------------------------------------------------------------
            TemperatureControlledPower = GetLogical( Model % Simulation, &
                'Temperature Controlled Electric Power',Found )
            IF ( .NOT.Found ) TemperatureControlledPower = .FALSE.
            !------------------------------------------------------------------------------
            IF(TemperatureControlledPower) THEN
                PowerControl_Integ_Length = GetInteger( Model % Simulation, &
                    'Integral Length For Electric Power Control', Found )
                IF ( .NOT.Found ) PowerControl_Integ_Length = 0

                ALLOCATE(HistoryErrorToTargetTemperature(MAX(PowerControl_Integ_Length,2),LocalNodes))

                PRINT *,'- PID Integration Length = ', PowerControl_Integ_Length
                !------------------------------------------------------------------------------     
                !           Initialize the corresponding variables:
                !------------------------------------------------------------------------------     
                HistoryErrorToTargetTemperature = 0.0D0
                DO i=1,LocalNodes
                    HistoryErrorToTargetTemperature(1,i) = TargetTemperature-Temperature(i)
                END DO
                IntegErrorToTargetTemperature(:) = HistoryErrorToTargetTemperature(1,:)*dt
                DerivErrorToTargetTemperature = 0.0D0
    !------------------------------------------------------------------------------     
            END IF !TemperatureControlledPower

    !------------------------------------------------------------------------------
    !       If specified in the input file, compute the maximum temperature over the model: 
    !------------------------------------------------------------------------------ 
            ControlMaxTemperature = GetLogical( SolverParams,'Control Max Temperature',Found )      
            IF ( .NOT.Found ) ControlMaxTemperature = .FALSE.
            IF (ControlMaxTemperature) THEN
                MaxTemperature = 0.0D0
                DO i=1,LocalNodes
                    IF (MaxTemperature < Temperature(i)) THEN
                        MaxTemperature = Temperature(i)
                    END IF
                END DO
    !------------------------------------------------------------------------------ 
    !       Write the header of the max-temperature control file
    !------------------------------------------------------------------------------     
                IF(ParEnv % PEs>1) THEN
                    WRITE(char_MyPe,*) ParEnv % MyPe
                    char_MyPe = ADJUSTL(char_MyPe)
                    MaxTemperatureFilename = 'maxtemp'//'_'//TRIM(TestName)//'_'//TRIM(char_MyPe)//'.dat'       
                ELSE 
                    MaxTemperatureFilename = 'maxtemp'//'_'//TRIM(TestName)//'.dat'         
                END IF
                
                OPEN(UNIT=1,FILE=MaxTemperatureFilename)
                WRITE(UNIT=1,FMT=*) 'Time    ', '    Blood Temperature    ', '    Tissue Temperature'
                CLOSE(1)
    !------------------------------------------------------------------------------
            END IF ! ControlMaxTemperature
    !------------------------------------------------------------------------------ 

            AllocationsDone = .TRUE.
        END SUBROUTINE Allocations

END SUBROUTINE AdditionalComputations
