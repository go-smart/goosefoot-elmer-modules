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
SUBROUTINE NumaPowerGeneratorSolver_init( Model,Solver,Timestep,TransientSimulation )
     USE DefUtils
     USE NumaElectricTips

     IMPLICIT NONE
     TYPE(Solver_t) :: Solver  
     TYPE(Model_t) :: Model    
     REAL(KIND=dp) :: Timestep
     LOGICAL :: TransientSimulation 

!------------------------------------------------------------------------------
!    Local variables
!------------------------------------------------------------------------------
     TYPE(ValueList_t),POINTER :: SolverParams

     SolverParams => GetSolverParams()
     CALL ListAddString( SolverParams,&
          NextFreeKeyword('Exported Variable',SolverParams),'Electric Distribution')
     CALL ListAddString( SolverParams,&
          NextFreeKeyword('Exported Variable',SolverParams),'-global ObservedTemperature')
     CALL ListAddString( SolverParams,&
          NextFreeKeyword('Exported Variable',SolverParams),'-global Impedance')
     CALL ListAddString( SolverParams,&
          NextFreeKeyword('Exported Variable',SolverParams),'-global Applied Power')
     CALL ListAddString( SolverParams,&
          NextFreeKeyword('Exported Variable',SolverParams),'-global Phase')

END SUBROUTINE NumaPowerGeneratorSolver_init

! *****************************************************************************/
! *  Catch-all for looking after power distribution and evolution in point source model
! *****************************************************************************/
!------------------------------------------------------------------------------
SUBROUTINE NumaPowerGeneratorSolver( Model,Solver,Timestep,TransientSimulation )
!******************************************************************************
!
!  Cells state computation  
!
!  ARGUMENTS:
!
!  TYPE(Model_t) :: Model,  
!     INPUT: All model information (mesh,materials,BCs,etc...)
!
!  TYPE(Solver_t) :: Solver
!     INPUT: Linear equation solver options
!
!  REAL(KIND=dp) :: Timestep
!     INPUT: Timestep size for time dependent simulations
!
!    LOGICAL :: TransientSimulation
!        INPUT: Steady state or transient simulation
!
!******************************************************************************
    USE Types
    USE Lists
    USE ElementDescription
    USE DefUtils
    USE ElementUtils
    USE NumaElectricTips

    IMPLICIT NONE

    TYPE(Model_t) :: Model
    TYPE(Solver_t), TARGET:: Solver
    REAL (KIND=DP) :: Timestep
    LOGICAL :: TransientSimulation

    TYPE(Variable_t), POINTER :: TempSol, ElectricDistributionVar
    TYPE(Variable_t), POINTER :: TimeVar, ImpedanceVar, PhaseVar, ObservedTemperatureVar, AppliedPowerVar
    TYPE(ValueList_t), POINTER :: SolverParams
    TYPE(Element_t),POINTER :: Element
    TYPE(Matrix_t), POINTER :: StiffMatrix

    REAL (KIND=DP), POINTER :: PowerGenerator(:), PowerGeneratorPrev(:), Temperature(:), ForceVector(:), &
        ThermocoupleTemperatures(:), SAR(:)
    REAL(KIND=dp), ALLOCATABLE ::  &
        LocalSTIFF(:,:), LocalFORCE(:), &
        distribution(:), HistoryErrorToTargetTemperature(:)
    REAL(KIND=dp), POINTER :: ParPtr(:), PhasePtr(:), ImpedancePtr(:), ObservedTemperaturePtr(:), &
        AppliedPowerPtr(:)
    REAL(KIND=dp) :: Norm, Volume, Power, EPS = 1e-10_dp, AverageThermocoupleTemp, Temp, &
        DerivErrorToTargetTemperature, IntegErrorToTargetTemperature, ObservedTemperature, &
        PowerControl_Kd, PowerControl_Ki, PowerControl_Kp, TargetTemperature, &
        ThermocoupleCutOffTemperature, ThermocoupleReactivateTemperature, MaxPower, &
        PreviousPower

    INTEGER :: t, j, PreviousPhase = -1, PresentPhase, active, n, TDOFs, ActiveThermocouples, LocalNodes, &
        PowerControl_Integ_Length
    INTEGER, POINTER :: PowerGeneratorPerm(:), TempPerm(:), Perm(:)

    LOGICAL :: AllocationsDone = .FALSE., Found = .FALSE., TemperatureControlledPower = .FALSE.

    SAVE distribution, AllocationsDone, &
        LocalSTIFF, LocalFORCE, &
        PreviousPhase, ImpedancePtr, AppliedPowerPtr, ObservedTemperaturePtr, &
        HistoryErrorToTargetTemperature, TemperatureControlledPower, &
        DerivErrorToTargetTemperature, IntegErrorToTargetTemperature, &
        PowerControl_Integ_Length, PowerControl_Kd, PowerControl_Ki, PowerControl_Kp
        !, PhasePtr

    CALL AddGaussianVariable(Solver)

!------------------------------------------------------------------------------
!  Get variables needed for solution
!  PowerGenerator has 2 d.o.f. which correspond to alive and dead states
!------------------------------------------------------------------------------
    IF ( .NOT. ASSOCIATED( Solver % Matrix ) ) RETURN

    StiffMatrix => Solver % Matrix
    ForceVector => Solver % Matrix % RHS

    PowerGenerator     => Solver % Variable % Values
    PowerGeneratorPrev => Solver % Variable % PrevValues(:,1)
    PowerGeneratorPerm => Solver % Variable % Perm
    SolverParams => GetSolverParams()

!------------------------------------------------------------------------------
!  The temperature is used to compute alive and dead states
!------------------------------------------------------------------------------    
    TempSol => VariableGet( Solver % Mesh % Variables, 'Temperature' )
    IF ( ASSOCIATED( TempSol ) ) THEN
        TempPerm    => TempSol % Perm
        Temperature => TempSol % Values
        TDOFs =  TempSol % DOFs
    END IF

    ParPtr => GetReal( Solver % Values, 'Present Phase' )
    PresentPhase = NINT(ParPtr(1))
    PRINT *, "Protocol phase is ", PresentPhase

    IF (PresentPhase /= PreviousPhase) THEN
        PRINT *, "Protocol phase changed"
        CALL GaussianInvalidate()
        !LOAD PHASE DATA HERE IF PresentPhase != PreviousPhase
    END IF

    PreviousPhase = PresentPhase

    ParPtr => GetReal(Solver % Values, 'Target Temperature', Found)
    IF ( .NOT. Found) THEN
        CALL Fatal("NumaPowerGeneratorSolver", "No target temperature supplied")
    END IF

    ThermocoupleCutOffTemperature = GetConstReal(Solver % Values, 'Thermocouple Cut Off Temperature', Found)
    IF (.NOT. Found) THEN
        ThermocoupleCutOffTemperature = -1
        ThermocoupleReactivateTemperature = -1
    ELSE
        ThermocoupleReactivateTemperature = GetConstReal(Solver % Values, 'Thermocouple Reactivate Temperature', Found)
        IF (.NOT. Found) THEN
            CALL Fatal("NumaPowerGeneratorSolver", "Thermocouple cut off supplied without reactivation temperature")
        END IF
        PRINT *, "Thermocouple cut off temperature set to : ", ThermocoupleCutOffTemperature
        PRINT *, "Thermocouple reactivation temperature set to : ", ThermocoupleReactivateTemperature
    END IF

    TargetTemperature = ParPtr(1)

    ObservedTemperatureVar => VariableGet(Model % Variables, 'ObservedTemperature')
    IF ( .NOT. ASSOCIATED(ObservedTemperatureVar) ) THEN
        NULLIFY(ObservedTemperaturePtr)
        ALLOCATE(ObservedTemperaturePtr(1))
        ObservedTemperaturePtr(1) = 310.0
        CALL VariableAdd(Model % Variables, Solver % Mesh, Solver, 'ObservedTemperature', 1, ObservedTemperaturePtr)
        ObservedTemperatureVar => VariableGet(Model % Variables, 'ObservedTemperature')
    END IF
    ThermocoupleTemperatures => GetThermocoupleTemperatures(TempSol, Model, Solver)
    AverageThermocoupleTemp = 0.0_dp
    ActiveThermocouples = 0
    DO t = 1, SIZE(ThermocoupleTemperatures)
        IF (ThermocoupleTemperatures(t) >= 1e-10_dp) THEN  ! NB In Kelvin, any negative value is invalid and indicates inactive tc
            AverageThermocoupleTemp = AverageThermocoupleTemp + ThermocoupleTemperatures(t)
            ActiveThermocouples = ActiveThermocouples + 1

            IF (ThermocoupleCutOffTemperature >= 0) THEN
                IF (ThermocoupleTemperatures(t) > ThermocoupleCutOffTemperature) THEN
                    CALL UpdateThermocoupleHeatingState(t, .FALSE.)
                    PRINT *, "Thermocouple ", t, " x: ", ThermocoupleTemperatures(t)
                ELSE IF (.NOT. GetThermocoupleHeatingState(t) &
                        .AND. ThermocoupleTemperatures(t) < ThermocoupleReactivateTemperature) THEN
                    CALL UpdateThermocoupleHeatingState(t, .TRUE.)
                    PRINT *, "Thermocouple ", t, " +: ", ThermocoupleTemperatures(t)
                ELSE
                    PRINT *, "Thermocouple ", t, "  : ", ThermocoupleTemperatures(t)
                END IF
            ELSE
                PRINT *, "Thermocouple ", t, "  : ", ThermocoupleTemperatures(t)
            END IF
        ELSE
            PRINT *, "Thermocouple ", t, "  : inactive"
        END IF
    END DO
    IF (ActiveThermocouples > 0) THEN
        AverageThermocoupleTemp = AverageThermocoupleTemp / ActiveThermocouples
        ObservedTemperatureVar % Values(1) = AverageThermocoupleTemp
    END IF

    ObservedTemperature = ObservedTemperatureVar % Values(1)

    IF ( .NOT. AllocationsDone .OR. Solver % Mesh % Changed ) THEN
        CALL Allocations()
    END IF

    PRINT *, "Temperature observed by generator : ", AverageThermocoupleTemp, "K"

    ImpedanceVar => VariableGet(Model % Variables, 'Impedance')
    IF ( .NOT. ASSOCIATED(ImpedanceVar) ) THEN
        NULLIFY(ImpedancePtr)
        ALLOCATE(ImpedancePtr(1))
        CALL VariableAdd(Model % Variables, Solver % Mesh, Solver, 'Impedance', 1, ImpedancePtr)
        ImpedanceVar => VariableGet(Model % Variables, 'Impedance')
    END IF

    AppliedPowerVar => VariableGet(Model % Variables, 'Applied Power')
    IF ( .NOT. ASSOCIATED(AppliedPowerVar) ) THEN
        NULLIFY(AppliedPowerPtr)
        ALLOCATE(AppliedPowerPtr(1))
        AppliedPowerPtr(1) = GetConstReal(Solver % Values, "Initial Power", Found)
        IF (.NOT. Found) THEN
            AppliedPowerPtr(1) = 0
        END IF
        CALL VariableAdd(Model % Variables, Solver % Mesh, Solver, 'Applied Power', 1, AppliedPowerPtr)
        AppliedPowerVar => VariableGet(Model % Variables, 'Applied Power')
    END IF
    PreviousPower = AppliedPowerVar % Values(1)

    PhaseVar => VariableGet(Model % Variables, 'Phase')
    IF ( .NOT. ASSOCIATED(PhasePtr) ) THEN
        NULLIFY(PhasePtr)
        ALLOCATE(PhasePtr(1));
        CALL VariableAdd(Model % Variables, Solver % Mesh, Solver, 'Phase', 1, PhasePtr)
        PhaseVar => VariableGet(Model % Variables, 'Phase')
    END IF
    PhaseVar % Values(1) = REAL(PresentPhase)

    ParPtr => GetReal(Solver % Values, 'Electric Power', Found)
    IF ( .NOT. Found ) THEN
        CALL Fatal("NumaPowerGeneratorSolver", "Input power has not been given (Electric Power in Solver section)")
    END IF
    MaxPower = ParPtr(1)
    PRINT *, "Unadjusted Power is ", MaxPower, "Target Temperature is ", TargetTemperature

    IF ( MaxPower < -EPS .OR. PresentPhase < 0 ) THEN
        PRINT *, "Negative Power or Phase detected, taken as an indication to stop simulation"
        PRINT *, "Power : ", MaxPower, " Phase : ", PresentPhase
        CALL ListAddConstReal(Model % Simulation, 'Exit Condition', 1.0_dp)
    END IF

    PRINT *, "Timestep is ", Timestep
    IF (TemperatureControlledPower) THEN
    !------------------------------------------------------------------------------
    !   If temperature-controlled power, get the power from computation: 
    !------------------------------------------------------------------------------
            Power = PreviousPower + &
                PowerControl_Kp * (TargetTemperature-ObservedTemperature) + & 
                PowerControl_Ki * IntegErrorToTargetTemperature + & 
                PowerControl_Kd * DerivErrorToTargetTemperature
            !------------------------------------------------------------------------------
            ! Control of max and min Power: 
            !------------------------------------------------------------------------------
            Power = MIN(Power,MaxPower)
            Power = MAX(Power,0.0)

            !------------------------------------------------------------------------------ 
            ! Save error between target and current temperature and update history:
            !------------------------------------------------------------------------------ 
            DO j=1,PowerControl_Integ_Length-1
                HistoryErrorToTargetTemperature(PowerControl_Integ_Length-j+1) = &
                    HistoryErrorToTargetTemperature(PowerControl_Integ_Length-j)
            END DO
            HistoryErrorToTargetTemperature(1) = TargetTemperature-ObservedTemperature

            !------------------------------------------------------------------------------ 
            ! Integral of error between target and current temperature:
            !------------------------------------------------------------------------------ 
            IF (PowerControl_Integ_Length /= 0) THEN
            !------------------------------------------------------------------------------
            !If specified length of integral, use history variable
            !------------------------------------------------------------------------------
                IntegErrorToTargetTemperature = 0.0D0
                DO j=1,PowerControl_Integ_Length
                    IntegErrorToTargetTemperature = IntegErrorToTargetTemperature + &
                        HistoryErrorToTargetTemperature(j) * Timestep
                END DO
            !------------------------------------------------------------------------------
            ELSE
            !------------------------------------------------------------------------------
            !Else, make the integral over all time-steps
            !------------------------------------------------------------------------------             
                IntegErrorToTargetTemperature = IntegErrorToTargetTemperature + &
                        HistoryErrorToTargetTemperature(1) * Timestep
            !------------------------------------------------------------------------------
            END IF
            !------------------------------------------------------------------------------ 
            ! Derivative of error between target and current temperature:
            !------------------------------------------------------------------------------ 
            DerivErrorToTargetTemperature = (HistoryErrorToTargetTemperature(1)- &
                HistoryErrorToTargetTemperature(2)) / Timestep
    ELSE
        Power = MaxPower
    END IF
              !------------------------------------------------------------------------------
    PRINT *, "Applied (PID-adjusted) Power is: ", Power
    AppliedPowerVar % Values(1) = Power

    active = GetNOFActive()

    Volume = 0.0_dp
    Norm = 0.0_dp

    LocalNodes = COUNT(Solver % Variable % Perm > 0)

    SAR => Solver % Variable % Values
    Perm => Solver % Variable % Perm

    DO t = 1, LocalNodes
        IF (Perm(t) <= 0) CYCLE
        SAR(Perm(t)) = Power * GaussianTipDistribution(Solver, Model, t)
    END DO

    DO t = 1, active
        Element => GetActiveElement(t)
        n = GetElementNOFNodes()

        ! Element not in this process
        IF (ANY(Perm(Element % NodeIndexes(1:n)) == 0)) CYCLE

        CALL LocalMatrix(Element, SAR, Perm, n, Norm, Volume, Power)
    END DO

    IF (Norm > TINY(Norm)) THEN
        Norm = SQRT(Norm) !RMV?/ Volume
    END IF

    ParPtr => GetReal(Solver % Values, 'Impedance Voltage')
    ImpedanceVar % Values(1) = ParPtr(1) ** 2 / (Norm / Volume)
    PRINT *, "Impedance : ", ImpedanceVar % Values(1), " over volume ", Volume, &
        " (SAR norm: ", Norm, ")"

  CONTAINS
    SUBROUTINE Allocations
        INTEGER :: N, istat

        N = Solver % Mesh % MaxElementDOFs

        IF (AllocationsDone) THEN
            DEALLOCATE(LocalFORCE, LocalSTIFF, STAT=istat )
        END IF

        ALLOCATE(LocalFORCE(N), LocalSTIFF(N,N), STAT=istat )

        IF ( istat /= 0 ) THEN
            CALL Fatal("NumaPowerGeneratorSolver", "Could not allocate memory")
        END IF

        !------------------------------------------------------------------------------ 
        !   Check if the electric power has to be controlled in function of temperature 
        !------------------------------------------------------------------------------
        TemperatureControlledPower = GetLogical( Solver % Values, &
           'Temperature Controlled Electric Power',Found )
        IF ( .NOT.Found ) TemperatureControlledPower = .FALSE.
        !------------------------------------------------------------------------------
        IF(TemperatureControlledPower) THEN
            !------------------------------------------------------------------------------ 
            !       Get the target temperature:
            !------------------------------------------------------------------------------     
            !------------------------------------------------------------------------------ 
            !       Get the PID parameters:
            !------------------------------------------------------------------------------     
            PowerControl_Kp = GetConstReal( Solver % Values, &
                'Proportional Gain For Electric Power Control', Found )
            IF ( .NOT.Found ) PowerControl_Kp = 0.0

            PowerControl_Kd = GetConstReal( Solver % Values, &
                'Derivative Gain For Electric Power Control', Found )
            IF ( .NOT.Found ) PowerControl_Kd = 0.0

            PowerControl_Ki = GetConstReal( Solver % Values, &
                'Integral Gain For Electric Power Control', Found )
            IF ( .NOT.Found ) PowerControl_Ki = 0.0

            PowerControl_Integ_Length = GetInteger( Solver % Values, &
                'Integral Length For Electric Power Control', Found )
            IF ( .NOT.Found ) PowerControl_Integ_Length = 0

            ALLOCATE(HistoryErrorToTargetTemperature(MAX(PowerControl_Integ_Length,2)))

            PRINT *,'PID Temperature Controlled Electric Power, with parameters:'
            PRINT *,'- PID Proportional gain = ',PowerControl_Kp
            PRINT *,'- PID Derivative gain = ',PowerControl_Kd
            PRINT *,'- PID Integral gain = ',PowerControl_Ki
            PRINT *,'- PID Target Temperature (K) = ',TargetTemperature
            PRINT *,'- PID Integration Length = ',PowerControl_Integ_Length
            !------------------------------------------------------------------------------     
            !           Initialize the corresponding variables:
            !------------------------------------------------------------------------------     
            HistoryErrorToTargetTemperature = 0.0D0
            HistoryErrorToTargetTemperature(1) = TargetTemperature - ObservedTemperature
            IntegErrorToTargetTemperature = HistoryErrorToTargetTemperature(1)*Timestep
            DerivErrorToTargetTemperature = 0.0D0
        !------------------------------------------------------------------------------     
        END IF !TemperatureControlledPower

        AllocationsDone = .TRUE.
    END SUBROUTINE

    SUBROUTINE LocalMatrix(Element, Values, Perm, n, Norm, Volume, Power)
        REAL(KIND=dp) :: Norm, Volume, Power
        INTEGER :: n
        INTEGER, POINTER :: Perm(:)
        REAL(KIND=dp), POINTER :: Values(:)
        TYPE(Element_t), POINTER :: Element
        TYPE(GaussIntegrationPoints_t) :: IP

        REAL(KIND=dp) :: Basis(n), detJ, Weight
        INTEGER :: i, t, k
        LOGICAL :: stat

        TYPE(Nodes_t) :: Nodes

        SAVE Nodes

        CALL GetElementNodes(Nodes)

        IP = GaussPoints(Element)

        DO t = 1, IP % n
            stat = ElementInfo(Element, Nodes, IP % U(t), IP % V(t), IP % W(t), detJ, Basis)

            weight = IP % s(t) * detJ

            DO i = 1, n
                k = Perm(Element % NodeIndexes(i))
                Norm = Norm + k * Basis(i) * (Values(k) ** 2)
            END DO
            Volume = Volume + weight
        END DO

    END SUBROUTINE
!------------------------------------------------------------------------------ 
END SUBROUTINE NumaPowerGeneratorSolver
!------------------------------------------------------------------------------
