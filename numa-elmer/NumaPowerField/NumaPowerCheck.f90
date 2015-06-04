SUBROUTINE NumaPowerFieldTrigger_init( Model,Solver,Timestep,TransientSimulation )
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
          NextFreeKeyword('Exported Variable',SolverParams),'-global Phase')
     CALL ListAddString( SolverParams,&
          NextFreeKeyword('Exported Variable',SolverParams),'-global Power')
     CALL ListAddString( SolverParams,&
          NextFreeKeyword('Exported Variable',SolverParams),'-global RecalculatePower')

END SUBROUTINE NumaPowerFieldTrigger_init

! *****************************************************************************/
! *  Catch-all for looking after power distribution and evolution in point source model
! *****************************************************************************/
!------------------------------------------------------------------------------
SUBROUTINE NumaPowerFieldTrigger( Model,Solver,Timestep,TransientSimulation )
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
    TYPE(Variable_t), POINTER :: TimeVar, RecalculatePowerVar, PowerVar, PhaseVar
    TYPE(ValueList_t), POINTER :: SolverParams
    TYPE(Element_t),POINTER :: Element
    TYPE(Matrix_t), POINTER :: StiffMatrix
    TYPE(Solver_t), POINTER :: DataSolver

    REAL (KIND=DP), POINTER :: PowerField(:), PowerFieldPrev(:), Temperature(:), ForceVector(:), &
        ThermocoupleTemperatures(:), SAR(:)
    REAL(KIND=dp), POINTER :: ParPtr(:), PhasePtr(:), PowerPtr(:), &
        RecalculatePowerPtr(:)
    REAL(KIND=dp), POINTER :: PhasesPtr(:,:)
    REAL(KIND=dp) :: PreviousPower, PresentPower
    CHARACTER(LEN=255) :: DataSolverFilename

    INTEGER :: t, j, active, n, TDOFs, ActiveThermocouples, LocalNodes, &
        PowerControl_Integ_Length, CurrentPhase
    INTEGER, POINTER :: PowerFieldPerm(:), TempPerm(:), Perm(:)

    LOGICAL :: AllocationsDone = .FALSE., Found = .FALSE., TemperatureControlledPower = .FALSE., &
        RecalculatePower = .FALSE.

    SAVE AllocationsDone, &
        PreviousPower, PhasePtr, PowerPtr, RecalculatePowerPtr
        !, PhasePtr

    PhasesPtr => ListGetConstRealArray( Solver % Values, 'Phases', Found)
    IF (.NOT. Found) THEN
        CALL Fatal("NumaPowerFieldTrigger", "Phases must be set")
    END IF

    TimeVar => VariableGet(Model % Variables, 'Time')

    DO CurrentPhase = 1, SIZE(PhasesPtr, 2) - 1
        IF (PhasesPtr(1, CurrentPhase + 1) >= TimeVar % Values(1) - 1e-5) THEN
          EXIT
        END IF
    END DO

    PresentPower = PhasesPtr(2, CurrentPhase)

    PRINT *, "Protocol power is ", PresentPower, " on phase ", CurrentPhase, " at time ", TimeVar % Values(1)

    IF (PresentPower /= PreviousPower) THEN
        RecalculatePower = .TRUE.
        PRINT *, "Protocol power changed"
    ELSE
        RecalculatePower = .FALSE.
    END IF

    PreviousPower = PresentPower

    IF ( PresentPower < 0 ) THEN
        PRINT *, "Negative Power detected, taken as an indication to stop simulation"
        CALL ListAddConstReal(Model % Simulation, 'Exit Condition', 1.0_dp)
    ELSE
        PhaseVar => VariableGet(Model % Variables, 'Phase')
        IF ( .NOT. ASSOCIATED(PhasePtr) ) THEN
            NULLIFY(PhasePtr)
            ALLOCATE(PhasePtr(1));
            CALL VariableAdd(Model % Variables, Solver % Mesh, Solver, 'Phase', 1, PhasePtr)
            PhaseVar => VariableGet(Model % Variables, 'Phase')
        END IF
        PhaseVar % Values(1) = CurrentPhase

        PowerVar => VariableGet(Model % Variables, 'Power')
        IF ( .NOT. ASSOCIATED(PowerPtr) ) THEN
            NULLIFY(PowerPtr)
            ALLOCATE(PowerPtr(1));
            CALL VariableAdd(Model % Variables, Solver % Mesh, Solver, 'Power', 1, PowerPtr)
            PowerVar => VariableGet(Model % Variables, 'Power')
        END IF
        PowerVar % Values(1) = REAL(PresentPower)

        j = ListGetInteger(Solver % Values, 'Data Solver', Found)
        IF ( .NOT. Found ) THEN
            CALL Fatal("NumaPowerCheck", "Need a target data solver to update filename")
        END IF
        DataSolver => Model % Solvers(j)
        WRITE(DataSolverFilename, '("sar-",I4.4,".dat")') NINT(PresentPower)

        PRINT *, "Switching to input file: ", DataSolverFilename
        CALL ListAddString(DataSolver % Values, "Point Data Filename", DataSolverFilename)

        RecalculatePowerVar => VariableGet(Model % Variables, 'RecalculatePower')
        IF ( .NOT. ASSOCIATED(RecalculatePowerPtr) ) THEN
            NULLIFY(RecalculatePowerPtr)
            ALLOCATE(RecalculatePowerPtr(1));
            CALL VariableAdd(Model % Variables, Solver % Mesh, Solver, 'RecalculatePower', 1, RecalculatePowerPtr)
            RecalculatePowerVar => VariableGet(Model % Variables, 'RecalculatePower')
        END IF

        IF (RecalculatePower) THEN
            RecalculatePowerVar % Values(1) = 1
        ELSE
            RecalculatePowerVar % Values(1) = -1
        END IF

        PRINT *, "Timestep is ", Timestep
    END IF
!------------------------------------------------------------------------------ 
END SUBROUTINE NumaPowerFieldTrigger
!------------------------------------------------------------------------------
