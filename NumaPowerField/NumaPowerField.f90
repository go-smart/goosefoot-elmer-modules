SUBROUTINE NumaPowerFieldSolver_init( Model,Solver,Timestep,TransientSimulation )
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

END SUBROUTINE NumaPowerFieldSolver_init

! *****************************************************************************/
! *  Catch-all for looking after power distribution and evolution in point source model
! *****************************************************************************/
!------------------------------------------------------------------------------
SUBROUTINE NumaPowerFieldSolver( Model,Solver,Timestep,TransientSimulation )
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

    TYPE(Variable_t), POINTER :: PhaseVar, PowerFieldVar, &
        RecalculatePowerVar, TargetVar
    REAL(KIND=dp), ALLOCATABLE :: FieldCache(:,:)
    CHARACTER(LEN=128) :: TargetNamePtr

    LOGICAL :: AllocationsDone = .FALSE., Found = .FALSE.

    IF (.NOT. AllocationsDone) THEN
        CALL Allocations()
    END IF

    RecalculatePowerVar => VariableGet(Model % Variables, 'RecalculatePower')
    IF (.NOT. ASSOCIATED(RecalculatePowerVar)) THEN
        CALL Fatal("NumaPowerFieldSolver", "Must be called with NumaPowerFieldTrigger")
    END IF

    PhaseVar => VariableGet(Model % Variables, 'Phase')
    IF (.NOT. ASSOCIATED(PhaseVar)) THEN
        CALL Fatal("NumaPowerFieldSolver", "Cannot find Phase variable")
    END IF

    TargetNamePtr = ListGetString(Solver % Values, 'Target', Found)
    IF (.NOT. Found) THEN
        CALL Fatal("NumaPowerFieldSolver", "Cannot find variable")
    END IF

    TargetVar => VariableGet(Model % Variables, TargetNamePtr)
    IF (.NOT. ASSOCIATED(TargetVar)) THEN
        CALL Fatal("NumaPowerFieldSolver", "Cannot find target variable")
    END IF

    !IF (RecalculatePowerVar % Values(1) > 0) THEN
    !    FieldCache(
    !END IF

    CONTAINS
        SUBROUTINE Allocations
            INTEGER :: N, istat, r
            LOGICAL :: Found

            N = Solver % Mesh % MaxElementDOFs
            !r = SIZE(ListGetIntegerArray(Solver % Values, 'Power Sequence', Found))

            !IF (.NOT. Found) THEN
            !    CALL Fatal("NumaPowerFieldSolver", "Must have power sequence")
            !END IF

            !IF (AllocationsDone) THEN
            !    DEALLOCATE(FieldCache, STAT=istat )
            !END IF

            !ALLOCATE(FieldCache(r,N), STAT=istat )

            !IF ( istat /= 0 ) THEN
            !    CALL Fatal("NumaPowerFieldSolver", "Could not allocate memory")
            !END IF

            AllocationsDone = .TRUE.
        END SUBROUTINE
!------------------------------------------------------------------------------ 
END SUBROUTINE NumaPowerFieldSolver
!------------------------------------------------------------------------------
