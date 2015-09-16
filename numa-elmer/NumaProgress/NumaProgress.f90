SUBROUTINE NumaProgressSolver_init( Model,Solver,Timestep,TransientSimulation )
     USE DefUtils

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

END SUBROUTINE NumaProgressSolver_init

! *****************************************************************************/
! *  Catch-all for looking after power distribution and evolution in point source model
! *****************************************************************************/
!------------------------------------------------------------------------------
SUBROUTINE NumaProgressSolver( Model,Solver,Timestep,TransientSimulation )
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
    USE, INTRINSIC :: iso_c_binding

    IMPLICIT NONE

    TYPE, BIND(C) :: sockaddr_un
        INTEGER(KIND=c_int) :: sun_family
        CHARACTER(KIND=c_char) :: sun_path(108)
    END TYPE sockaddr_un

    TYPE(Model_t) :: Model
    TYPE(Solver_t), TARGET:: Solver
    REAL (KIND=DP) :: Timestep
    LOGICAL :: TransientSimulation

    REAL(KIND=dp), POINTER :: ParPtr(:)
    TYPE(Variable_t), POINTER :: TimeVar
    REAL(KIND=dp) :: PercentageProgress, Time
    LOGICAL :: AllocationsDone = .FALSE.
    INTEGER :: PercentageSocket
    CHARACTER(LEN=30, KIND=c_char) :: stat
    TYPE(sockaddr_un) :: socket_addr
    INTEGER :: i

    SAVE AllocationsDone, PercentageSocket, socket_addr

    INTERFACE
        FUNCTION socket(domain, type, protocol) bind(c, name="socket")
            USE, INTRINSIC :: iso_c_binding
            INTEGER(KIND=c_int) :: socket
            INTEGER(KIND=c_int), value :: domain, type, protocol
        END FUNCTION socket
        FUNCTION connect(sockfd, addr, addrlen) bind(c, name="connect")
            USE, INTRINSIC :: iso_c_binding
            INTEGER(KIND=c_int) :: connect
            INTEGER(KIND=c_int) :: sockfd, addrlen
            TYPE(*) :: addr
        END FUNCTION connect
        FUNCTION make_connection()
            INTEGER :: make_connection
        END FUNCTION make_connection
        FUNCTION output_percentage(stat, perc)
            USE Types
            INTEGER :: output_percentage
            CHARACTER(LEN=30, KIND=c_char) :: stat
            REAL(KIND=dp) :: perc
        END FUNCTION output_percentage
        FUNCTION geterrno()
            INTEGER :: geterrno
        END FUNCTION geterrno
        SUBROUTINE printerr()
        END SUBROUTINE printerr
        SUBROUTINE clearerrno()
        END SUBROUTINE clearerrno
    END INTERFACE

    IF (.NOT. AllocationsDone) THEN
        CALL clearerrno()

        i = make_connection()

        AllocationsDone = .TRUE.
    END IF

    ParPtr => GetReal( Solver % Values, 'Percentage Progress' )
    TimeVar => VariableGet(Model % Variables, 'Time')
    Time = TimeVar % Values(1)

    PercentageProgress = ParPtr(1)
    WRITE (stat, "(A5, F10.5)") "Elmer in progress: ", Time
    PRINT *, stat
    i = output_percentage(TRIM(stat)//C_NULL_CHAR, PercentageProgress)

    PRINT *, "Percentage progress is ", PercentageProgress
!------------------------------------------------------------------------------ 
END SUBROUTINE NumaProgressSolver
!------------------------------------------------------------------------------
