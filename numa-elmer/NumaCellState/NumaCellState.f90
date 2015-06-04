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
SUBROUTINE NumaCellStateSolver_init( Model, Solver, Timestep, TransientSimulation)
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
       CALL ListAddString( SolverParams,&
            NextFreeKeyword('Exported Variable',SolverParams),'Vulnerable')

END SUBROUTINE NumaCellStateSolver_init

! *****************************************************************************/
! *  Subroutines for the detection of died cells
! *****************************************************************************/
!------------------------------------------------------------------------------
SUBROUTINE NumaCellStateSolver( Model,Solver,Timestep,TransientSimulation )
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

    IMPLICIT NONE

    TYPE(Model_t) :: Model
    TYPE(Solver_t), TARGET:: Solver
    REAL (KIND=DP) :: Timestep
    LOGICAL :: TransientSimulation

    TYPE(Variable_t), POINTER :: TempSol
    TYPE(Variable_t), POINTER :: TimeVar
    TYPE(ValueList_t), POINTER :: SolverParams
    TYPE(Element_t),POINTER :: Element
    TYPE(Nodes_t)   :: ElementNodes
    TYPE(Matrix_t), POINTER :: StiffMatrix

    REAL (KIND=DP), POINTER :: CellState(:), CellStatePrev(:), Temperature(:), ForceVector(:), CellStateKm1(:)

    REAL(KIND=dp), ALLOCATABLE ::  DeathHeatingTime(:), &
        DeathTemperature(:), AblationLength(:,:), &
        LocalMASS(:,:), LocalSTIFF(:,:), LocalFORCE(:), &
        ForwardRate, BackwardRate, AliveCoeff(:,:)
        REAL(KIND=dp), POINTER :: Vulnerable(:)

    REAL(KIND=dp) :: NonlinearTol, &
        Relax, SaveRelax, RelativeChange, Norm, PrevNorm, CumulativeTime,Time,PrevTime, &
        arealt, at, at0, srealt, st, totat, dt, totst,CPUTime,RealTime, ExponentialRate

    REAL(KIND=dp) :: Te, A, A_p, A_k, A_km1, D, D_p, D_k, D_km1, V_k, V_km1, Tk, kf, kfb, kb, fAk, fDk, fAkm1, fDkm1
!    REAL(KIND=dp),ALLOCATABLE:: PanchCells(:,:)
    INTEGER :: i, j, k, m, t, n, istat, LocalNodes, ADOFs, k_dof, TDOFs, CellStateModel, &
        NonlinearIter, iter

    INTEGER, POINTER :: CellStatePerm(:), TempPerm(:)
!    CHARACTER(LEN=256):: Panchfile
    LOGICAL :: AllocationsDone = .FALSE., FirstTime = .TRUE., found = .FALSE., &
        NonlinearIterAbort = .TRUE.,ConvergenceDone = .FALSE., EnforceVariableBounds = .FALSE.

    SAVE AllocationsDone, DeathTemperature, AblationLength, DeathHeatingTime, &
        FirstTime, Time,PrevTime,ElementNodes, LocalMASS, LocalSTIFF, LocalFORCE, &
        AliveCoeff, Vulnerable, EnforceVariableBounds, CellStateKm1

!------------------------------------------------------------------------------
!  Get variables needed for solution
!  CellState has 2 d.o.f. which correspond to alive and dead states
!------------------------------------------------------------------------------
    IF ( .NOT. ASSOCIATED( Solver % Matrix ) ) RETURN
    StiffMatrix => Solver % Matrix
    ForceVector => Solver % Matrix % RHS

    CellState     => Solver % Variable % Values
    CellStatePrev => Solver % Variable % PrevValues(:,1)
    CellStatePerm => Solver % Variable % Perm
    ADOFs =  Solver % Variable % DOFs
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
!------------------------------------------------------------------------------    
!    Total number of nodes in the mesh
!------------------------------------------------------------------------------    
    LocalNodes = Model % NumberOfNodes
!    ALLOCATE(PanchCells(LocalNodes,9))
!------------------------------------------------------------------------------ 
!    Read the death model in input file
!------------------------------------------------------------------------------    
    CellStateModel = GetInteger( Model % Simulation,'Cell State Model', Found )
    IF ( .NOT.Found ) CellStateModel = 2
!------------------------------------------------------------------------------
!  Allocate some permanent storage, this is done first time only
!------------------------------------------------------------------------------
    IF ( .NOT. AllocationsDone .OR. Solver % Mesh % Changed) THEN
    !------------------------------------------------------------------------------ 
        N = Solver % Mesh % MaxElementNodes
              
        IF ( AllocationsDone ) THEN
            IF (CellStateModel == 1) THEN
                DEALLOCATE( DeathTemperature,DeathHeatingTime,AblationLength )
            ELSE
                DEALLOCATE(ElementNodes % x,ElementNodes % y,ElementNodes % z, &
                LocalMASS, LocalSTIFF, LocalFORCE, &
                AliveCoeff,Vulnerable)
            END IF
        END IF

        IF (CellStateModel == 1) THEN
            ALLOCATE( DeathTemperature(ADOFs),DeathHeatingTime(ADOFs), &
                AblationLength(ADOFs,LocalNodes),STAT=istat )
        ELSE
          ALLOCATE( ElementNodes % x( N ), ElementNodes % y( N ), &
                ElementNodes % z( N ), LocalSTIFF( ADOFs*2*N,ADOFs*2*N ), & 
                LocalMASS( ADOFs*2*N,ADOFs*2*N ), LocalFORCE( ADOFs*2*N ), &
                AliveCoeff( ADOFs,N),&
                Vulnerable(LocalNodes), STAT=istat, CellStateKm1(ADOFs * LocalNodes))
        END IF    

        CellStatePrev = CellState

        IF ( istat /= 0 ) THEN
            CALL Fatal( 'NumaCellStateSolve', 'Memory allocation error.' )
        END IF
        
        AllocationsDone = .TRUE.
!------------------------------------------------------------------------------
!        Add vulnerable state to the variable list
!------------------------------------------------------------------------------    
        IF (CellStateModel == 2) THEN
            DO i=1,LocalNodes
                    Vulnerable(i) = 1 - CellState((i-1)*ADOFs+1) - CellState((i-1)*ADOFs+2)
            END DO
            CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
                Solver, 'Vulnerable', 1, Vulnerable, CellStatePerm )
!------------------------------------------------------------------------------
        END IF
!------------------------------------------------------------------------------
    END IF !.NOT. AllocationsDone
!------------------------------------------------------------------------------
!    Print some information
!------------------------------------------------------------------------------
    CALL Info( 'NumaCellStateSolve', ' ',Level=4 )
    CALL Info( 'NumaCellStateSolve', '-------------------------------------',Level=4 )
    CALL Info( 'NumaCellStateSolve', 'NUMA CELLS STATE SOLVER:  ', Level=4 )
    CALL Info( 'NumaCellStateSolve', '-------------------------------------',Level=4 )
    CALL Info( 'NumaCellStateSolve', 'Starting Computing...', Level=4 )
    CALL Info( 'NumaCellStateSolve', ' ',Level=4 )    
!------------------------------------------------------------------------------
!    Compute cell state in function of the model:
!------------------------------------------------------------------------------
!    Two-states simple model
!------------------------------------------------------------------------------
    IF (CellStateModel == 1) THEN
!------------------------------------------------------------------------------
!        Read the ablation conditions in input file
!------------------------------------------------------------------------------    
        DeathTemperature = 340.0D0
        DeathHeatingTime = 240.0D0
!------------------------------------------------------------------------------    
        DO k_dof = 1,TDOFs
!------------------------------------------------------------------------------ 
!            Limit temperature to be reached during DeathHeatingTime for death
!------------------------------------------------------------------------------
            DeathTemperature(k_dof) = GetConstReal( Model % &
                Materials(MIN(k_dof,Model % NumberOfMaterials)) % Values, &
                'Death Temperature', Found )
            IF (.NOT. Found) DeathTemperature(k_dof) = 340.0
!------------------------------------------------------------------------------ 
!            Time during which DeathTemperature has to be applied for death
!------------------------------------------------------------------------------        
            DeathHeatingTime(k_dof) = GetConstReal( Model % &
                Materials(MIN(k_dof,Model % NumberOfMaterials)) % Values, &
                'Death Heating Time', Found )
            IF (.NOT. Found) DeathHeatingTime(k_dof) = 240.0
!------------------------------------------------------------------------------         
        END DO !k_dof
!------------------------------------------------------------------------------
!        Ablation length = time during a cell has been heated over 
!        Death Temperature in a continuous way 
!------------------------------------------------------------------------------        
        IF( FirstTime) THEN
            AblationLength = 0.0D0    
            FirstTime = .FALSE.
            CALL Info( 'NumaCellStateSolve', 'NUMA CELLS STATE SOLVER:  ', Level=4 )
            CellState = 1.0D0
        END IF
!------------------------------------------------------------------------------
        IF ( ASSOCIATED( TempSol ) ) THEN
!------------------------------------------------------------------------------
            DO k_dof=1,TDOFs
!------------------------------------------------------------------------------
                DO i=1,LocalNodes
!------------------------------------------------------------------------------
!                 Get temperature and Compute cumulative time of ablation
!------------------------------------------------------------------------------
                    k = TempPerm(i)
                    m = CellStatePerm(i)
                    IF (Temperature((k-1)*TDOFs+k_dof) > DeathTemperature(k_dof)) THEN
                        AblationLength(k_dof,m) = AblationLength(k_dof,m) + Timestep
                    ELSE
                        AblationLength(k_dof,m) = 0.0
                    END IF                    
!------------------------------------------------------------------------------           
!                 Check cells death
!------------------------------------------------------------------------------           
                    IF (AblationLength(k_dof,m) >= DeathHeatingTime(k_dof)) THEN
                        CellState((m-1)*ADOFs+k_dof) = 0
                    END IF
!------------------------------------------------------------------------------        
                END DO    
!------------------------------------------------------------------------------    
            END DO
!------------------------------------------------------------------------------    
        END IF
!------------------------------------------------------------------------------
!     Compute the norm of the variable CellState
!------------------------------------------------------------------------------
        Solver % Variable % Norm = SQRT( SUM( CellState**2 ) / (ADOFs*LocalNodes) )
!------------------------------------------------------------------------------
!    Three-states model
!------------------------------------------------------------------------------
    ELSE
!------------------------------------------------------------------------------ 
!        Read the iteration scheme parameters in input file
!------------------------------------------------------------------------------ 
!        Maximum number of nonlinear iterations
!------------------------------------------------------------------------------    
        NonlinearIter = GetInteger( SolverParams,'Nonlinear System Max Iterations',Found )
        IF ( .NOT.Found ) NonlinearIter = 1
!------------------------------------------------------------------------------ 
!        Stop criteria if NonlinearTol not reached during NonlinearIter
!------------------------------------------------------------------------------        
        NonlinearIterAbort = GetLogical(SolverParams,'Nonlinear System Abort Not Converged',Found)
        IF ( .NOT.Found ) NonlinearIterAbort = .TRUE.
!------------------------------------------------------------------------------ 
!        Tolerance to be reached during nonlinear iterations
!------------------------------------------------------------------------------    
        NonlinearTol = GetConstReal( SolverParams,'Nonlinear System Convergence Tolerance', Found )
        IF ( .NOT.Found ) NonlinearTol = 3.0
!------------------------------------------------------------------------------ 
!        Relaxation factor for convergence of the nonlinear iterations
!------------------------------------------------------------------------------    
        Relax = GetCReal( SolverParams,'Nonlinear System Relaxation Factor',Found )
        IF ( .NOT.Found ) Relax = 1
!------------------------------------------------------------------------------
        SaveRelax = Relax
        CumulativeTime = 0.0d0
        dt = Timestep
!------------------------------------------------------------------------------
        FirstTime = .TRUE.
!------------------------------------------------------------------------------     
        DO WHILE( CumulativeTime < Timestep-1.0d-12 .OR. .NOT. TransientSimulation )
!------------------------------------------------------------------------------
!          The first time around this has been done by the caller
!------------------------------------------------------------------------------
            IF ( TransientSimulation .AND. .NOT.FirstTime ) THEN
                CALL InitializeTimestep(Solver)
            END IF
            FirstTime = .FALSE.
!------------------------------------------------------------------------------   
            totat = 0.0d0
            totst = 0.0d0
!------------------------------------------------------------------------------    
!          Get current (physical) time
!------------------------------------------------------------------------------
            TimeVar => VariableGet( Solver % Mesh % Variables, 'Time' )
            Time = TimeVar % Values(1)    
!------------------------------------------------------------------------------ 
!         Compute the norm of the solution
!------------------------------------------------------------------------------         
            Norm = SQRT( SUM( CellState**2 ) / (ADOFs*LocalNodes) )
!------------------------------------------------------------------------------    
!          Non linear iterations
!------------------------------------------------------------------------------
            ConvergenceDone = .FALSE.
!------------------------------------------------------------------------------
!            Do t=1,LocalNodes
!                k = CellStatePerm(t)
!                i=TempPerm(t)
!                PanchCells(t,1)=CellStateKm1(2 * k - 1)
!                PanchCells(t,2)=CellStateKm1(2 * k )
!                PanchCells(t,3)=CellState(2 * k - 1)
!                PanchCells(t,4)=CellState(2 * k )
!                PanchCells(t,5)=CellStatePrev(2 * k - 1)
!                PanchCells(t,6)=CellStatePrev(2 * k )
!                PanchCells(t,7)=Temperature((i-1)*TDOFs + TDOFs)
!            END Do
            DO iter=1,NonlinearIter
!------------------------------------------------------------------------------
                at  = CPUTime()
                at0 = RealTime()
                arealt = RealTime()

                CALL Info( 'NumaCellStateSolve', ' ', Level=4 )
                CALL Info( 'NumaCellStateSolve', '-------------------------------------',Level=4 )
                WRITE( Message,* ) 'CELLS STATE non-linear iteration', iter
                CALL Info( 'NumaCellStateSolve', Message, Level=4 )
                CALL Info( 'NumaCellStateSolve', '-------------------------------------',Level=4 )
                CALL Info( 'NumaCellStateSolve', ' ', Level=4 )
                CALL Info( 'NumaCellStateSolve', 'Starting Assembly...', Level=4 )            
!------------------------------------------------------------------------------
                CALL DefaultInitialize()
!------------------------------------------------------------------------------
!                Assembly of the system
!------------------------------------------------------------------------------
!              Go through bulk and boundary elements
!------------------------------------------------------------------------------
               ForwardRate = GetConstReal( SolverParams, 'Forward Rate',Found)
               IF ( .NOT.Found ) ForwardRate = 0.00333
!--------------------------------------------------------------------------    
!                    BackwardRate: used in [vulnerable to alive] transition
!--------------------------------------------------------------------------    
               BackwardRate = GetConstReal( SolverParams, 'Backward Rate',Found)
               IF ( .NOT.Found ) BackwardRate = 0.00777
!--------------------------------------------------------------------------    
!                    ExponentialRate: used in forward rate exponential expression
!--------------------------------------------------------------------------    
               ExponentialRate = GetConstReal( SolverParams, 'Exponential Rate', Found)
               IF ( .NOT.Found ) ExponentialRate = 40.5
                DO t=1,LocalNodes
!------------------------------------------------------------------------------    
!                 Get the element nodes characteristics
!------------------------------------------------------------------------------
!                    Compute nodal values of parameters from input file values 
!------------------------------------------------------------------------------    
!                    Modification of ForwardRate in function of Temperature and ExponentialRate
!--------------------------------------------------------------------------    

                    kfb = ForwardRate
                    kb = BackwardRate
                    Tk = ExponentialRate

!                    IF ( ASSOCIATED( TempSol ) ) THEN
!                            k=  TempPerm(t)
!                            kfb = kfb * &
!                              exp ( (Temperature((k-1)*TDOFs + TDOFs)-273.15)/ ExponentialRate) - &
!                              kfb * &
!                              exp ( (312-273.15)/ ExponentialRate)*0.1
!                    END IF
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------    
!                    Compute nodal value of coefficient (1-A) from previous value
!------------------------------------------------------------------------------    
                    k = CellStatePerm(t)
                    i = TempPerm(t)

                    A_km1 = CellStateKm1(2 * k - 1)
                    D_km1 = CellStateKm1(2 * k)
                    V_km1 = 1 - A_km1 - D_km1

                    A_k = CellState(2 * k - 1)
                    D_k = CellState(2 * k)
                    V_k = 1 - A_k - D_k

                    CellStateKm1(2 * k - 1) = CellState(2 * k - 1)
                    CellStateKm1(2 * k) = CellState(2 * k)

                    A_p = CellStatePrev(2 * k - 1)
                    D_p = CellStatePrev(2 * k)

                    Te = Temperature((i-1)*TDOFs + TDOFs)

                    kfb = kfb * (exp((Te - 273.15) / Tk) - exp((312 - 273.15) / Tk) * 0.1)

                    fAk = A_k - A_p - dt * (- kfb * (1 - A_k) * A_k + kb * V_k)
                    fDk = D_k - D_p - dt * kfb * (1 - A_k) * V_k
                    fAkm1 = A_km1 - A_p - dt * (- kfb * (1 - A_km1) * A_km1 + kb * V_km1)
                    fDkm1 = D_km1 - D_p - dt * kfb * (1 - A_km1) * V_km1

                    IF (iter > 1 .AND. ABS(fAk - fAkm1) > 0 .AND. &
                                ABS(fDk - fDkm1) > 0) THEN
                        CellState(2 * k - 1) = ( A_km1 * fAk - A_k * fAkm1 ) / (fAk - fAkm1)
                        CellState(2 * k) = ( D_km1 * fDk - D_k * fDkm1 ) / (fDk - fDkm1)
                    ELSE
                        CellState(2 * k - 1) = A_k - fAk
                        CellState(2 * k) = D_k - fDk
                    END IF


!------------------------------------------------------------------------------
    !                Solving of the system
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------    
                END DO ! Bulk and boundary elements
!------------------------------------------------------------------------------
!             Compute assembly CPU time, save current CPU time for solving CPU time below, 
!             and save current solution norm
!------------------------------------------------------------------------------
                at = CPUTime() - at
                arealt = RealTime() -arealt
                st = CPUTime()
                srealt = RealTime()
                Solver % Variable % Norm = SQRT( SUM( CellState**2 ) / (ADOFs*LocalNodes) )

!------------------------------------------------------------------------------
!                Test to enforce bounding of the variables:
!------------------------------------------------------------------------------
                EnforceVariableBounds = ListGetLogical( SolverParams, 'Enforce Variable Bounds ', Found )
                IF (.NOT. Found) EnforceVariableBounds = .FALSE.
                IF (EnforceVariableBounds) THEN
                    DO i=1,LocalNodes
                        IF(CellState((i-1)*ADOFs+1)>1.0) CellState((i-1)*ADOFs+1)=1.0
                        IF(CellState((i-1)*ADOFs+2)>1.0) CellState((i-1)*ADOFs+2)=1.0
                        IF(CellState((i-1)*ADOFs+1)<0.0) CellState((i-1)*ADOFs+1)=0.0
                        IF(CellState((i-1)*ADOFs+2)<0.0) CellState((i-1)*ADOFs+2)=0.0
                    END DO
                END IF
!------------------------------------------------------------------------------
!             Compute solving CPU time, and total assembly and solving CPU time in the 
!             coupled system iteration (may contain several nonlinear iterations) 
!------------------------------------------------------------------------------
                CALL Info( 'NumaCellStateSolve', 'Solving done', Level=4 )                
                st = CPUTIme()-st
                srealt = RealTime()-srealt
                totat = totat + at
                totst = totst + st
                WRITE(Message,'(a,i4,a,F8.2,F8.2)') 'iter: ',iter,', Assembly CPU Time (nonlinear it., coupled it.): (s)', at, totat
                CALL Info( 'NumaCellStateSolve', Message, Level=4 )
                WRITE(Message,'(a,i4,a,F8.2)') 'iter: ',iter,', Assembly Real Time (nonlinear it): (s)', arealt
                CALL Info( 'NumaCellStateSolve', Message, Level=4 )
                WRITE(Message,'(a,i4,a,F8.2,F8.2)') 'iter: ',iter,', Solve CPU Time (nonlinear it., coupled it.):    (s)', st, totst
                CALL Info( 'NumaCellStateSolve', Message, Level=4 )
                WRITE(Message,'(a,i4,a,F8.2)') 'iter: ',iter,', Solve Real Time (nonlinear it): (s)', srealt
                CALL Info( 'NumaCellStateSolve', Message, Level=4 )
!------------------------------------------------------------------------------
!             Compute the change in norm of the solution 
!------------------------------------------------------------------------------
                PrevNorm = Norm
                Norm = SQRT( SUM( CellState**2 ) / (ADOFs*LocalNodes) )
                IF ( PrevNorm + Norm /= 0.0d0 ) THEN
                    RelativeChange = 2.0d0 * ABS( PrevNorm-Norm ) / (PrevNorm + Norm)
                ELSE
                    RelativeChange = 0.0d0
                END IF

                WRITE( Message, * ) 'Result Norm   : ',Norm
                CALL Info( 'NumaCellStateSolve', Message, Level=4 )
                WRITE( Message, * ) 'Relative Change : ',RelativeChange
                CALL Info( 'NumaCellStateSolve', Message, Level=4 )
!------------------------------------------------------------------------------
!             If NonlinearTol is reached, stop the nonlinear iterations before maxiter: 
!------------------------------------------------------------------------------
                ConvergenceDone = .FALSE.
                IF ( RelativeChange < NonlinearTol ) ConvergenceDone = .TRUE.
                IF(ParEnv % PEs>1) CALL ParallelAllReduceAnd( ConvergenceDone )
            IF( ConvergenceDone ) EXIT
!------------------------------------------------------------------------------
            END DO ! of the nonlinear iteration
!            Read(*,*) Panchfile
!            open(unit=1099,status='unknown',file=Panchfile)
            Do t=1,LocalNodes
                k = CellStatePerm(t)
                CellState(2 * k - 1) = MIN(MAX(CellState(2 * k - 1), 0.0), 1.0)
                CellState(2 * k) = MIN(MAX(CellState(2 * k), 0.0), 1.0)
!                PanchCells(t,8)=CellState(2 * k - 1)
!                PanchCells(t,9)=CellState(2 * k )
!                Write(1099,*) PanchCells(t,1:9)
            END Do
!------------------------------------------------------------------------------
!          Check if convergence is reached towards specified tolerance >0
!------------------------------------------------------------------------------    
            IF ( (NonlinearIterAbort) .AND. (NonlinearTol/=0) .AND. ( RelativeChange > NonlinearTol ) &
                ) THEN
                WRITE( Message, * ) 'No convergence reached during the ', NonlinearIter,' non-linear iterations '
                CALL Error( 'NumaCellStateSolve', Message )
                CALL Fatal( 'NumaCellStateSolve', ' ' )
            END IF
!------------------------------------------------------------------------------           
!         Save Time as Previous Time for next iteration
!------------------------------------------------------------------------------
            PrevTime = TimeVar % Values(1)
!------------------------------------------------------------------------------
!         Compute cumulative time done by now and time remaining
!------------------------------------------------------------------------------
            IF ( .NOT. TransientSimulation ) EXIT
            CumulativeTime = CumulativeTime + dt
            dt = Timestep - CumulativeTime
!------------------------------------------------------------------------------
        END DO ! end of timestep
!------------------------------------------------------------------------------
        Solver % dt = Timestep
!------------------------------------------------------------------------------
        CALL  ListAddConstReal( Solver % Values,'Nonlinear System Relaxation Factor', SaveRelax )
!------------------------------------------------------------------------------
!        Compute vulnerable state
!------------------------------------------------------------------------------
        DO i=1,LocalNodes
            Vulnerable(i) = 1 - CellState((i-1)*ADOFs+1) - CellState((i-1)*ADOFs+2)
        END DO
!------------------------------------------------------------------------------
!        For multi mesh resolution:
!------------------------------------------------------------------------------
        CALL InvalidateVariable( Model % Meshes, Solver % Mesh, 'Vulnerable')
!------------------------------------------------------------------------------
    END IF ! Cell death model = 2
!------------------------------------------------------------------------------                
    CALL Info( 'NumaCellStateSolve', 'State of cells Computing done', Level=4 )
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------ 
END SUBROUTINE NumaCellStateSolver
!------------------------------------------------------------------------------



