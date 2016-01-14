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
! *****************************************************************************/
! *  Subroutines for the detection of died cells
! *****************************************************************************/
! *  25/08/09:
! *  By reading the input file, get the conditions of ablation (temperature, length)
! *  and detect died cells 
! *  Use the temperature of blood and tissue in input
! *  Output the marker Alive = 0 or 1 (used by NumaHeatSolve)
! *****************************************************************************/
! *****************************************************************************/
! * List of subroutines:
! *****************************************************************************/
! *  - NumaCellStateSolver
! *****************************************************************************/
! *****************************************************************************/
! *  15/02/10: 
! *  - Change of the name of the solver to NumaCellStateSolve.src 
! *         (main routine become NumaCellStateSolver())
!    *  - Implementation of 3-State Death model
! *****************************************************************************/
! *****************************************************************************/
! *  31/03/10: 
! *  - For model 1, dt replaced by Timestep for computation of ablationlength 
! *****************************************************************************/
! *****************************************************************************/
! *  09/04/10: 
! *  - Read cell state model in simulation section
! *****************************************************************************/
! *****************************************************************************/
! *  29/07/10: 
! *  - After solving, enforce bounds of Alive and Dead bounds to be 0 and 1, if
! *  keyword "Enforce Variable Bounds" set to true in input file
! *****************************************************************************/
! *****************************************************************************/
! *  26/11/10: 
! *  - Call to NumaSetInitialConditions() removed as initialization of all
! *  variables already done in NumaHeatSolve.src when NumaSetInitialConditions()
! *  is called.
! *****************************************************************************/
! *****************************************************************************/
! *  25/01/11:
! *  - Default value of CellStateModel set to 2
! *  - Default value of NonlinearTol set to 3.0
! *  - Default value of EnforceVariableBounds set to .FALSE.
! *  - Default value of ForwardRate set to 0.00333
! *  - Default value of BackwardRate set to 0.00777
! *  - Default value of ExponentialRate set to 40.5
! *  - Default value of DeathTemperature set to 340.0
! *  - Default value of DeathHeatingTime set to 240.0
! *****************************************************************************/
! *****************************************************************************/
! *  31/01/11:
! *  - Correction of default value setting for ForwardRate and BackwardRate
! *  (each dof done separately)
! *****************************************************************************/
! *****************************************************************************/
! *  16/03/11:
! *  - Definition of NumaSetInitialConditions() removed
! *****************************************************************************/
!------------------------------------------------------------------------------
SUBROUTINE NumaCellStateSolver( Model,Solver,Timestep,TransientSimulation )
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
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
!------------------------------------------------------------------------------    
    IMPLICIT NONE
!------------------------------------------------------------------------------
    TYPE(Model_t) :: Model
    TYPE(Solver_t), TARGET:: Solver
    REAL (KIND=DP) :: Timestep
    LOGICAL :: TransientSimulation
!------------------------------------------------------------------------------
!  Local variables
!------------------------------------------------------------------------------
    TYPE(Variable_t), POINTER :: TempSol
    TYPE(Variable_t), POINTER :: TimeVar
    TYPE(ValueList_t), POINTER :: SolverParams
    TYPE(Element_t),POINTER :: Element
    TYPE(Nodes_t)   :: ElementNodes
    TYPE(Matrix_t), POINTER :: StiffMatrix

    REAL (KIND=DP), POINTER :: CellState(:), Temperature(:), ForceVector(:)

    REAL(KIND=dp), ALLOCATABLE ::  DeathHeatingTime(:), &
        DeathTemperature(:), AblationLength(:,:), &
        LocalMASS(:,:), LocalSTIFF(:,:), LocalFORCE(:), &
        ForwardRate(:,:), BackwardRate(:,:), AliveCoeff(:,:)
        REAL(KIND=dp), POINTER :: Vulnerable(:)

    REAL(KIND=dp) :: NonlinearTol, &
        Relax, SaveRelax, RelativeChange, Norm, PrevNorm, CumulativeTime,Time,PrevTime, &
        arealt, at, at0, srealt, st, totat, dt, totst,CPUTime,RealTime, ExponentialRate
    
    INTEGER :: i, j, k, m, t, n, istat, LocalNodes, ADOFs, k_dof, TDOFs, CellStateModel, &
        NonlinearIter, iter 
    
    INTEGER, POINTER :: CellStatePerm(:), TempPerm(:)
    
    LOGICAL :: AllocationsDone = .FALSE., FirstTime = .TRUE., found = .FALSE., &
        NonlinearIterAbort = .TRUE.,ConvergenceDone = .FALSE., EnforceVariableBounds = .FALSE.

    SAVE AllocationsDone, DeathTemperature, AblationLength, DeathHeatingTime, &
        FirstTime, Time,PrevTime,ElementNodes, LocalMASS, LocalSTIFF, LocalFORCE, &
        ForwardRate, BackwardRate, AliveCoeff, Vulnerable, EnforceVariableBounds
!------------------------------------------------------------------------------
!  Get variables needed for solution
!  CellState has 2 d.o.f. which correspond to alive and dead states
!------------------------------------------------------------------------------
    IF ( .NOT. ASSOCIATED( Solver % Matrix ) ) RETURN
    StiffMatrix => Solver % Matrix
    ForceVector => Solver % Matrix % RHS

    CellState     => Solver % Variable % Values
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
                ForwardRate, BackwardRate, AliveCoeff,Vulnerable)
            END IF
        END IF

        IF (CellStateModel == 1) THEN
            ALLOCATE( DeathTemperature(ADOFs),DeathHeatingTime(ADOFs), &
                AblationLength(ADOFs,LocalNodes),STAT=istat )
        ELSE
          ALLOCATE( ElementNodes % x( N ), ElementNodes % y( N ), &
                ElementNodes % z( N ), LocalSTIFF( ADOFs*2*N,ADOFs*2*N ), & 
                LocalMASS( ADOFs*2*N,ADOFs*2*N ), LocalFORCE( ADOFs*2*N ), &
                ForwardRate( ADOFs,N), BackwardRate( ADOFs,N), AliveCoeff( ADOFs,N),&
                Vulnerable(LocalNodes), STAT=istat )
        END IF    

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
            TimeVar => VariableGet( CurrentModel % Solver % Mesh % Variables, 'Time' )
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
                DO t=1,Solver % Mesh % NumberOfBulkElements + Solver % Mesh % NumberOfBoundaryElements
!------------------------------------------------------------------------------    
                    IF (t<=Solver % Mesh % NumberOfBulkElements) THEN
                        Element => GetActiveElement(t)
                    ELSE
                        Element => GetBoundaryElement(t-Solver % Mesh % NumberOfBulkElements)
                    END IF
!------------------------------------------------------------------------------
!                 Get the element nodes characteristics
!------------------------------------------------------------------------------
                    n = GetElementNOFNodes()
                    CALL GetElementNodes( ElementNodes )            
!------------------------------------------------------------------------------    
                    IF ( RealTime() - at0 > 1.0 ) THEN
                        WRITE(Message,'(a,i3,a)' ) '   Assembly: ', INT(100.0 - 100.0 * &
                            (Solver % Mesh % NumberOfBulkElements + &
                            Solver % Mesh % NumberOfBoundaryElements-t) / &
                            (1.0*Solver % Mesh % NumberOfBulkElements + &
                            Solver % Mesh % NumberOfBoundaryElements)), ' % done'          
                        CALL Info( 'NumaCellStateSolve', Message, Level=5 )
                        at0 = RealTime()
                    END IF
!------------------------------------------------------------------------------    
!                    Compute nodal values of parameters from input file values 
!------------------------------------------------------------------------------    
                    ForwardRate = 0.0D0
                    ForwardRate = 0.0D0
                    ExponentialRate = 0.0D0
!------------------------------------------------------------------------------
                    DO k_dof =1,ADOFs
!------------------------------------------------------------------------------    
!                        ForwardRate: used in [alive to vulnerable] and [vulnerable to dead] transitions
!------------------------------------------------------------------------------    
                        ForwardRate(k_dof,1:n) = GetReal( SolverParams, 'Forward Rate',Found)
                        IF ( .NOT.Found ) ForwardRate(k_dof,:) = 0.00333
!------------------------------------------------------------------------------    
!                        BackwardRate: used in [vulnerable to alive] transition
!------------------------------------------------------------------------------    
                        BackwardRate(k_dof,1:n) = GetReal( SolverParams, 'Backward Rate',Found)
                        IF ( .NOT.Found ) BackwardRate(k_dof,:) = 0.00777
!------------------------------------------------------------------------------    
!                        ExponentialRate: used in forward rate exponential expression
!------------------------------------------------------------------------------    
                        ExponentialRate = GetConstReal( SolverParams, 'Exponential Rate', Found)
                        IF ( .NOT.Found ) ExponentialRate = 40.5
!------------------------------------------------------------------------------    
!                        Modification of ForwardRate in function of Temperature and ExponentialRate
!------------------------------------------------------------------------------    
                        IF ( ASSOCIATED( TempSol ) ) THEN

                            DO i=1,n
                                k=TempPerm(Element % NodeIndexes(i))
!!!!TEST!!!!!!!!!!11/03/11
                                
                                    !ForwardRate(k_dof,i) = ForwardRate(k_dof,i) * &
                                    !    exp ( (Temperature((k-1)*TDOFs + TDOFs)-273.15)/ ExponentialRate)

                                    ForwardRate(k_dof,i) = ForwardRate(k_dof,i) * &
                                        exp ( (Temperature((k-1)*TDOFs + TDOFs)-273.15)/ ExponentialRate) - &
                                        ForwardRate(k_dof,i) * &
                                        exp ( (312-273.15)/ ExponentialRate)*0.1
                
                            END DO
                        END IF
!------------------------------------------------------------------------------                    
                    END DO ! k_dof
!------------------------------------------------------------------------------    
!                    Compute nodal value of coefficient (1-A) from previous value
!------------------------------------------------------------------------------    
                    AliveCoeff = 0.0D0                    
                    DO k_dof =1,ADOFs                        
                        DO i=1,n
                            k=CellStatePerm(Element % NodeIndexes(i))
                            AliveCoeff(k_dof,i) = 1 - CellState((k-1)*ADOFs + 1)
                        END DO
                    END DO ! k_dof
!------------------------------------------------------------------------------    
!                    Compute element contribution
!------------------------------------------------------------------------------    
                    CALL CellStateCompose( LocalMASS,LocalSTIFF,LocalFORCE,  &
                        ForwardRate, BackwardRate, AliveCoeff, Element, n, ElementNodes, ADOFs )
!------------------------------------------------------------------------------
!                 If time dependent simulation, add mass matrix to stiff matrix
!------------------------------------------------------------------------------
                    IF ( TransientSimulation ) THEN
                        CALL Add1stOrderTime( LocalMASS, LocalSTIFF, LocalFORCE, dt, n, ADOFs, &
                        CellStatePerm(Element % NodeIndexes(1:n)), Solver )     
                    END IF   
!------------------------------------------------------------------------------
!                 Update global matrices from local matrices
!------------------------------------------------------------------------------
                    CALL UpdateGlobalEquations( StiffMatrix, LocalSTIFF, &
                        forcevector, LocalFORCE, n, ADOFs, CellStatePerm(Element % NodeIndexes) )     
!------------------------------------------------------------------------------    
                END DO ! Bulk and boundary elements
!------------------------------------------------------------------------------
                CALL Info( 'NumaCellStateSolve', 'Assembly done', Level=4 )
!------------------------------------------------------------------------------
!             Compute assembly CPU time, save current CPU time for solving CPU time below, 
!             and save current solution norm
!------------------------------------------------------------------------------
                at = CPUTime() - at
                arealt = RealTime() -arealt
                st = CPUTime()
                srealt = RealTime()
!------------------------------------------------------------------------------
!                Solving of the system
!------------------------------------------------------------------------------
                CALL SolveSystem( StiffMatrix, ParMatrix, ForceVector, &
                    CellState, Solver % Variable % Norm, ADOFs, Solver )
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


CONTAINS



!------------------------------------------------------------------------------
    SUBROUTINE CellStateCompose( MassMatrix,StiffMatrix,ForceVector,  &
        NodalForwardRate, NodalBackwardRate, NodalAliveCoeff, Element, n, Nodes, NDOFs )
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Return element local matrices and RHS vector for equation:
!
!     dA/dt + [kf*(1-A) + kb]*A + kb*D = kb
!     dD/dt + kf*(1-A)*D + kf*(1-A)*A = kf(1-A)
!
!     with kf <= NodalForwardRate, bf <= NodalBackwardRate, (1-A) <= NodalAliveCoeff
!
!  ARGUMENTS:
!
!  REAL(KIND=dp) :: MassMatrix(:,:)
!     OUTPUT: time derivative coefficient matrix
!
!  REAL(KIND=dp) :: StiffMatrix(:,:)
!     OUTPUT: rest of the equation coefficients
!
!  REAL(KIND=dp) :: ForceVector(:)
!     OUTPUT: RHS vector
!
!  REAL(KIND=dp) :: NodalForwardRate(:,:)
!     INPUT: Nodal values of the forward rate
!
!  REAL(KIND=dp) :: NodalBackwardRate(:,:)
!     INPUT: Nodal values of the backward rate
!
!  REAL(KIND=dp) :: NodalAliveCoeff(:,:)
!     INPUT: Nodal values of the Alive Coeff (1-A)
!
!  TYPE(Element_t) :: Element
!       INPUT: Structure describing the element (dimension,nof nodes,
!               interpolation degree, etc...)
!
!  INTEGER :: n
!       INPUT: Number of element nodes
!
!  TYPE(Nodes_t) :: Nodes
!       INPUT: Element node coordinates
!
!  INTEGER :: NDOFs
!       INPUT: Number of degrees of freedom of the solution
!******************************************************************************
        REAL(KIND=dp) :: ForceVector(:), MassMatrix(:,:),StiffMatrix(:,:), &
            NodalForwardRate(:,:), NodalBackwardRate(:,:), NodalAliveCoeff(:,:)

        INTEGER :: n, NDOFs

        TYPE(Nodes_t) :: Nodes
        TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
!        Local variables
!------------------------------------------------------------------------------
        REAL(KIND=dp) :: Basis(n), SqrtElementMetric, U, V, W, S, &
            Alpha,Beta,M,L,FR(NDOFs),BR(NDOFs)

        INTEGER :: i,j,k,p,q,t,dim,k_dof

        LOGICAL :: stat

        TYPE(GaussIntegrationPoints_t), TARGET :: IntegStuff
!------------------------------------------------------------------------------
!        Get model dimension
!------------------------------------------------------------------------------
        dim = CoordinateSystemDimension()
!------------------------------------------------------------------------------
!        Initialization
!------------------------------------------------------------------------------
        ForceVector = 0.0D0
        StiffMatrix = 0.0D0
        MassMatrix  = 0.0D0
        Alpha = 0.0D0
        Beta = 0.0D0
        M = 0.0D0
        L = 0.0D0
!------------------------------------------------------------------------------
!   Integration stuff
!------------------------------------------------------------------------------
        IntegStuff = GaussPoints( element )
!------------------------------------------------------------------------------
!        Loop over Gauss integration points
!------------------------------------------------------------------------------
        DO t=1,IntegStuff % n
!------------------------------------------------------------------------------
                U = IntegStuff % u(t)
                V = IntegStuff % v(t)
                W = IntegStuff % w(t)
                S = IntegStuff % s(t)
!------------------------------------------------------------------------------
!     Basis function values & derivatives at the integration point
!------------------------------------------------------------------------------
            stat = ElementInfo( Element,Nodes,U,V,W,SqrtElementMetric,Basis )
            s = SqrtElementMetric * S    
!------------------------------------------------------------------------------
!     Interpolation of parameters at the integration point
!------------------------------------------------------------------------------
            DO k_dof=1,NDOFs
                FR(k_dof) = SUM( NodalForwardRate(k_dof,1:n) * Basis(1:n) ) * &
                    SUM( NodalAliveCoeff(k_dof,1:n) * Basis(1:n) )  
                BR(k_dof) = SUM( NodalBackwardRate(k_dof,1:n) * Basis(1:n) ) 
            END DO    
!------------------------------------------------------------------------------
!     Computation of contribution (integration with Gauss quadrature)   
!------------------------------------------------------------------------------
            DO k_dof=1,NDOFs
!------------------------------------------------------------------------------
                DO p=1,n
!------------------------------------------------------------------------------
                    DO q=1,n
!------------------------------------------------------------------------------
!           Stiff matrix
!------------------------------------------------------------------------------
                        Alpha = FR(k_dof) * Basis(q) * Basis(p)
                        Beta = BR(k_dof) * Basis(q) * Basis(p)
!------------------------------------------------------------------------------
                        IF (k_dof<NDOFs) THEN 
                            StiffMatrix(NDOFs*(p-1)+k_dof,NDOFs*(q-1)+k_dof) = &
                                StiffMatrix(NDOFs*(p-1)+ k_dof,NDOFs*(q-1)+k_dof) + s * &
                              ( Alpha + Beta )
                            StiffMatrix(NDOFs*(p-1)+k_dof,NDOFs*(q-1)+k_dof+1) = &
                                StiffMatrix(NDOFs*(p-1)+ k_dof,NDOFs*(q-1)+k_dof+1) + s * Beta
                        ELSE    
                            StiffMatrix(NDOFs*(p-1)+k_dof,NDOFs*(q-1)+k_dof) = &
                                StiffMatrix(NDOFs*(p-1)+k_dof,NDOFs*(q-1)+k_dof) + s * Alpha    
                            StiffMatrix(NDOFs*(p-1)+k_dof,NDOFs*(q-1)+k_dof-1) = &
                                StiffMatrix(NDOFs*(p-1)+k_dof,NDOFs*(q-1)+k_dof-1) + s * Alpha    
                        END IF
!------------------------------------------------------------------------------
!           Mass matrix (time derivative)
!------------------------------------------------------------------------------                
                        M = Basis(q) * Basis(p)
!------------------------------------------------------------------------------    
                        MassMatrix(NDOFs*(p-1)+k_dof,NDOFs*(q-1)+k_dof)  = &
                            MassMatrix(NDOFs*(p-1)+k_dof,NDOFs*(q-1)+k_dof)  + s * M
!------------------------------------------------------------------------------                                        
                    END DO ! q
!------------------------------------------------------------------------------                
                END DO ! p
!------------------------------------------------------------------------------
!                Right-Hand side
!------------------------------------------------------------------------------
                DO q=1,n            
!------------------------------------------------------------------------------
                    IF (k_dof<NDOFs) THEN 
                        L = BR(k_dof) * Basis(q)
                    ELSE    
                        L = FR(k_dof) * Basis(q)
                    END IF
!------------------------------------------------------------------------------
                    ForceVector(NDOFs*(q-1)+k_dof) = ForceVector(NDOFs*(q-1)+k_dof) + s * L
!------------------------------------------------------------------------------                
                END DO ! q
!------------------------------------------------------------------------------                                
            END DO ! k_dof
!------------------------------------------------------------------------------
        END DO ! Gauss integration points
!------------------------------------------------------------------------------
   END SUBROUTINE CellStateCompose
!------------------------------------------------------------------------------







!------------------------------------------------------------------------------ 
END SUBROUTINE NumaCellStateSolver
!------------------------------------------------------------------------------


