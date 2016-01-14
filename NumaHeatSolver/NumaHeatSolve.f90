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


SUBROUTINE NumaHeatSolver_init( Model,Solver,Timestep,TransientSimulation )

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
            NextFreeKeyword('Exported Variable',SolverParams),'Heat Source Calc')
END SUBROUTINE NumaHeatSolver_init

!******************************************************************************
!------------------------------------------------------------------------------
SUBROUTINE NumaHeatSolver( Model,Solver,Timestep,TransientSimulation )
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Solve the heat equation 
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
!   LOGICAL :: TransientSimulation
!       INPUT: Steady state or transient simulation
!
!******************************************************************************
    USE MaterialModels
    USE DefUtils
    USE NumaAdaptive
!------------------------------------------------------------------------------
    IMPLICIT NONE
!------------------------------------------------------------------------------
    TYPE(Model_t)  :: Model
    TYPE(Solver_t), TARGET :: Solver
    LOGICAL :: TransientSimulation
    REAL(KIND=dp) :: Timestep
!------------------------------------------------------------------------------
!  Local variables
!------------------------------------------------------------------------------
    TYPE(Matrix_t), POINTER :: StiffMatrix
    TYPE(Variable_t), POINTER :: TempSol
    TYPE(ValueList_t), POINTER :: SolverParams
    TYPE(Variable_t), POINTER :: TimeVar

    INTEGER :: iter,body_id,LocalNodes, &
        NonlinearIter, ios,MaxNonLinearIterDone, &
        MaxCouplingIterDone,CouplingIter, &
        ierr

    INTEGER, POINTER :: TempPerm(:)

    CHARACTER(LEN=100) :: char_MyPe, iterationsFilename, &
        TestName, TotalPowerFilename

    LOGICAL :: FirstTime, &
        Found, AllocationsDone = .FALSE., NonlinearIterAbort = .TRUE., &
        ControlIterations = .FALSE., &
        ConvergenceDone = .FALSE., &
        ControlTotalPower= .FALSE., &
        TemperatureControlledPower = .FALSE.

    REAL(KIND=dp) :: NonlinearTol, Relax, at, at0, dt, &
        SaveRelax, CumulativeTime, RelativeChange, Norm, PrevNorm, &
        totat,st,totst,CPUTime,RealTime,Time,PrevTime, &
        MeshSize,srealt,arealt, &
        Integ_Force, &
        TotalPower, &
        tmpPower, TargetTemperature

    REAL(KIND=dp), POINTER :: Temperature(:), ForceVector(:), &
        HeatSource(:)

    REAL(KIND=dp), ALLOCATABLE ::  &
        PreviousPower(:),CurrentPower(:), &
        IntegErrorToTargetTemperature(:),DerivErrorToTargetTemperature(:)

    SAVE AllocationsDone, LocalNodes, Time, PrevTime, MaxNonLinearIterDone, MaxCouplingIterDone, &
        CouplingIter, TestName, Integ_Force, HeatSource, NonlinearIter, NonlinearIterAbort, &
        NonlinearTol, Relax, ControlIterations, ControlTotalPower, TotalPower, &
        TemperatureControlledPower, TargetTemperature, PreviousPower, CurrentPower, &
        IntegErrorToTargetTemperature, DerivErrorToTargetTemperature
!------------------------------------------------------------------------------
!  Interfaces
!------------------------------------------------------------------------------     
    INTERFACE
!------------------------------------------------------------------------------
        FUNCTION TemperatureBoundaryResidual( Model,Edge,Mesh,Quant,Perm,Gnorm,NDOFs) RESULT(Indicator)
            USE Types
            TYPE(Element_t), POINTER :: Edge
            TYPE(Model_t) :: Model
            TYPE(Mesh_t), POINTER :: Mesh
            REAL(KIND=dp) :: Quant(:), Indicator(2), Gnorm
            INTEGER :: Perm(:)
            INTEGER :: NDOFs
        END FUNCTION TemperatureBoundaryResidual
!------------------------------------------------------------------------------
        FUNCTION TemperatureEdgeResidual( Model,Edge,Mesh,Quant,Perm,NDOFs) RESULT(Indicator)
            USE Types
            TYPE(Element_t), POINTER :: Edge
            TYPE(Model_t) :: Model
            TYPE(Mesh_t), POINTER :: Mesh
            REAL(KIND=dp) :: Quant(:), Indicator(2)
            INTEGER :: Perm(:)
            INTEGER :: NDOFs
        END FUNCTION TemperatureEdgeResidual
!------------------------------------------------------------------------------
        FUNCTION TemperatureInsideResidual( Model,Element,Mesh,Quant,Perm, Fnorm,NDOFs) RESULT(Indicator)
            USE Types
            TYPE(Element_t), POINTER :: Element
            TYPE(Model_t) :: Model
            TYPE(Mesh_t), POINTER :: Mesh
            REAL(KIND=dp) :: Quant(:), Indicator(2), Fnorm
            INTEGER :: Perm(:)
            INTEGER :: NDOFs
        END FUNCTION TemperatureInsideResidual

        SUBROUTINE BulkAssembly(Solver, Model, TempSol, StiffMatrix, ForceVector, TransientSimulation, dt, &
                TotalPower, Integ_Force, &
                TempPerm, Temperature, PreviousPower, CurrentPower, &
                IntegErrorToTargetTemperature, DerivErrorToTargetTemperature, TargetTemperature, &
                Time)
            USE Types
            TYPE(Matrix_t), POINTER :: StiffMatrix
            TYPE(Model_t)  :: Model
            TYPE(Solver_t), TARGET :: Solver
            TYPE(Variable_t), POINTER :: TempSol
            REAL(KIND=dp) :: dt, TotalPower, IntegErrorToTargetTemperature(:), &
                DerivErrorToTargetTemperature(:), PreviousPower(:), &
                CurrentPower(:), TargetTemperature, Integ_Force, Time
            REAL(KIND=dp), POINTER :: Temperature(:), ForceVector(:)
            INTEGER, POINTER :: TempPerm(:)
            LOGICAL :: TransientSimulation
        END SUBROUTINE BulkAssembly

        SUBROUTINE AdditionalComputations(Solver, Model, Temperature, TempPerm, IntegErrorToTargetTemperature, &
                DerivErrorToTargetTemperature, TargetTemperature, HeatSource, dt)
            USE Types
            TYPE(Solver_t), TARGET :: Solver
            TYPE(Model_t) :: Model
            REAL(KIND=dp), POINTER :: HeatSource(:), Temperature(:)
            REAL(KIND=dp) :: dt, IntegErrorToTargetTemperature(:), DerivErrorToTargetTemperature(:), &
                TargetTemperature
            INTEGER, POINTER :: TempPerm(:)
        END SUBROUTINE AdditionalComputations

        SUBROUTINE AssemblyNeumann(Solver, Model, StiffMatrix, ForceVector, &
                TempPerm, Temperature, &
                TransientSimulation, Time, dt)

            USE Types
            TYPE(Matrix_t), POINTER :: StiffMatrix
            TYPE(Model_t)  :: Model
            TYPE(Solver_t), TARGET :: Solver
            REAL(KIND=dp) :: Time, dt
            REAL(KIND=dp), POINTER :: Temperature(:), ForceVector(:)
            LOGICAL :: TransientSimulation
            INTEGER, POINTER :: TempPerm(:)

        END SUBROUTINE AssemblyNeumann

        FUNCTION ComputeMeshSize( Solver, Model ) RESULT(MeshSize)
            USE Types
            TYPE(Solver_t) :: Solver
            TYPE(Model_t) :: Model
            REAL(KIND=dp) :: MeshSize
        END FUNCTION ComputeMeshSize
!------------------------------------------------------------------------------     
    END INTERFACE       
!------------------------------------------------------------------------------     
    CALL Info( 'NumaHeatSolve', ' ',Level=4 )
    CALL Info( 'NumaHeatSolve', '-------------------------------------',Level=4 )
    CALL Info( 'NumaHeatSolve', 'TEMPERATURE SOLVER:  ', Level=4 )
    CALL Info( 'NumaHeatSolve', '-------------------------------------',Level=4 )
    CALL Info( 'NumaHeatSolve', ' ',Level=4 )   
!------------------------------------------------------------------------------
!  Get variables needed for solution
!------------------------------------------------------------------------------
    IF ( .NOT. ASSOCIATED( Solver % Matrix ) ) RETURN

    StiffMatrix => Solver % Matrix
    ForceVector => Solver % Matrix % RHS

    TempSol => Solver % Variable
    TempPerm    => TempSol % Perm
    Temperature => TempSol % Values
    
!------------------------------------------------------------------------------     
    LocalNodes = COUNT( TempPerm > 0 )
    IF ( LocalNodes <= 0 ) RETURN

    PRINT*, 'There are',LocalNodes,' nodes in the part of mesh treated by proc ',ParEnv % MyPe

    SolverParams => GetSolverParams()

!------------------------------------------------------------------------------
!  Allocate some permanent storage, this is done first time only
!------------------------------------------------------------------------------
    IF ( .NOT. AllocationsDone .OR. Solver % Mesh % Changed ) THEN
        CALL Allocations
!------------------------------------------------------------------------------
    END IF ! not allocations done
!------------------------------------------------------------------------------
!  Do some additional initialization, and go for it
!------------------------------------------------------------------------------
    dt = Timestep   
!------------------------------------------------------------------------------
    SaveRelax = Relax
    CumulativeTime = 0.0d0
!------------------------------------------------------------------------------
    FirstTime = .TRUE.
!------------------------------------------------------------------------------     
    DO WHILE( CumulativeTime < Timestep-1.0d-12 .OR. .NOT. TransientSimulation )
    !------------------------------------------------------------------------------
    !  The first time around this has been done by the caller
    !------------------------------------------------------------------------------
        IF ( TransientSimulation .AND. .NOT.FirstTime ) THEN
            CALL InitializeTimestep(Solver)
        END IF
        FirstTime = .FALSE.
        !------------------------------------------------------------------------------   
        totat = 0.0d0
        totst = 0.0d0
        !------------------------------------------------------------------------------ 
        !  Get current (physical) time
        !------------------------------------------------------------------------------
        TimeVar => VariableGet( CurrentModel % Solver % Mesh % Variables, 'Time' )
        Time = TimeVar % Values(1)  
        !------------------------------------------------------------------------------         
        !   Save total power of the previous time in a file 
        !   Control that this is a new global time iteration, i.e. not only the nonlinear and 
        !  coupled system iterations have changed
        !  If parralel, only mester proc writes, after getting contribution from other procs
        !------------------------------------------------------------------------------         
        ! Get contribution from the other procs for parallel execution
        !------------------------------------------------------------------------------
        IF (ParEnv % PEs > 1) THEN
            !------------------------------------------------------------------------------
            tmpPower = TotalPower
            CALL MPI_ALLREDUCE( tmpPower, TotalPower, 1, MPI_DOUBLE_PRECISION, &
                MPI_SUM, MPI_COMM_WORLD, ierr )
            !------------------------------------------------------------------------------
        END IF
        !------------------------------------------------------------------------------
        ! Update the variable PreviousPower
        !------------------------------------------------------------------------------
        IF(TemperatureControlledPower) THEN
        !------------------------------------------------------------------------------
            PreviousPower = CurrentPower
        !------------------------------------------------------------------------------
        END IF !TemperatureControlledPower
        !------------------------------------------------------------------------------
        IF ( (ControlTotalPower) .AND. (PrevTime < Time) .AND. (ParEnv % MyPe==0) ) THEN
        !------------------------------------------------------------------------------
            TotalPowerFilename = 'totalpower'//'_'//TRIM(TestName)//'.csv'  
            !------------------------------------------------------------------------------
            ! Modify the file
            !------------------------------------------------------------------------------         
            OPEN(UNIT=1,FILE=TotalPowerFilename, FORM='FORMATTED', ACTION='WRITE', POSITION='APPEND', &
                IOSTAT=ios)
            WRITE(UNIT=1,FMT='(F13.4)', ADVANCE='no') PrevTime
            WRITE(UNIT=1,FMT='(A)', ADVANCE='no') ','
            WRITE(UNIT=1,FMT='(F16.4)', ADVANCE='no') TotalPower
            WRITE(UNIT=1,FMT='(A)', ADVANCE='no') ','
            WRITE(UNIT=1,FMT='(F16.4)', ADVANCE='yes') TotalPower
            CLOSE(1)
        !------------------------------------------------------------------------------
        END IF
        !------------------------------------------------------------------------------         
        !   Save number of coupled system iterations and max number of non-linear iterations 
        !  done in the time step in a file  
        !   Control that this is a new global time iteration, i.e. not only the nonlinear and 
        !  coupled system iterations have changed
        !------------------------------------------------------------------------------             
        IF ( (ControlIterations) .AND. (PrevTime < Time) ) THEN
            IF(ParEnv % PEs>1) THEN
                WRITE(char_MyPe,*) ParEnv % MyPe
                char_MyPe = ADJUSTL(char_MyPe)
                iterationsFilename = 'iterations'//'_'//TRIM(TestName)//'_'//TRIM(char_MyPe)//'.dat'        
            ELSE 
                iterationsFilename = 'iterations'//'_'//TRIM(TestName)//'.dat'          
            END IF
            OPEN(UNIT=1,FILE=iterationsFilename, FORM='FORMATTED', ACTION='WRITE', POSITION='APPEND', &
                IOSTAT=ios)
            WRITE(UNIT=1,FMT=*) PrevTime, MaxNonLinearIterDone, CouplingIter
            CLOSE(1)
        END IF
        !------------------------------------------------------------------------------ 
        ! Compute the norm of the solution
        !------------------------------------------------------------------------------         
        Norm = SQRT( SUM( Temperature**2 ) / LocalNodes )
        !------------------------------------------------------------------------------
        !  Non linear iterations
        !------------------------------------------------------------------------------
        ConvergenceDone = .FALSE.
        !------------------------------------------------------------------------------
        DO iter=1,NonlinearIter
            at  = CPUTime()
            at0 = RealTime()
            arealt = RealTime()

            CALL Info( 'NumaHeatSolve', ' ', Level=4 )
            CALL Info( 'NumaHeatSolve', '-------------------------------------',Level=4 )
            WRITE( Message,* ) 'TEMPERATURE non-linear iteration', iter
            CALL Info( 'NumaHeatSolve', Message, Level=4 )
            CALL Info( 'NumaHeatSolve', '-------------------------------------',Level=4 )
            CALL Info( 'NumaHeatSolve', ' ', Level=4 )
            CALL Info( 'NumaHeatSolve', 'Starting Assembly...', Level=4 )           
            !------------------------------------------------------------------------------
            ! Assume we are doing no reassembly!!
            IF (FirstTime) THEN
                CALL DefaultInitialize()
            END IF
            !------------------------------------------------------------------------------
            body_id = -1
            Integ_Force = 0.0
            TotalPower = 0.0
            !------------------------------------------------------------------------------
            !  Go through bulk elements
            !------------------------------------------------------------------------------

            CALL BulkAssembly(Solver, Model, TempSol, StiffMatrix, ForceVector, TransientSimulation, dt, &
                TotalPower, Integ_Force, &
                TempPerm, Temperature, PreviousPower, CurrentPower, &
                IntegErrorToTargetTemperature, DerivErrorToTargetTemperature, TargetTemperature, Time)

            PRINT *,'TotalPower=',TotalPower
            PRINT *,'Integ_Force=',Integ_Force 
            !PRINT *,'Reassembled bulk elements changed= ', mass_changed, "=", &
            !    100.0 * mass_changed / Solver % NumberOfActiveElements, '% (MASS) ', &
            !    stiff_changed, "=", 100.0 * stiff_changed / Solver % NumberOfActiveElements, ' % (STIFF)'
            !------------------------------------------------------------------------------

            CALL AssemblyNeumann(Solver, Model, StiffMatrix, ForceVector, TempPerm, Temperature, TransientSimulation, Time, dt)
            !------------------------------------------------------------------------------
            !  Dirichlet boundary conditions
            !------------------------------------------------------------------------------
            CALL NumaDefaultDirichletConditions(Solver)
            !------------------------------------------------------------------------------
            CALL Info( 'NumaHeatSolve', 'Assembly done', Level=4 )
            !------------------------------------------------------------------------------
            ! Compute assembly CPU time, save current CPU time for solving CPU time below, 
            ! and save current solution norm
            !------------------------------------------------------------------------------
            at = CPUTime() - at
            arealt = RealTime() -arealt
            st = CPUTime()
            srealt = RealTime()
            PrevNorm = Norm    
            !------------------------------------------------------------------------------
            !     Solve the system 
            !------------------------------------------------------------------------------
            CALL SolveSystem( StiffMatrix, ParMatrix, ForceVector, &
                Temperature, Solver % Variable % Norm, 1, Solver )
            PRINT *, "MAX/MIN : ", MAXVAL(Temperature), MINVAL(Temperature)
            !------------------------------------------------------------------------------
            !   Enforce the computation of the norm
            !   (This computation is processor-local in parallel execution)
            !------------------------------------------------------------------------------
            Solver % Variable % Norm = SQRT( SUM( Temperature**2 ) / (LocalNodes) )
            Norm = Solver % Variable % Norm
            !------------------------------------------------------------------------------
            CALL AdditionalComputations(Solver, Model, Temperature, TempPerm, IntegErrorToTargetTemperature, &
                DerivErrorToTargetTemperature, TargetTemperature, HeatSource, dt)
        !------------------------------------------------------------------------------      
        ! Write some information messages
        !------------------------------------------------------------------------------      
        !------------------------------------------------------------------------------
        ! Compute solving CPU time, and total assembly and solving CPU time in the 
        ! coupled system iteration (may contain several nonlinear iterations) 
        !------------------------------------------------------------------------------
        st = CPUTIme()-st
        srealt = RealTime()-srealt
        totat = totat + at
        totst = totst + st
        WRITE(Message,'(a,i4,a,F8.2,F8.2)') 'iter: ',iter,', Assembly CPU Time (nonlinear it., coupled it.): (s)', at, totat
        CALL Info( 'NumaHeatSolve', Message, Level=4 )
        WRITE(Message,'(a,i4,a,F8.2)') 'iter: ',iter,', Assembly Real Time (nonlinear it): (s)', arealt
        CALL Info( 'NumaHeatSolve', Message, Level=4 )
        WRITE(Message,'(a,i4,a,F8.2,F8.2)') 'iter: ',iter,', Solve CPU Time (nonlinear it., coupled it.):    (s)', st, totst
        CALL Info( 'NumaHeatSolve', Message, Level=4 )
        WRITE(Message,'(a,i4,a,F8.2)') 'iter: ',iter,', Solve Real Time (nonlinear it): (s)', srealt
        CALL Info( 'NumaHeatSolve', Message, Level=4 )
        !------------------------------------------------------------------------------
        ! Compute norm of the solution and check convergence criteria for nonlinear iterations
        !------------------------------------------------------------------------------
        IF ( PrevNorm + Norm /= 0.0d0 ) THEN
            RelativeChange = 2.0d0 * ABS( PrevNorm-Norm ) / (PrevNorm + Norm)
        ELSE
            RelativeChange = 0.0d0
        END IF

        WRITE( Message, * ) 'Result Norm   : ',Norm
        CALL Info( 'NumaHeatSolve', Message, Level=4 )
        WRITE( Message, * ) 'Relative Change : ',RelativeChange
        CALL Info( 'NumaHeatSolve', Message, Level=4 )
        !------------------------------------------------------------------------------
        ! If NonlinearTol is reached, stop the nonlinear iterations before maxiter: 
        ! In case of a parallel execution, communication with the other processus and
        ! stop of iterations only if for all processus convergence is reached
        !------------------------------------------------------------------------------
        ConvergenceDone = .FALSE.
        IF ( RelativeChange < NonlinearTol ) THEN
            ConvergenceDone = .TRUE.
        END IF
        !PRINT *,'proc= ',ParEnv % MyPe,'ConvergenceDone= ',ConvergenceDone
        IF(ParEnv % PEs>1) THEN
            CALL ParallelAllReduceAnd( ConvergenceDone )
            !PRINT *,'aftercomm, proc= ',ParEnv % MyPe,'ConvergenceDone= ',ConvergenceDone
        END IF
        IF( ConvergenceDone ) EXIT
        !------------------------------------------------------------------------------
        END DO ! of the nonlinear iteration
        !------------------------------------------------------------------------------ 
        ! Save the maximum number of non-linear iterations (for control)
        !------------------------------------------------------------------------------ 
        IF (ControlIterations) THEN
        
            IF (iter > NonLinearIter)  THEN
                CALL Info( 'NumaHeatSolve', 'Nonlinear Iterations done this timestep= ', Level=4 )
                PRINT *,iter-1
                IF (iter-1 > MaxNonLinearIterDone) THEN
                    MaxNonLinearIterDone = iter-1
                END IF
            ELSE 
                CALL Info( 'NumaHeatSolve', 'Nonlinear Iterations done this timestep= ', Level=4 )
                PRINT *,iter
                IF (iter > MaxNonLinearIterDone) THEN
                    MaxNonLinearIterDone = iter
                END IF
            END IF
            
            CALL Info( 'NumaHeatSolve', 'Max number of Nonlinear Iterations= ', Level=4 )
            PRINT *,MaxNonLinearIterDone
            
        END IF
        !------------------------------------------------------------------------------
        !  Check if convergence is reached towards specified tolerance >0
        !  If not, stop the simulation
        !------------------------------------------------------------------------------ 
        IF ( (NonlinearIterAbort) .AND. (NonlinearTol/=0) .AND. ( RelativeChange > NonlinearTol ) &
            ) THEN
            WRITE( Message, * ) 'No convergence reached during the ', NonlinearIter,' non-linear iterations '
            CALL Error( 'NumaHeatSolve', Message )
            CALL Fatal( 'NumaHeatSolve', ' ' )
        END IF
        !------------------------------------------------------------------------------
        !  Save the maximum number of coupling iterations (for control)
        !------------------------------------------------------------------------------ 
        IF (ControlIterations) THEN
            IF (PrevTime < Time) THEN 
                !------------------------------------------------------------------------------
                ! We are in the first coupled system sub-iteration:
                !------------------------------------------------------------------------------
                CouplingIter = 1
            ELSE
                CouplingIter = CouplingIter + 1
            END IF
            IF (CouplingIter > MaxCouplingIterDone) THEN
                MaxCouplingIterDone = CouplingIter
            END IF
            CALL Info( 'NumaHeatSolve', 'Coupling Iterations done this timestep= ', Level=4 )
            PRINT *, CouplingIter
            CALL Info( 'NumaHeatSolve', 'Max number of Coupling Iterations= ', Level=4 )
            PRINT *, MaxCouplingIterDone
        END IF
        !------------------------------------------------------------------------------         
        ! Save Time as Previous Time for next iteration
        !------------------------------------------------------------------------------
        PrevTime = TimeVar % Values(1)
        !------------------------------------------------------------------------------
        ! Compute cumulative time done by now and time remaining
        !------------------------------------------------------------------------------
        IF ( .NOT. TransientSimulation ) EXIT
        CumulativeTime = CumulativeTime + dt
        dt = Timestep - CumulativeTime
    !------------------------------------------------------------------------------   
    END DO ! time interval
   Solver % dt = Timestep
    !------------------------------------------------------------------------------
   CALL  ListAddConstReal( Solver % Values,  &
        'Nonlinear System Relaxation Factor', SaveRelax )
    !------------------------------------------------------------------------------
    ! Compute mesh average size
    !------------------------------------------------------------------------------        
   MeshSize = ComputeMeshSize(Solver, Model)
   !------------------------------------------------------------------------------
    ! Refine mesh if required
    !------------------------------------------------------------------------------ 
    IF ( ListGetLogical( Solver % Values, 'Adaptive Mesh Refinement', Found ) ) &
        CALL NumaRefineMesh( Model,Solver,Temperature,TempPerm, &
            TemperatureInsideResidual, TemperatureEdgeResidual, TemperatureBoundaryResidual, 1 )
!------------------------------------------------------------------------------
CONTAINS
    SUBROUTINE Allocations()
        IF ( AllocationsDone ) THEN
            DEALLOCATE( &
                IntegErrorToTargetTemperature, &
                PreviousPower,                          &
                DerivErrorToTargetTemperature, &
                CurrentPower, &
                HeatSource, &
            )
        END IF
        ALLOCATE( &
            IntegErrorToTargetTemperature(LocalNodes), &
            DerivErrorToTargetTemperature(LocalNodes), &
            PreviousPower(LocalNodes),          &
            HeatSource(LocalNodes), &
            CurrentPower(LocalNodes),         &
        )

        TemperatureControlledPower = GetLogical( Model % Simulation, &
            'Temperature Controlled Electric Power',Found )
        IF ( .NOT.Found ) TemperatureControlledPower = .FALSE.

        IF (TemperatureControlledPower) THEN
            !------------------------------------------------------------------------------ 
            !       Get the target temperature:
            !------------------------------------------------------------------------------     
            TargetTemperature = GetConstReal( Model % Simulation, &
                'Target Temperature For Electric Power Control', Found )
            IF ( .NOT.Found ) TargetTemperature = 373.15
        END IF

        !------------------------------------------------------------------------------
        !       If specified in the input file, compute the maximum number of non-linear 
        !       iterations and coupling iterations between solvers done during the whole 
        !       simulation
        !------------------------------------------------------------------------------ 
        ControlIterations = GetLogical( SolverParams,'Control Iterations',Found )       
        IF ( .NOT.Found ) ControlIterations = .FALSE.
        IF (ControlIterations) THEN
        !------------------------------------------------------------------------------     
        !           Maximum number of non-linear iterations done during the whole simulation:
        !------------------------------------------------------------------------------
        MaxNonLinearIterDone = 1
        !------------------------------------------------------------------------------ 
        !       Maximum number of coupling iterations between solvers done during the 
        !           whole simulation:
        !------------------------------------------------------------------------------             
        MaxCouplingIterDone = 0 
        CouplingIter = 0
        !------------------------------------------------------------------------------ 
        !       Header of iterations file
        !------------------------------------------------------------------------------             
        IF(ParEnv % PEs>1) THEN
            WRITE(char_MyPe,*) ParEnv % MyPe
            char_MyPe = ADJUSTL(char_MyPe)
            iterationsFilename = 'iterations'//'_'//TRIM(TestName)//'_'//TRIM(char_MyPe)//'.dat'        
        ELSE 
            iterationsFilename = 'iterations'//'_'//TRIM(TestName)//'.dat'          
        END IF          
        OPEN(UNIT=1,FILE=iterationsFilename, FORM='FORMATTED', ACTION='WRITE', POSITION='APPEND', &
            IOSTAT=ios)
        WRITE(UNIT=1,FMT=*) 'Time    ', '    MaxNonlinearIterDone    ', '   CouplingIter'
        CLOSE(1)
        !------------------------------------------------------------------------------ 
        END IF ! ControlIterations
        !
        !------------------------------------------------------------------------------ 
        !   Maximum number of nonlinear iterations
        !------------------------------------------------------------------------------ 
        NonlinearIter = GetInteger( SolverParams, &
            'Nonlinear System Max Iterations', Found )
        IF ( .NOT.Found ) NonlinearIter = 1
        !------------------------------------------------------------------------------ 
        !   Tolerance to be reached during nonlinear iterations
        !------------------------------------------------------------------------------ 
        NonlinearTol = GetConstReal( SolverParams, &
            'Nonlinear System Convergence Tolerance', Found )
        IF ( .NOT.Found ) NonlinearTol = 3.0
        !------------------------------------------------------------------------------ 
        !   Stop criteria if NonlinearTol not reached during NonlinearIter
        !------------------------------------------------------------------------------     
        NonlinearIterAbort = GetLogical( SolverParams, &
            'Nonlinear System Abort Not Converged',Found )
        IF ( .NOT.Found ) NonlinearIterAbort = .TRUE.
        !------------------------------------------------------------------------------ 
        !   Relaxation factor for convergence of the nonlinear iterations
        !------------------------------------------------------------------------------ 
        Relax = GetCReal( SolverParams, &
            'Nonlinear System Relaxation Factor',Found )
        IF ( .NOT.Found ) Relax = 1

        !------------------------------------------------------------------------------ 
        !       If saving the total electric power in a control file, write header:
        !------------------------------------------------------------------------------ 
        TotalPower = 0.0
        ControlTotalPower = GetLogical( SolverParams,'Control Total Power',Found )      
        IF ( .NOT.Found ) ControlTotalPower = .TRUE.
        IF ((ControlTotalPower) .AND. (ParEnv % MyPe==0)) THEN  
            TotalPowerFilename = 'totalpower'//'_'//TRIM(TestName)//'.csv'          
            OPEN(UNIT=1,FILE=TotalPowerFilename)
            WRITE(UNIT=1,FMT=*) 'Time,', 'Blood-Total-Power,', 'Tissue-Total-Power'
            CLOSE(1)
        END IF ! ControlTotalPower      

        AllocationsDone = .TRUE.
    END SUBROUTINE Allocations

!------------------------------------------------------------------------------
END SUBROUTINE NumaHeatSolver
!------------------------------------------------------------------------------
