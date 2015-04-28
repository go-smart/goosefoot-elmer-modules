! *****************************************************************************/
! *  The original file is relicensed, as permitted by the authors, as GPLv3
! *  Original file in Elmer source tree: fem/src/modules/HeatSolve.F90
! *****************************************************************************/
!/*****************************************************************************/
! *
! *  Elmer, A Finite Element Software for Multiphysical Problems
! *
! *  Copyright 1st April 1995 - , CSC - IT Center for Science Ltd., Finland
! *
! *  (NUMA: altered from original GPLv2-or-later)
! *  This program is free software: you can redistribute it and/or modify
! *  it under the terms of the GNU General Public License as published by
! *  the Free Software Foundation, either version 3 of the License, or
! *  (at your option) any later version.
! *
! *  This program is distributed in the hope that it will be useful,
! *  but WITHOUT ANY WARRANTY; without even the implied warranty of
! *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
! *  GNU General Public License for more details.
! *
! *  You should have received a copy of the GNU General Public License
! *  along with this program. If not, see <http://www.gnu.org/licenses/>.
! *****************************************************************************/
!
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
!/******************************************************************************
! *
! *  Module containing a solver for heat equation
! *
! ******************************************************************************
! *
! *  Authors: Juha Ruokolainen
! *  Email:   Juha.Ruokolainen@csc.fi
! *  Web:     http://www.csc.fi/elmer
! *  Address: CSC - IT Center for Science Ltd.
! *           Keilaranta 14
! *           02101 Espoo, Finland 
! *
! *  Original Date: 08 Jun 1997
! *
! *****************************************************************************/

!------------------------------------------------------------------------------
!> Subroutine for solving the energy a.k.a. heat equation in various coordinate systems.
!> \ingroup Solvers
!------------------------------------------------------------------------------
MODULE HeatSolve
   USE Differentials
   USE Radiation
   USE MaterialModels
   USE Adaptive
   USE DefUtils

   USE HeatAssemblyBulk
   USE HeatPhaseDefs
   USE HeatAddGlobalTime
   USE HeatAddHeatFluxBC
   USE HeatAddHeatGap
   USE HeatAssemblyBulk
   USE HeatAssemblyNeumann
   USE HeatBoundaryResidual
   USE HeatCheckLatentHeat
   USE HeatDiffuseGrayRadiation
   USE HeatEdgeResidual
   USE HeatEffectiveHeatCapacity
   USE HeatFindGapIndexes
   USE HeatInsideResidual
   USE HeatIntegOverA
   USE HeatSolveSystem
   USE HeatIncorporateCells

  CONTAINS
   SUBROUTINE SolverMain( Model,Solver,Timestep,TransientSimulation )
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
     IMPLICIT NONE
!------------------------------------------------------------------------------
     TYPE(Model_t) :: Model
     TYPE(Solver_t) :: Solver
     REAL(KIND=dp) :: Timestep
     LOGICAL :: TransientSimulation

     TYPE(Variable_t), POINTER :: TempSol, FlowSol, DensitySol
     TYPE(ValueList_t), POINTER :: BodyForce, Constants, Material, SolverParams
     TYPE(Matrix_t), POINTER :: StiffMatrix
     CHARACTER(LEN=MAX_NAME_LEN) :: ConvectionField
     INTEGER, POINTER :: TempPerm(:), FlowPerm(:)
     REAL(KIND=dp), POINTER :: Temperature(:), FlowSolution(:), PrevSolution(:), ForceVector(:), &
         ParPtr(:)
     REAL(KIND=dp), ALLOCATABLE :: HeaterSource(:), HeaterArea(:), &
         HeaterDensity(:), HeaterTarget(:), HeaterScaling(:), &
         XX(:), YY(:), ForceHeater(:), Density(:), LOAD(:)
     INTEGER :: bf_id, CheckConvergence, i, iter, LocalNodes, n, NewtonIter, NonlinearIter, &
         SmartHeaterBC, NSDOFs, SmartHeaterNode, istat, t, panchl
     LOGICAL :: AllocationsDone = .FALSE., PhaseChange, CheckLatentHeatRelease, ConstantBulk, &
         Converged, FirstTime, Found, HeaterControlLocal, IntegralHeaterControl, IsRadiation, &
         NewtonLinearization, SaveBulk, SmartHeaterControl, TransientHeaterControl, SmartTolReached, &
         SmartHeaterAverage, TransientAssembly, GotMeltPoint, CellsDeath, VolumetricHeatSource
     REAL(KIND=dp) :: CumulativeTime, dt, MeltPoint, NewtonTol, Norm, PrevNorm, PowerRelax, &
         PowerScaling, PrevPowerScaling, PowerTimeScale, PowerSensitivity, &
         RelativeChange, Relax, s, SaveRelax, SmartTol, &
         yave, NonlinearTol, dt0
     LOGICAL, ALLOCATABLE :: SmartHeaters(:), IntegralHeaters(:)
     TYPE(Element_t), POINTER :: Element

     SAVE PowerTimeScale, AllocationsDone, Density, HeaterSource, HeaterArea, &
         HeaterDensity, HeaterTarget, HeaterScaling, &
         SmartHeaters, IntegralHeaters, &
         XX, YY, ForceHeater, CellsDeath, &
         LocalNodes, SmartTolReached, PrevPowerScaling, &
         PowerScaling, GotMeltPoint, MeltPoint, SmartHeaterNode, SmartHeaterBC, SmartHeaterAverage, &
         SmartTol, NonlinearTol, NonlinearIter, Relax

     REAL(KIND=dp) :: at,at0,totat,st,totst,CPUTime,RealTime

!------------------------------------------------------------------------------
!    The View and Gebhardt factors may change. If this is necessary, this is 
!    done within this subroutine. The routine is called in the
!    start as it may affect the matrix topology.
!    Newton lineariarization option is needed only when there is radiation.
!------------------------------------------------------------------------------
     IsRadiation = ListCheckPresentAnyBC( Model,'Radiation')
     IF( IsRadiation ) THEN
       CALL RadiationFactors( Solver, .FALSE.)
     END IF

!------------------------------------------------------------------------------
!    Get variables needed for solution
!------------------------------------------------------------------------------

     IF ( .NOT. ASSOCIATED( Solver % Matrix ) ) RETURN

     StiffMatrix => GetMatrix()
     ForceVector => Solver % Matrix % RHS

     TempSol => Solver % Variable
     TempPerm    => TempSol % Perm
     Temperature => TempSol % Values
  
     LocalNodes = COUNT( TempPerm > 0 )
     IF ( LocalNodes <= 0 ) RETURN

     SolverParams => GetSolverParams()
     ConvectionField = GetString( SolverParams, 'Temperature Convection Field', Found )

     Relax = GetCReal( SolverParams, &
               'Nonlinear System Relaxation Factor', Found)
     IF ( .NOT.Found ) Relax = 1

     IF ( Found ) THEN
       FlowSol => VariableGet( Solver % Mesh % Variables, ConvectionField )
     ELSE
       FlowSol => VariableGet( Solver % Mesh % Variables, 'Flow Solution' )
     END IF

     IF ( ASSOCIATED( FlowSol ) ) THEN
       FlowPerm     => FlowSol % Perm
       NSDOFs       =  FlowSol % DOFs
       FlowSolution => FlowSol % Values
     END IF

     DensitySol => VariableGet( Solver % Mesh % Variables, 'Density' )

     ! Check whether we have some heater controls. This will affect initialization stuff. 
     SmartHeaterControl = ListCheckPresentAnyBodyForce( Model,'Smart Heater Control')
     IntegralHeaterControl = ListCheckPresentAnyBodyForce( Model,'Integral Heat Source')
   
!------------------------------------------------------------------------------
!    Allocate some permanent storage, this is done first time only
!------------------------------------------------------------------------------
     IF ( .NOT. AllocationsDone .OR. Solver % Mesh % Changed ) THEN
         CALL Allocations
    END IF

<<<<<<< Updated upstream
    IF ( CellsDeath ) THEN
        CALL CellSetup(Solver, Model, SolverParams, Timestep, Temperature, TempPerm)
    END IF
=======
    CALL CellSetup(Solver, Model, SolverParams, Timestep, Temperature, TempPerm)
>>>>>>> Stashed changes

    IF( IsRadiation ) THEN
      NewtonTol     = GetConstReal( SolverParams, &
                     'Nonlinear System Newton After Tolerance',  Found)
      NewtonIter    = GetInteger(   SolverParams, &
                     'Nonlinear System Newton After Iterations', Found)
    ELSE
      NewtonTol = 1.0_dp
      NewtonIter =  0
    END IF
    IF ( NewtonIter == 0) NewtonLinearization = .TRUE.

!------------------------------------------------------------------------------
!    Do some additional initialization, and go for it
!------------------------------------------------------------------------------
     dt = Timestep
     Constants => GetConstants()
     ConstantBulk = GetLogical( SolverParams, 'Constant Bulk System', Found )
     SaveBulk = ConstantBulk .OR. GetLogical( SolverParams, 'Save Bulk System', Found )
     SaveBulk = ConstantBulk .OR. GetLogical( SolverParams, 'Calculate Loads', Found )

     NonlinearIter = GetInteger(   SolverParams, &
                     'Nonlinear System Max Iterations', Found)
     IF ( .NOT.Found ) NonlinearIter = 1

     NonlinearTol  = GetConstReal( SolverParams, &
                     'Nonlinear System Convergence Tolerance',    Found)

     Relax = GetCReal( SolverParams, &
               'Nonlinear System Relaxation Factor', Found)
     IF ( .NOT.Found ) Relax = 1

     dt0 = ListGetConstReal(SolverParams,'Steady State Transition Timestep',Found)
     IF(.NOT. Found) dt0 = ListGetConstReal(SolverParams,'Smart Heater Time Scale',Found)

     TransientAssembly = TransientSimulation
     IF(Found .AND. dt > dt0) TransientAssembly = .FALSE.

     SaveRelax = Relax
     CumulativeTime = 0.0d0

!------------------------------------------------------------------------------
     FirstTime = .TRUE.
     ALLOCATE(PrevSolution(LocalNodes))

     DO WHILE( CumulativeTime < Timestep-1.0d-12 .OR. .NOT. TransientSimulation )
!------------------------------------------------------------------------------
!    The first time around this has been done by the caller...
!------------------------------------------------------------------------------
     IF ( TransientSimulation .AND. .NOT.FirstTime ) &
       CALL InitializeTimestep(Solver)
     FirstTime = .FALSE.
!------------------------------------------------------------------------------
!    Save current solution
!------------------------------------------------------------------------------
     PrevSolution = Temperature(1:LocalNodes)
!------------------------------------------------------------------------------

     totat = 0.0d0
     totst = 0.0d0

     Norm = Solver % Variable % Norm

     DO iter=1,NonlinearIter
       at  = CPUTime()
       at0 = RealTime()

       CALL Info( 'HeatSolve', ' ', Level=4 )
       CALL Info( 'HeatSolve', ' ', Level=4 )
       CALL Info( 'HeatSolve', '-------------------------------------',Level=4 )
       WRITE( Message,* ) 'TEMPERATURE ITERATION', iter
       CALL Info( 'HeatSolve', Message, Level=4 )
       CALL Info( 'HeatSolve', '-------------------------------------',Level=4 )
       CALL Info( 'HeatSolve', ' ', Level=4 )
       CALL Info( 'HeatSolve', 'Starting Assembly...', Level=4 )

500    IF ( ConstantBulk .AND. ASSOCIATED(Solver % Matrix % BulkValues) ) THEN
         Solver % Matrix % Values = Solver % Matrix % BulkValues
         Solver % Matrix % RHS = Solver % Matrix % BulkRHS
         GOTO 1000
       END IF

!------------------------------------------------------------------------------
       CALL DefaultInitialize()
!------------------------------------------------------------------------------
 
       IF ( SmartHeaterControl .OR. IntegralHeaterControl ) THEN
          IF( SmartHeaterControl) ForceHeater = 0.0d0
          HeaterArea = 0.0d0
          HeaterSource = 0.0d0
          HeaterScaling = 1.0d0
          HeaterDensity = 0.0d0
          HeaterTarget = 0.0d0
          HeaterControlLocal = .FALSE.

          DO t=1,Solver % NumberOfActiveElements             
             Element => GetActiveElement(t)             
             BodyForce => GetBodyForce()
             
             IF ( .NOT. ASSOCIATED( BodyForce ) ) CYCLE
             bf_id = GetBodyForceId()
             
             IF( .NOT. (SmartHeaters(bf_id) .OR. IntegralHeaters(bf_id) ) ) CYCLE

             n = GetElementNOFNodes()

             Material => GetMaterial()

             ParPtr => GetReal( Material, 'Density' )
             Density = ParPtr
             ParPtr => GetReal( BodyForce, 'Heat Source' )
             LOAD = ParPtr
             VolumetricHeatSource = GetLogical(BodyForce, 'Volumetric Heat Source', Found)
             IF (.NOT. Found .OR. .NOT. VolumetricHeatSource) THEN
                 Density = 1.0_dp
             END IF

             s = ElementArea( Solver % Mesh, Element, n )

             IF( CurrentCoordinateSystem() == AxisSymmetric .OR. &
                  CurrentCoordinateSystem() == CylindricSymmetric ) s = 2 * PI * s

             HeaterSource(bf_id) = HeaterSource(bf_id) + s * SUM(Density(1:n) * LOAD(1:n)) / n
             HeaterArea(bf_id) = HeaterArea(bf_id) + s
             HeaterDensity(bf_id) = HeaterDensity(bf_id) + s * SUM( Density(1:n) ) / n
          END DO

          DO i = 1,Model % NumberOfBodyForces
             IF( IntegralHeaters(i) .OR. SmartHeaters(i) ) THEN
                HeaterDensity(i) = HeaterDensity(i) / HeaterArea(i)
             END IF
             IF(IntegralHeaters(i)) THEN
                HeaterTarget(i) = GetCReal(  Model % BodyForces(i) % Values, &
                     'Integral Heat Source', Found )
                HeaterScaling(i) = HeaterTarget(i) / HeaterSource(i)
             END IF
          END DO
       END IF

!------------------------------------------------------------------------------
       NULLIFY(Material)
!------------------------------------------------------------------------------
!      Bulk elements
!------------------------------------------------------------------------------
       CALL StartAdvanceOutput( 'HeatSolve', 'Assembly:' )

       IF (BulkAssembly(Solver, Model, TempSol, FlowSol, DensitySol, dt, &
          PrevPowerScaling, &
          ConstantBulk, TransientSimulation, TransientAssembly, PhaseChange, &
          ForceHeater, TransientHeaterControl, SmartTolReached, &
          HeaterControlLocal, CheckLatentHeatRelease, &
          IsRadiation, NewtonLinearization, SmartHeaterControl, IntegralHeaterControl, &
          HeaterScaling, SmartHeaters, IntegralHeaters, CellsDeath)) THEN
         RETURN
       END IF


!------------------------------------------------------------------------------

1000  CONTINUE

!------------------------------------------------------------------------------
!     Neumann & Newton boundary conditions
!------------------------------------------------------------------------------
      CALL NeumannAssembly(Solver, Model, ForceVector, ForceHeater, &
          NewtonLinearization, TransientHeaterControl, &
          IsRadiation, TempSol, TransientAssembly, HeaterControlLocal, &
          ConstantBulk, dt)
      !------------------------------------------------------------------------------
      !  Dirichlet boundary conditions
      !------------------------------------------------------------------------------
!------------------------------------------------------------------------------

      IF ( TransientSimulation .AND. ConstantBulk ) CALL AddGlobalTime(Solver, dt)

      CALL DefaultFinishAssembly()
      CALL Info( 'HeatSolve', 'Assembly done', Level=4 )

      CALL DefaultDirichletBCs()

!------------------------------------------------------------------------------
!     Solve the system and check for convergence
!------------------------------------------------------------------------------
      at = CPUTime() - at
      st = CPUTime()

      PrevNorm = Norm

      CheckConvergence = SolveHeatSystem(Solver, Model, Norm, TempSol, SmartHeaterNode, XX, YY, HeaterScaling, dt, &
            ForceHeater, PowerScaling, PowerTimeScale, NewtonLinearization, MeltPoint, IntegralHeaterControl, Relax, &
            SmartHeaterControl, SmartHeaterBC, SmartHeaterAverage, SmartTolReached, TransientHeaterControl, &
            SmartHeaters, IntegralHeaters, HeaterArea, HeaterSource, HeaterDensity, yave)

      IF (CheckConvergence == 1) THEN
          GOTO 500
      ELSE IF (CheckConvergence == 2) THEN
          EXIT
      END IF

      IF (CellsDeath) THEN
          CALL GetCellFinalize(Temperature, dt)
      END IF
      !CALL AdditionalComputations(Solver, Model, Temperature, TempPerm, &
      !    DerivErrorToTargetTemperature, TargetTemperature, HeatSource, dt)

      st = CPUTIme()-st
      totat = totat + at
      totst = totst + st
      WRITE(Message,'(a,i4,a,F8.2,F8.2)') 'iter: ',iter,' Assembly: (s)', at, totat
      WRITE(Message,*) 'Minimum Temperature',MINVAL(Temperature),'Maximum Temperature ',MAXVAL(Temperature)
      CALL Info( 'HeatSolve', Message, Level=4 )
      WRITE(Message,'(a,i4,a,F8.2,F8.2)') 'iter: ',iter,' Solve:    (s)', st, totst
      CALL Info( 'HeatSolve', Message, Level=4 )
      
!!      DO t=1, Solver % Mesh % NumberOfBoundaryElements
!!        Element => GetBoundaryElement(t)
!!        IF ( .NOT. ActiveBoundaryElement() ) CYCLE
!!        n = GetElementNOFNodes()
!!        st = 0
!!        DO i=1,n
!!          panchl = TempPerm(Element % NodeIndexes(i))
!!          st = st + Temperature(panchl)
!!        END DO
!!        PRINT *, st
!!      END DO
!------------------------------------------------------------------------------
!     If modelling phase change (and if requested by the user), check if any
!     node has jumped over the phase change interval, and if so, reduce
!     timestep and or relaxation and recompute.
!------------------------------------------------------------------------------
      IF (PhaseChange .AND. CheckLatentHeatRelease ) THEN
!------------------------------------------------------------------------------
        IF ( CheckLatentHeat(Solver, Model, TempPerm, Temperature, PrevSolution) ) THEN
          Temperature(1:LocalNodes) = PrevSolution
          Norm = PrevNorm

          IF ( TransientSimulation ) THEN
            dt = dt / 2
            Solver % dt = dt
            WRITE( Message, * ) &
                  'Latent heat release check: reducing timestep to: ',dt
            CALL Info( 'HeatSolve', Message, Level=4 )
          ELSE
            Relax = Relax / 2
            CALL  ListAddConstReal( Solver % Values,  &
                 'Nonlinear System Relaxation Factor', Relax )
            WRITE( Message, * ) &
                 'Latent heat release check: reducing relaxation to: ',Relax
            CALL Info( 'HeatSolve', Message, Level=4 )
          END IF

          CYCLE
        END IF
        IF ( .NOT.TransientSimulation ) PrevSolution=Temperature(1:LocalNodes)
      END IF
!------------------------------------------------------------------------------
     
      RelativeChange = Solver % Variable % NonlinChange

      WRITE( Message, * ) 'Result Norm   : ',Norm
      CALL Info( 'HeatSolve', Message, Level=4 )
      WRITE( Message, * ) 'Relative Change : ',RelativeChange
      CALL Info( 'HeatSolve', Message, Level=4 )

      IF ( RelativeChange < NewtonTol .OR. iter >= NewtonIter ) &
               NewtonLinearization = .TRUE.
      Converged =  ( Solver % Variable % NonlinConverged == 1 ) .AND. &
          ( .NOT. SmartHeaterControl .OR. SmartTolReached )
      IF( Converged ) EXIT

      IF(SmartHeaterControl) THEN
        IF ( RelativeChange < SmartTol ) THEN
          SmartTolReached = .TRUE.
          YY = Temperature
        END IF
      END IF
      
!------------------------------------------------------------------------------
    END DO ! of the nonlinear iteration
!------------------------------------------------------------------------------
    IF(TransientHeaterControl) THEN
      PowerRelax = GetCReal(Solver % Values,'Smart Heater Relaxation Factor', Found)
      IF(.NOT. Found) PowerRelax = 1.0_dp
      PowerSensitivity = ListGetConstReal(Solver % Values,'Smart Heater Power Sensivity',Found)
      IF(.NOT. Found) PowerSensitivity = 4.0_dp
      PowerScaling = PowerScaling * (1 + PowerSensitivity * PowerRelax * (MeltPoint/yave - 1.0d0) ) 

      IF( ListGetLogical( Solver % Values,'Smart Heater Transient Speedup',Found ) ) THEN
        Temperature = Temperature * (1 + PowerRelax * (MeltPoint/yave - 1.0d0)   )     
      END IF
      YY = Temperature
    END IF

!------------------------------------------------------------------------------
!   Compute cumulative time done by now and time remaining
!------------------------------------------------------------------------------
    IF ( .NOT. TransientSimulation ) EXIT
    CumulativeTime = CumulativeTime + dt
    dt = Timestep - CumulativeTime

   END DO ! time interval
   Solver % dt = Timestep

!------------------------------------------------------------------------------
   CALL  ListAddConstReal( Solver % Values,  &
        'Nonlinear System Relaxation Factor', SaveRelax )
!------------------------------------------------------------------------------

   DEALLOCATE( PrevSolution )

   IF ( ListGetLogical( Solver % Values, 'Adaptive Mesh Refinement', Found ) ) &
      CALL RefineMesh( Model,Solver,Temperature,TempPerm, &
            InsideResidual, EdgeResidual, BoundaryResidual )

CONTAINS

  SUBROUTINE Allocations()
      INTEGER :: j, k, l, DoneTime=-1
      REAL(KIND=dp) :: ControlPoint(3), dist, jx, jy, jz, mindist
      REAL(KIND=dp), POINTER :: RealWork(:,:)

      N = Solver % Mesh % MaxElementDOFs

      IF ( AllocationsDone ) THEN
         DEALLOCATE(  &
             Density, &
             LOAD &
         )
      END IF

      ALLOCATE( &
                Density( N ),               &
                LOAD( N ), &
                STAT=istat )

      IF ( istat /= 0 ) THEN
        CALL Fatal( 'HeatSolve', 'Memory allocation error' )
      END IF

      !-------------------------------------------------------------------------- 
      !  Check if Cell Death Modelling specified in input file 
      !-------------------------------------------------------------------------- 
      CellsDeath = GetLogical( SolverParams,'Cell Death Modelling',Found )
      IF ( .NOT.Found ) CellsDeath = .TRUE. !RMV this should be changed before releasing


      IF ( SmartHeaterControl .OR. IntegralHeaterControl) THEN          
         n = Model % NumberOfBodyForces
         IF ( AllocationsDone ) DEALLOCATE( HeaterArea, HeaterDensity, HeaterSource, &
              HeaterScaling, HeaterTarget, SmartHeaters, IntegralHeaters )
         ALLOCATE( HeaterArea(n), HeaterDensity(n), HeaterSource(n), &
              HeaterScaling(n), HeaterTarget(n), SmartHeaters(n), &
              IntegralHeaters(n) )
         IF ( istat /= 0 ) THEN
            CALL Fatal( 'HeatSolve', 'Memory allocation error' )
         END IF
         SmartHeaters = .FALSE.
         IntegralHeaters = .FALSE.
      END IF

      PowerTimeScale = ListGetConstReal(Solver % Values, &
           'Smart Heater Time Scale',Found)
     
      TransientHeaterControl = .FALSE.
      IF( SmartHeaterControl ) THEN
         IF ( AllocationsDone ) DEALLOCATE( XX, YY, ForceHeater  )
         n = SIZE( Temperature )
         ALLOCATE( XX( n ), YY(n), ForceHeater( n ), STAT=istat )
         IF ( istat /= 0 ) THEN
            CALL Fatal( 'HeatSolve', 'Memory allocation error' )
         END IF
         XX = 0.0d0 
         YY = 0.0d0
         ForceHeater = 0.0d0

         IF(TransientSimulation .AND. dt < PowerTimeScale) THEN
            TransientHeaterControl = .TRUE.
            CALL Info( 'HeatSolve', 'Using Transient Heater Control')
         ELSE
            TransientHeaterControl = .FALSE.
            CALL Info( 'HeatSolve', 'Using Steady-state Heater Control')
         END IF
      END IF

      SmartTol  = GetConstReal( SolverParams, &
           'Smart Heater Control After Tolerance',  Found )
      IF(.NOT. Found) THEN
        SmartTolReached = .TRUE.
        SmartTol = 1.0
      END IF

      IF(SmartHeaterControl) THEN

        ! Mark the smart heaters
        SmartHeaters = .FALSE.
        bf_id = 0
        DO i = 1,Model % NumberOfBodyForces
          IF( ListGetLogical( Model % BodyForces(i) % Values, &
              'Smart Heater Control', Found ) ) THEN
            SmartHeaters(i) = .TRUE.
            bf_id = i
          END IF
        END DO

        ! Find the BC that controls the heater
        ! If not found assume that smart heater is related to phase change
        MeltPoint = GetCReal( Model % BodyForces(bf_id) % Values,&
            'Smart Heater Temperature', GotMeltPoint)

        SmartHeaterAverage = .FALSE.
        SmartHeaterNode = ListGetInteger( Model % BodyForces(bf_id) % Values,&
            'Smart Heater Control Node', Found)
        IF(.NOT. Found) THEN
          RealWork => ListGetConstRealArray(Model % BodyForces(bf_id) % Values,&
              'Smart Heater Control Point', Found)
          IF( Found ) THEN
            ControlPoint(1:3) = RealWork(1:3,1)

            mindist = HUGE( mindist )
            DO l=1,Model % NumberOfNodes
              IF( TempPerm(l) == 0 ) CYCLE

              jx = Model % Mesh % Nodes % x(l)
              jy = Model % Mesh % Nodes % y(l)
              jz = Model % Mesh % Nodes % z(l)

              dist = (ControlPoint(1)-jx)**2 + (ControlPoint(2)-jy)**2 + (ControlPoint(3)-jz)**2
              IF( dist < mindist ) THEN
                mindist = dist
                SmartHeaterNode = l
              END IF
            END DO
          END IF

          WRITE(Message,*) 'Found Control Point at distance:',SQRT(mindist)
          CALL Info('HeatSolve',Message)
          WRITE(Message,*) 'Control Point Index:',SmartHeaterNode
          CALL Info('HeatSolve',Message)
        END IF

        IF( .NOT. GotMeltPoint .OR. SmartHeaterNode == 0) THEN
          Found = .FALSE.
          SmartHeaterBC = 0

          DO i=1,Model % NumberOfBCs
            Found = ListGetLogical( Model % BCs(i) % Values,'Smart Heater Boundary', Found )
            IF(Found) THEN
              SmartHeaterBC = i
              EXIT
            END IF
          END DO
          IF(.NOT. Found) THEN
            DO i=1,Model % NumberOfBCs
              Found = ListGetLogical( Model % BCs(i) % Values,'Phase Change', Found )
              IF(Found) THEN
                SmartHeaterBC = i
                EXIT
              END IF
            END DO
          END IF
          IF(SmartHeaterBC == 0) THEN
            CALL Fatal('HeatSolve','Smart Heater Boundary / Phase Change is undefined')
          END IF

          MeltPoint = GetCReal( Model % BCs(SmartHeaterBC) % Values,&
              'Smart Heater Temperature',Found)
          IF(.NOT. Found) THEN
            DO k=1, Model % NumberOfMaterials
              MeltPoint = GetCReal( Model % Materials(k) % Values, &
                  'Melting Point', Found )
              IF(Found) EXIT
            END DO
            IF(.NOT. Found) THEN
              CALL Fatal('HeatSolver','Smart Heater Temperature / Melting Point is undefined')
            END IF
          END IF

          ! Find the node related to temperature control
          SmartHeaterAverage = ListGetLogical(Solver % Values,'Smart Heater Average', Found)
          IF(.NOT. SmartHeaterAverage) THEN
            jx = -HUGE(jx)
            DO k = Model % Mesh % NumberOfBulkElements + 1, &
                Model % Mesh % NumberOfBulkElements + Model % Mesh % NumberOfBoundaryElements

              Element => Model % Mesh % Elements(k)

              IF ( Element % BoundaryInfo % Constraint == SmartHeaterBC ) THEN
                DO l=1,Element % TYPE % NumberOfNodes
                  IF ( Model % Mesh % Nodes % x(Element % NodeIndexes(l)) >= jx ) THEN
                    j = Element % NodeIndexes(l)
                    jx = Model % Mesh % Nodes % x(Element % NodeIndexes(l))
                  END IF
                END DO
              END IF
            END DO
            SmartHeaterNode = j
          END IF
        END IF

         IF(Solver % DoneTime /= DoneTime) THEN
            PrevPowerScaling = PowerScaling
            DoneTime = Solver % DoneTime
         END IF
      END IF

      IF( IntegralHeaterControl) THEN
         CALL Info( 'HeatSolve', 'Using Integral Heater Control')
         IntegralHeaters = .FALSE.
         DO i = 1,Model % NumberOfBodyForces
            IntegralHeaters(i) = ListCheckPresent( Model % BodyForces(i) % Values, &
                 'Integral Heat Source')
         END DO
      END IF


      
      AllocationsDone = .TRUE.
    END SUBROUTINE Allocations

!------------------------------------------------------------------------------
  END SUBROUTINE SolverMain
END MODULE HeatSolve
