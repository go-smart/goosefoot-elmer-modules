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
MODULE HeatAssemblyBulk
  USE DiffuseConvective
  USE DiffuseConvectiveGeneral
  USE HeatEffectiveHeatCapacity
  USE HeatIncorporateCells
 CONTAINS
  FUNCTION BulkAssembly(Solver, Model, TempSol, FlowSol, DensitySol, dt, &
          PrevPowerScaling, &
          ConstantBulk, TransientSimulation, TransientAssembly, PhaseChange, &
          ForceHeater, TransientHeaterControl, SmartTolReached, &
          HeaterControlLocal, CheckLatentHeatRelease, &
          IsRadiation, NewtonLinearization, SmartHeaterControl, IntegralHeaterControl, &
          HeaterScaling, SmartHeaters, IntegralHeaters, CellsDeath) &
          RESULT(ShouldExit)

      USE MaterialModels
      USE DefUtils
      USE NumaAdaptive
      IMPLICIT NONE

      TYPE(Solver_t), TARGET :: Solver
      TYPE(Model_t)  :: Model
      TYPE(Variable_t), POINTER :: TempSol, FlowSol, DensitySol, LoadVar
      REAL(KIND=dp) :: dt, &
          PrevPowerScaling
      LOGICAL :: ConstantBulk, TransientSimulation, TransientAssembly, &
          PhaseChange, CheckLatentHeatRelease, HeaterControlLocal, &
          TransientHeaterControl, IsRadiation, NewtonLinearization, SmartTolReached, &
          IntegralHeaterControl, SmartHeaterControl, CellsDeath
      LOGICAL, ALLOCATABLE :: SmartHeaters(:), IntegralHeaters(:)
      REAL(KIND=dp), ALLOCATABLE :: ForceHeater(:), HeaterScaling(:)
      LOGICAL :: ShouldExit

      TYPE(Nodes_t)   :: ElementNodes
      TYPE(Element_t), POINTER :: Element
      TYPE(ValueList_t), POINTER :: BodyForce, Equation, Material, SolverParams
      LOGICAL :: &
          Found, &
          VisualizePerfusion = .FALSE., &
          PhaseSpatial=.FALSE., VolumetricHeatSource = .FALSE.
          !InterpolatedElectricPower = .FALSE., UseNormedJH = .FALSE.
      INTEGER :: bf_id, n, i, j, k, t, LocalNodes, body_id, &
          NSDOFs, CompressibilityModel
      INTEGER, POINTER :: FlowPerm(:), TempPerm(:)
          !JouleHeatingPerm(:)
      REAL(KIND=dp) :: at, at0, &
          ReferencePressure = -1.0d0, SpecificHeatRatio

      TYPE(Variable_t), POINTER :: PerfusionVar
      REAL(KIND=dp), POINTER :: Hwrk(:,:,:), Temperature(:), ParPtr(:), &
          PrevTemperature(:), FlowSolution(:), Perfusion(:)
      REAL(KIND=dp), ALLOCATABLE :: &
          HeatExpansionCoeff(:), ReferenceTemperature(:), HeatCapacity(:), &
          PerfusionRefTemperature(:), PerfusionRate(:), &
          PerfusionHeatCapacity(:), PerfusionDensity(:)
          !, JouleHeating(:)
      REAL(KIND=dp), ALLOCATABLE :: ats(:), LOAD(:), &
          U(:), V(:), W(:), MU(:,:), C1(:), &
          Enthalpy(:), PressureCoeff(:), &
          zv(:), MASS(:,:), STIFF(:,:), PowerForce(:), FORCE(:), &
          PerfusionCoeff(:), HeatCapacityConvect(:), Power(:), HeatSource(:), Density(:), &
          C0(:), OldHeatCapacity(:), GasConstant(:), OldPerfusionCoeff(:), &
          Pressure(:), &
          LocalTemperature(:), dPressureDt(:), Viscosity(:)
      CHARACTER(LEN=MAX_NAME_LEN) :: PhaseModel, StabilizeFlag, CompressibilityFlag, ConvectionFlag, &
          ConvectionField

      LOGICAL :: Stabilize = .FALSE., AllocationsDone = .FALSE., &
          Bubbles = .TRUE., UseBubbles = .FALSE.
          !FirstTime = .TRUE.

      REAL(KIND=dp) :: &
          CPUTime,RealTime

      REAL(KIND=dp), ALLOCATABLE ::  &
          HeatConductivity(:,:,:), &
          !InterMaterialCoeff(:), &
          !VolumeFraction(:), &
          TimeForce(:), &
          PowerProfileVector(:)

      SAVE LOAD, ElementNodes, HeatConductivity, HeatCapacity, OldHeatCapacity, Density, zv, &
          U, V, W, MU, PerfusionRate, PerfusionHeatCapacity, HeatSource, &
          GasConstant, AllocationsDone, LocalNodes, C0, Hwrk, MASS, STIFF, PowerForce, FORCE, &
          HeatCapacityConvect, Perfusion, &
          Power, &
          TimeForce, OldPerfusionCoeff, PerfusionCoeff, Stabilize, &
          UseBubbles, &
          !FirstTime, &
          PowerProfileVector, Enthalpy, &
          VisualizePerfusion, ats, &
          Pressure, PressureCoeff, ReferenceTemperature, HeatExpansionCoeff, &
          LocalTemperature, dPressureDt, Viscosity, &
          PerfusionRefTemperature, PerfusionDensity, &
          C1, &
          SolverParams

      ShouldExit = .FALSE.

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

      TempPerm    => TempSol % Perm
      Temperature => TempSol % Values
      LocalNodes = COUNT( TempPerm > 0 )
      body_id = -1

      IF ( .NOT. AllocationsDone .OR. Solver % Mesh % Changed ) THEN
          CALL Allocations()
      END IF
      !STIFF = 0
      !MASS = 0
      !FORCE =0

      ConvectionField = GetString( SolverParams, 'Temperature Convection Field', Found )
      PerfusionVar => VariableGet(Solver % Mesh % Variables, 'Perfusion')

      IF ( TransientSimulation ) THEN
        PrevTemperature => Solver % Variable % PrevValues(:,1)
      END IF

      IF ( ASSOCIATED( FlowSol ) ) THEN
        FlowPerm     => FlowSol % Perm
        NSDOFs       =  FlowSol % DOFs
        FlowSolution => FlowSol % Values
      END IF

      at  = CPUTime()
      at0 = RealTime()
      ats = 0.0
      zv = 0.0

      N = Solver % Mesh % MaxElementNodes
      !IF (ActiveCells /= Solver % NumberOfActiveElements) THEN
      !    RecalculateElectricTips = .TRUE.

      !    IF (ActiveCells > 0) THEN
      !        DEALLOCATE(HeatSourceCache)
      !    END IF

      !    ALLOCATE(HeatSourceCache(Solver % NumberOfActiveElements, N))

      !    ActiveCells = Solver % NumberOfActiveElements
      !    PRINT *, 'B', ActiveCells
      !ELSE
      !    RecalculateElectricTips = CheckTipsUpdate(Model, Time)
      !    PRINT *, 'A'
      !END IF

      !ReassemblePower = FirstTime .OR. RecalculateElectricTips

      !IF (ReassemblePower) THEN
      !    PRINT *, "Reassembling Power"
      !    PowerProfileVector = 0
      !END IF

      LoadVar => VariableGet(Solver % Mesh % Variables, 'Deposition')
      LoadVar % Values = 0._dp

      DO t=1,Solver % NumberOfActiveElements
      FORCE =0
      STIFF =0

        CALL AdvanceOutput(t,GetNOFActive())
  !------------------------------------------------------------------------------
  !     Check if this element belongs to a body where temperature
  !     should be calculated
  !------------------------------------------------------------------------------
        Element => GetActiveElement(t)

  !------------------------------------------------------------------------------
        IF ( Element % BodyId /= body_id ) THEN
  !------------------------------------------------------------------------------
          Equation => GetEquation()
          ConvectionFlag = GetString( Equation, 'Convection', Found )

          Material => GetMaterial()
  !------------------------------------------------------------------------------
          CompressibilityFlag = GetString( Material, &
                'Compressibility Model', Found)
          IF ( .NOT.Found ) CompressibilityModel = Incompressible

          SELECT CASE( CompressibilityFlag )

            CASE( 'incompressible' )
              CompressibilityModel = Incompressible

            CASE( 'user defined' )
              CompressibilityModel = UserDefined1

            CASE( 'perfect gas', 'perfect gas equation 1' )
              CompressibilityModel = PerfectGas1

            CASE( 'thermal' )
              CompressibilityModel = Thermal

            CASE DEFAULT
              CompressibilityModel = Incompressible
          END SELECT
  !------------------------------------------------------------------------------

          PhaseModel = GetString( Equation, 'Phase Change Model',Found )
          IF(.NOT. Found) PhaseModel = GetString( Material, 'Phase Change Model',Found )

          PhaseChange = Found .AND. (PhaseModel(1:4) /= 'none')
          IF ( PhaseChange ) THEN
             CheckLatentHeatRelease = GetLogical( Equation, &
                  'Check Latent Heat Release',Found )
          END IF
        END IF
  !------------------------------------------------------------------------------

        n = GetElementNOFNodes()
        CALL GetElementNodes( ElementNodes )

        CALL GetScalarLocalSolution( LocalTemperature )
  !------------------------------------------------------------------------------
  !     Get element material parameters
  !------------------------------------------------------------------------------
        ParPtr => GetReal( Material, 'Heat Capacity', Found )
        HeatCapacity = ParPtr

        CALL ListGetRealArray( Material,'Heat Conductivity',Hwrk,n, &
                     Element % NodeIndexes )
        HeatConductivity = 0.0d0
        IF ( SIZE(Hwrk,1) == 1 ) THEN
          DO i=1,3
            HeatConductivity( i,i,1:n ) = Hwrk( 1,1,1:n )
          END DO
        ELSE IF ( SIZE(Hwrk,2) == 1 ) THEN
          DO i=1,MIN(3,SIZE(Hwrk,1))
            HeatConductivity(i,i,1:n) = Hwrk(i,1,1:n)
          END DO
        ELSE
          DO i=1,MIN(3,SIZE(Hwrk,1))
            DO j=1,MIN(3,SIZE(Hwrk,2))
              HeatConductivity( i,j,1:n ) = Hwrk(i,j,1:n)
            END DO
          END DO
        END IF
  !------------------------------------------------------------------------------

        IF ( CompressibilityModel == PerfectGas1 ) THEN

          ! Read Specific Heat Ratio:
          !--------------------------
          SpecificHeatRatio = GetConstReal( Material, &
              'Specific Heat Ratio', Found )
          IF ( .NOT.Found ) SpecificHeatRatio = 5.d0/3.d0

          ! For an ideal gas, \gamma, c_p and R are really a constant
          ! GasConstant is an array only since HeatCapacity formally is:
          !-------------------------------------------------------------
          GasConstant(1:n) = ( SpecificHeatRatio - 1.d0 ) * &
              HeatCapacity(1:n) / SpecificHeatRatio

          ParPtr => GetReal( Material, 'Pressure Coefficient', Found )
          IF ( Found ) THEN
              PressureCoeff(1:n) = ParPtr(1:n)
          ELSE
              PressureCoeff(1:n) = 1.0_dp
          END IF
        ELSE IF ( CompressibilityModel == Thermal ) THEN
          ParPtr => GetReal( Material, 'Reference Temperature' )
          ReferenceTemperature = ParPtr
          ParPtr => GetReal( Material, 'Heat Expansion Coefficient' )
          HeatExpansionCoeff = ParPtr

          ParPtr => GetReal( Material,'Density' )
          Density(1:n) = ParPtr(1:n) * ( 1 - HeatExpansionCoeff(1:n)  * &
               ( LocalTemperature(1:n) - ReferenceTemperature(1:n) ) )

          ParPtr => GetReal( Material, 'Pressure Coefficient', Found )
          IF ( Found ) THEN
            PressureCoeff(1:n) = ParPtr(1:n)
          ELSE
            PressureCoeff(1:n) = LocalTemperature(1:n) * HeatExpansionCoeff(1:n) / &
                  ( 1-HeatExpansionCoeff(1:n)*( &
                              LocalTemperature(1:n)-ReferenceTemperature(1:n)) )
          END IF
        ELSE IF ( CompressibilityModel == UserDefined1 ) THEN
          IF ( ASSOCIATED( DensitySol ) ) THEN
            CALL GetScalarLocalSolution( Density, 'Density' )
          ELSE
            ParPtr => GetReal( Material,'Density' )
            Density = ParPtr
          END IF
          ParPtr => GetReal( Material, 'Pressure Coefficient', Found )
          IF ( Found ) THEN
              PressureCoeff = ParPtr
          ELSE
              PressureCoeff(1:n) = 0.0_dp
          END IF
        ELSE
          ParPtr => GetReal( Material, 'Pressure Coefficient', Found )
          IF ( Found ) THEN
              PressureCoeff = ParPtr
          ELSE
              PressureCoeff(1:n) = 0.0_dp
          END IF
          ParPtr => GetReal( Material, 'Density' )
          Density = ParPtr
        END IF

  !------------------------------------------------------------------------------
  ! Take pressure deviation p_d as the dependent variable, p = p_0 + p_d
  ! for PerfectGas, read p_0
  !------------------------------------------------------------------------------
        IF ( CompressibilityModel /= Incompressible ) THEN
          ReferencePressure = ListGetConstReal( Material, &
              'Reference Pressure', Found)
          IF ( .NOT.Found ) ReferencePressure = 0.0d0
        END IF
  !------------------------------------------------------------------------------

        HeaterControlLocal = .FALSE.
        Load = 0.0D0
        Pressure = 0.0d0
        dPressuredt = 0.0d0
  !------------------------------------------------------------------------------
  !     Check for convection model
  !------------------------------------------------------------------------------
        C1 = 1.0D0
        U = 0._dp
        V = 0._dp
        W = 0._dp

        MU = 0.0d0
        CALL GetVectorLocalSolution( MU, 'Mesh Velocity' )

        IF ( ConvectionFlag == 'constant' ) THEN

          ParPtr => GetReal( Material, 'Convection Velocity 1', Found )
          IF ( .NOT. Found ) &
             ParPtr => GetReal( Equation, 'Convection Velocity 1', Found )
          U = ParPtr
          ParPtr => GetReal( Material, 'Convection Velocity 2', Found )
          IF ( .NOT. Found ) &
             ParPtr => GetReal( Equation, 'Convection Velocity 2', Found )
          V = ParPtr
          ParPtr => GetReal( Material, 'Convection Velocity 3', Found )
          IF ( .NOT. Found ) &
             ParPtr => GetReal( Equation, 'Convection Velocity 3', Found )
          W = ParPtr
        ELSE IF ( ConvectionFlag == 'computed' .AND. &
             ASSOCIATED(FlowSolution) ) THEN
          DO i=1,n
            k = FlowPerm(Element % NodeIndexes(i))
            IF ( k > 0 ) THEN
  !------------------------------------------------------------------------------
              Pressure(i) = FlowSolution(NSDOFs*k) + ReferencePressure
              SELECT CASE( CompressibilityModel )
                CASE( PerfectGas1 )
                  Density(i)  = Pressure(i) / &
                      ( GasConstant(i) * LocalTemperature(i) )
              END SELECT
              IF ( TransientSimulation ) THEN
                dPressureDt(i) = ( FlowSolution(NSDOFs*k) - &
                    FlowSol % PrevValues(NSDOFs*k,1) ) / dt
              END IF
  !------------------------------------------------------------------------------

              SELECT CASE( NSDOFs )
              CASE(3)
                U(i) = FlowSolution( NSDOFs*k-2 )
                V(i) = FlowSolution( NSDOFs*k-1 )
                W(i) = 0.0D0

              CASE(4)
                U(i) = FlowSolution( NSDOFs*k-3 )
                V(i) = FlowSolution( NSDOFs*k-2 )
                W(i) = FlowSolution( NSDOFs*k-1 )
              END SELECT
            ELSE
              U(i) = 0.0d0
              V(i) = 0.0d0
              W(i) = 0.0d0
            END IF
          END DO
        ELSE IF (ConvectionFlag=='computed' ) THEN
          CALL Warn( 'HeatSolver', 'Convection model specified ' //  &
                'but no associated flow field present?' )
        ELSE
          IF ( ALL(MU==0) ) C1 = 0.0D0
        END IF
  !------------------------------------------------------------------------------
  !     Check if modelling Phase Change with Eulerian approach
  !------------------------------------------------------------------------------
        PhaseSpatial = .FALSE.
        IF (  PhaseChange ) THEN
          CALL EffectiveHeatCapacity(n, Temperature, PrevTemperature, TempPerm, Element, LocalTemperature, &
              HeatCapacity, Density, Enthalpy, &
              Material, PhaseModel, PhaseSpatial, TransientSimulation, Solver % Mesh % Changed)
        ELSE
          HeatCapacity(1:n) = Density(1:n) * HeatCapacity(1:n)
        END IF

        IF (CellsDeath) THEN
            CALL GetCellHeat(C1, U, V, W, HeatCapacity, Element, Material, n)
        END IF

        Viscosity = 0.0d0
  !------------------------------------------------------------------------------
  !     Add body forces, if any
  !------------------------------------------------------------------------------
        BodyForce => GetBodyForce()
        Power = 0.0_dp
        IF ( ASSOCIATED( BodyForce ) ) THEN
          bf_id = GetBodyForceId()
  !------------------------------------------------------------------------------
  !       Frictional viscous heating
  !------------------------------------------------------------------------------
          IF ( GetLogical( BodyForce, 'Friction Heat',Found) ) THEN
             ParPtr => GetReal( Material,'Viscosity' )
             Viscosity = ParPtr
          END IF

  !------------------------------------------------------------------------------
  !       Given heat source
  !------------------------------------------------------------------------------ 
          VolumetricHeatSource = GetLogical(BodyForce, 'Volumetric Heat Source', FOUND)
          IF (.NOT. Found) VolumetricHeatSource = .FALSE.

          ParPtr => GetReal( BodyForce, 'Heat Source', Found )
          HeatSource = ParPtr

          Load(1:n) = 0
          IF (CellsDeath) THEN
              CALL GetCellLoadAdjust(Load, Density, HeatSource, Element, &
                  Power, TempSol % Values(:), TempPerm, n, VolumetricHeatSource)
          ELSE IF (VolumetricHeatSource) THEN
              Load(1:n) = HeatSource(1:n)
          ELSE
              Load(1:n) = Density(1:n) * HeatSource(1:n)
          END IF

          IF ( SmartHeaterControl .AND. NewtonLinearization .AND. SmartTolReached) THEN
             IF(  SmartHeaters(bf_id) ) THEN
              HeaterControlLocal = .TRUE.
              IF( TransientHeaterControl ) THEN
                Load(1:n) = PrevPowerScaling * Load(1:n)
                HeaterScaling(bf_id) = PrevPowerScaling
              END IF
            END IF
          END IF

          IF ( IntegralHeaterControl ) THEN
             IF( IntegralHeaters(bf_id) ) THEN
                Load(1:n) = Load(1:n) * HeaterScaling(bf_id)
             END IF
          END IF
        END IF
        DO i=1,n
          k = LoadVar % Perm(Element % NodeIndexes(i))
          IF ( k > 0 ) THEN
!---------------------------------------------------------------------------
            LoadVar % Values(k) = Load(i)
          END IF
        END DO

        C0 = 0.0_dp
        !------------------------------------------------------------------------------
        ! Note at this point HeatCapacity = \rho * c_p OR \rho * (c_p - R)
        ! and C1 = 0 (diffusion) or 1 (convection)
        !------------------------------------------------------------------------------

        !------------------------------------------------------------------------------
        !       Perfusion (added as suggested by Matthias Zenker)
        !------------------------------------------------------------------------------
        IF (CellsDeath) THEN
            CALL GetCellPerfusionRate(Element, BodyForce, PerfusionRate, n)
            Found = .TRUE.
        ELSE
            ParPtr => GetReal( BodyForce, 'Perfusion Rate', Found )
            PerfusionRate = ParPtr
        END IF

        IF ( Found ) THEN
          ParPtr => GetReal( BodyForce, 'Perfusion Reference Temperature' )
          PerfusionRefTemperature = ParPtr
          ParPtr => GetReal( BodyForce, 'Perfusion Density' )
          PerfusionDensity = ParPtr
          ParPtr => GetReal( BodyForce, 'Perfusion Heat Capacity' )
          PerfusionHeatCapacity = ParPtr
          C0(1:n) = PerfusionHeatCapacity(1:n) * PerfusionRate(1:n) * PerfusionDensity(1:n)
          Load(1:n) = Load(1:n) + C0(1:n) * PerfusionRefTemperature(1:n)

          IF (VisualizePerfusion) THEN
              DO i = 1, n
                k = TempPerm(Element % NodeIndexes(i))
                IF (k > 0) THEN
                    PerfusionVar % Values(k) = C0(i)
                END IF
              END DO
          END IF
        END IF

  !------------------------------------------------------------------------------
  !     Get element local matrices, and RHS vectors
  !------------------------------------------------------------------------------
        IF ( CurrentCoordinateSystem() == Cartesian ) THEN
  !------------------------------------------------------------------------------
          CALL DiffuseConvectiveCompose( &
              MASS, STIFF, FORCE, LOAD, &
              HeatCapacity, C0, C1*HeatCapacity(1:n), HeatConductivity, &
              PhaseSpatial, LocalTemperature, Enthalpy, U, V, W, &
              MU(1,1:n),MU(2,1:n),MU(3,1:n), Viscosity, Density, Pressure, &
              dPressureDt, PressureCoeff, CompressibilityModel /= Incompressible, &
              Stabilize, UseBubbles, Element, n, ElementNodes )

  !------------------------------------------------------------------------------
        ELSE
  !------------------------------------------------------------------------------
          CALL DiffuseConvectiveGenCompose( &
              MASS, STIFF, FORCE, LOAD, &
              HeatCapacity, C0, C1*HeatCapacity(1:n), HeatConductivity, &
              PhaseSpatial, LocalTemperature, Enthalpy, U, V, W, &
              MU(1,1:n),MU(2,1:n),MU(3,1:n), Viscosity, Density, Pressure, &
              dPressureDt, PressureCoeff, CompressibilityModel /= Incompressible, &
              Stabilize, Element, n, ElementNodes )
  !------------------------------------------------------------------------------
        END IF
  !------------------------------------------------------------------------------

        IF ( HeaterControlLocal .AND. .NOT. TransientHeaterControl) THEN

          IF ( TransientAssembly .AND. .NOT. ConstantBulk ) THEN
            CALL Default1stOrderTime( MASS, STIFF, FORCE )
          END IF

          CALL UpdateGlobalEquations( Solver % Matrix, STIFF, &
              ForceHeater, FORCE, n, 1, TempPerm(Element % NodeIndexes) )
        ELSE
           Bubbles = UseBubbles .AND. .NOT.Stabilize .AND. &
           ( ConvectionFlag == 'computed' .OR. ConvectionFlag == 'constant' )

  !------------------------------------------------------------------------------
  !        If time dependent simulation add mass matrix to stiff matrix
  !------------------------------------------------------------------------------
           TimeForce  = 0.0_dp
           IF ( TransientAssembly ) THEN
              IF ( ConstantBulk ) THEN
                CALL DefaultUpdateMass( MASS )
              ELSE
                CALL Default1stOrderTime( MASS,STIFF,FORCE )
              END IF
           ELSE IF ( Solver % NOFEigenValues>0 ) THEN
             CALL DefaultUpdateDamp(MASS)
           END IF
  !------------------------------------------------------------------------------
  !        Update global matrices from local matrices
  !------------------------------------------------------------------------------
           IF (  Bubbles ) THEN
              CALL Condensate( N, STIFF, FORCE, TimeForce )
           END IF

           CALL DefaultUpdateEquations( STIFF, FORCE )
        END IF
  !------------------------------------------------------------------------------
     END DO     !  Bulk elements
  !------------------------------------------------------------------------------

     PRINT *, MAXVAL(Solver % Matrix % Values), MAXVAL(Solver % Matrix %RHS)
     CALL DefaultFinishBulkAssembly()
     PRINT *, MAXVAL(Solver % Matrix % Values), MAXVAL(Solver % Matrix %RHS)

     CONTAINS
       SUBROUTINE Allocations()

           INTEGER :: N, istat
           REAL(KIND=dp) :: alloctime, allocrealtime
           !TYPE(Variable_t), POINTER :: JouleHeatingSol
           REAL(KIND=dp), POINTER :: VolF(:)
           !LOGICAL :: VarVolumeFraction
           TYPE(ValueList_t),POINTER :: SolverParams

           SAVE VolF!, VarVolumeFraction

           !------------------------------------------------------------------------------
           !   Save time for computation of allocation time:
           !------------------------------------------------------------------------------
           alloctime = CPUTime()
           allocrealtime = RealTime()
           SolverParams => GetSolverParams()

           N = Solver % Mesh % MaxElementNodes

           IF ( AllocationsDone ) THEN
               DEALLOCATE(                     &
                   U, V, W, MU,                      &
                   PerfusionRate, &
                   dPressureDt,           &
                   ElementNodes % x,             &
                   ElementNodes % y,             &
                   ElementNodes % z,             &
                   Density,                 &
                   Perfusion,               &
                   C0,                            &
                   HeatCapacity,                 &
                   OldHeatCapacity,                 &
                   HeatConductivity,             &
                   ReferenceTemperature,  &
                   LocalTemperature,      &
                   LOAD,                         &
                   Power,                                          &
                   MASS, STIFF,        &
                   !InterMaterialCoeff,           &
                   HeatCapacityConvect,          &
                   PressureCoeff,        &
                   Pressure, &
                   C1,                           &
                   zv, &
                   !VolumeFraction,              &
                   HeatExpansionCoeff, &
                   Viscosity, &
                   TimeForce,                                   &
                   PerfusionCoeff,                         &
                   PowerProfileVector, &
                   OldPerfusionCoeff,                         &
                   ats, &
                   GasConstant, &
                   PerfusionRefTemperature, &
                   PerfusionHeatCapacity, &
                   PerfusionDensity, &
                   VolF)
           END IF

           ALLOCATE(                                         &
               U( N ),   V( N ),         &
               W( N ), MU(3, N),                         &
               dPressuredt(N), &
               ElementNodes % x( N ),                &
               ElementNodes % y( N ),                &
               ElementNodes % z( N ),                &
               Density( N ),   &
               HeatSource( N ),   &
               C0(N),                                &
               Perfusion(LocalNodes),               &
               HeatCapacity( N ),              &
               LocalTemperature(N),      &
               OldHeatCapacity( N ),              &
               HeatConductivity( 3,3,N ),      &
               LOAD( N ),                      &
               Power( N ),                     &
               STIFF( 2*N,2*N ),    &
               MASS( 2*N,2*N ),     &
               FORCE( 2*N ),              &
               PowerFORCE( 2*N ),              &
               !InterMaterialCoeff( N ),        &
               C1(N), &
               zv(N), &
               ats(100),                          &
               HeatCapacityConvect( N ),       &
               HeatExpansionCoeff(N), &
               !VolumeFraction(N),              &
               GasConstant( N ),          &
               Viscosity(N), &
               PressureCoeff( N ),                   &
               Pressure(N), &
               TimeForce(2*N),                                         &
               PerfusionCoeff( N ),                        &
               OldPerfusionCoeff( N ),                        &
               PerfusionRefTemperature(N), &
               PerfusionRate(N), &
               PerfusionHeatCapacity(N), &
               PerfusionDensity(N), &
               ReferenceTemperature(N), &
               VolF(LocalNodes), &
               PowerProfileVector(LocalNodes), &
               STAT=istat                            )

           IF ( istat /= 0 ) THEN
               CALL Fatal( 'NumaHeatSolve', 'Memory allocation error' )
           END IF

           NULLIFY( Hwrk )
           AllocationsDone = .TRUE.

   !------------------------------------------------------------------------------
   !   If space dependent volume fraction specified in input file, add the
   !       volume fraction of blood as a new variable:
   !------------------------------------------------------------------------------
           VolF = 0.0D0
  !------------------------------------------------------------------------------
    Stabilize = GetLogical( SolverParams,'Stabilize', Found)

    UseBubbles = GetLogical( SolverParams,'Bubbles', Found)
    IF ( .NOT. Found ) UseBubbles = .TRUE.

    !------------------------------------------------------------------------------ 
    !   Add the heat source as a variable for visualization
    !------------------------------------------------------------------------------ 
    VisualizePerfusion = GetLogical( SolverParams,'Perfusion Visualization',Found )
    IF ( .NOT.Found ) VisualizePerfusion = .FALSE.
    !------------------------------------------------------------------------------
    IF(VisualizePerfusion) THEN
    !------------------------------------------------------------------------------
        CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
            Solver, 'Perfusion', 1, Perfusion, TempPerm ) 
    !------------------------------------------------------------------------------ 
    END IF !VisualizePerfusion

    StabilizeFlag = GetString( SolverParams, &
         'Stabilization Method', Found )

    SELECT CASE(StabilizeFlag)
    CASE('vms')
      Stabilize = .FALSE.
      UseBubbles= .FALSE.
    CASE('stabilized')
      Stabilize = .TRUE.
      UseBubbles = .FALSE.
    CASE('bubbles')
      Stabilize = .FALSE.
      UseBubbles = .TRUE.
    END SELECT

  !------------------------------------------------------------------------------

  !------------------------------------------------------------------------------
    END SUBROUTINE Allocations
  END FUNCTION BulkAssembly
END MODULE HeatAssemblyBulk
