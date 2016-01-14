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
MODULE HeatIncorporateCells
        USE DefUtils
        IMPLICIT NONE

        TYPE(Solver_t) :: Solver
        TYPE(Model_t) :: Model
        TYPE(ValueList_t), POINTER :: SolverParams
        TYPE(Nodes_t) :: ElementNodes
        REAL(KIND=dp), POINTER :: HistoryErrorToTargetTemperatureVariable(:), CellState(:), &
            HeatSource(:), ElectricPowerVisualization(:), ElectricPower(:)
        REAL(KIND=dp), ALLOCATABLE, TARGET :: HistoryErrorToTargetTemperature(:,:)
        REAL(KIND=dp), ALLOCATABLE :: Load(:), &
            IntegErrorToTargetTemperature(:), DerivErrorToTargetTemperature(:), &
            LocalHeatSource(:), CurrentPower(:), LocalCellState(:), &
            PreviousPower(:)
        TYPE(Variable_t), POINTER :: CellStateSol
        INTEGER, POINTER :: CellStatePerm(:), TempPerm(:)
        INTEGER :: LocalNodes, ADOFs, CellStateModel, PowerControl_Integ_Length
        REAL(KIND=dp) :: DeadThreshold, MaxTemperature, PowerControl_Kd, PowerControl_Ki, &
            PowerControl_Kp, Timestep, TotalPower, x, y, z, TargetTemperature, TempCutOff, &
            TempCutOffGradient, CutOffCoefficient
        LOGICAL :: AllocationsDone = .FALSE., ControlMaxTemperature, ControlTotalPower, ElectricPowerCutOff, &
            TemperatureControlledPower, UseElectricPower, InterpolatedElectricPower, UseTempCutOffGradient

        SAVE AllocationsDone, Load, HistoryErrorToTargetTemperature, IntegErrorToTargetTemperature, &
            DerivErrorToTargetTemperature, LocalHeatSource, HeatSource, CurrentPower, LocalCellState, &
            PreviousPower, ElectricPower, ControlMaxTemperature, CellStateModel, LocalNodes, ADOFs, &
            CellStateSol, CellState, ControlTotalPower, DeadThreshold, TempCutOff, TempCutOffGradient, &
            ElectricPowerCutOff, UseTempCutOffGradient, MaxTemperature, PowerControl_Integ_Length, &
            PowerControl_Kd, PowerControl_Ki, PowerControl_Kp, TargetTemperature, TemperatureControlledPower, &
            ElementNodes

        PRIVATE :: Solver, Model, SolverParams, ElementNodes, &
            HistoryErrorToTargetTemperatureVariable, CellState, &
            HeatSource, ElectricPowerVisualization, ElectricPower, &
            HistoryErrorToTargetTemperature, Load, &
            IntegErrorToTargetTemperature, DerivErrorToTargetTemperature, &
            LocalHeatSource, CurrentPower, LocalCellState, &
            PreviousPower, CellStateSol, CellStatePerm, TempPerm, &
            LocalNodes, ADOFs, CellStateModel, PowerControl_Integ_Length, &
            DeadThreshold, MaxTemperature, PowerControl_Kd, PowerControl_Ki, &
            PowerControl_Kp, Timestep, TotalPower, x, y, z, TargetTemperature, &
            AllocationsDone, ControlMaxTemperature, ControlTotalPower, ElectricPowerCutOff, &
            UseTempCutOffGradient, TempCutOffGradient, TempCutOff, &
            TemperatureControlledPower, UseElectricPower, InterpolatedElectricPower

      CONTAINS
          SUBROUTINE CellSetup(Solver, Model, SolverParams, Timestep, Temperature, TempPerm)
              TYPE(Solver_t) :: Solver
              TYPE(Model_t) :: Model
              TYPE(ValueList_t), POINTER :: SolverParams
              REAL(KIND=dp) :: Timestep, Temperature(:)
              INTEGER, POINTER :: TempPerm(:)

              IF (.NOT. AllocationsDone .OR. Solver % Mesh % Changed) THEN
                  CALL CellAllocations(Solver, Model, SolverParams, Timestep, Temperature, TempPerm)
              END IF
          END SUBROUTINE CellSetup

          SUBROUTINE GetCellLoadAdjust(Load, Density, HeatSource, Element, &
                  Power, Temperature, TempPerm, n, VolumetricHeatSource)
              TYPE(Element_t) :: Element
              REAL(KIND=dp) :: Load(:), Power(:), Density(:), HeatSource(:), &
                  Temperature(:)
              INTEGER, POINTER :: TempPerm(:)
              TYPE(ValueList_t), POINTER :: Material
              LOGICAL :: VolumetricHeatSource

              REAL(KIND=dp) :: coeff(n), Dead, DeadCutOff, DeadCutOffGradient, &
                  Temp, TempCutOff, TempCutOffGradient, MinimumDeposition
              LOGICAL :: UseDeadCutOffGradient, ElectricPowerCutOff, &
                  UseTempCutOffGradient, Found
              INTEGER :: i, k, n

              coeff = 1.0_dp
              IF (.NOT. VolumetricHeatSource) THEN
                  coeff = Density
              END IF

              ! This reduces deposition to dead-levels where the cell death exceeds a value
              Material => GetMaterial()
              DeadCutOff = GetConstReal(Material, "Coagulation Cut Off", ElectricPowerCutOff)
              CutOffCoefficient = 1.0_dp
              IF (ElectricPowerCutOff) THEN
                  DeadCutOffGradient = GetConstReal(Material, "Coagulation Gradient", UseDeadCutOffGradient)

                  CutOffCoefficient = 1.0_dp
                  Dead = MAXVAL(CellStateSol % PrevValues(2 * CellStatePerm(Element % NodeIndexes(1:n)), 1))
                  IF (Dead > DeadCutOff) THEN
                      IF (UseDeadCutOffGradient) THEN
                          CutOffCoefficient = MAX(0.0_dp, 1 - (Dead - DeadCutOff) * DeadCutOffGradient)
                      ELSE
                          CutOffCoefficient = 0.0_dp
                      END IF

                      MinimumDeposition = GetConstReal(Material, "Coagulation Minimum Deposition", Found)
                      IF (Found) THEN
                          CutOffCoefficient = MAX(CutOffCoefficient, MinimumDeposition)
                      END IF

                      coeff(1:n) = coeff(1:n) * CutOffCoefficient
                  END IF
              END IF

              ! This reduces deposition to dead-levels where the temperature exceeds a value
              TempCutOff = GetConstReal(Material, "Vapourization Cut Off", ElectricPowerCutOff)
              CutOffCoefficient = 1.0_dp
              IF (ElectricPowerCutOff) THEN
                  TempCutOffGradient = GetConstReal(Material, "Vapourization Gradient", UseTempCutOffGradient)

                  CutOffCoefficient = 1.0_dp
                  Temp = MAXVAL(Temperature(TempPerm(Element % NodeIndexes(1:n))))

                  IF (Temp > TempCutOff) THEN
                      IF (UseTempCutOffGradient) THEN
                          CutOffCoefficient = MAX(0.0_dp, 1 - (Temp - TempCutOff) * TempCutOffGradient)
                      ELSE
                          CutOffCoefficient = 0.0_dp
                      END IF

                      MinimumDeposition = GetConstReal(Material, "Vapourization Minimum Deposition", Found)
                      IF (Found) THEN
                          CutOffCoefficient = MAX(CutOffCoefficient, MinimumDeposition)
                      END IF

                      coeff(1:n) = coeff(1:n) * CutOffCoefficient
                  END IF
              END IF

              !------------------------------------------------------------------------------
              ! Read the body force value in the input file and modify Load
              !------------------------------------------------------------------------------
              IF (InterpolatedElectricPower) THEN
                  DO i = 1,n
                      k = CellStatePerm(Element % NodeIndexes(i))
                      Load(i) = Load(i) + coeff(i) * Power(i) * ElectricPower(k)
                  END DO
              ELSE IF (UseElectricPower) THEN
                  Load(1:n) = Load(1:n) + coeff(1:n) * Power(1:n) * HeatSource(1:n)
              ELSE
                  Load(1:n) = Load(1:n) + coeff(1:n) * HeatSource(1:n)
              END IF

              !DO i=1,n
              !  k = LoadVar % Perm(Element % NodeIndexes(i))
              !  IF ( k > 0 ) THEN
              !    LoadVar % Values(k) = Load(i)
              !  END IF
              !END DO
          END SUBROUTINE GetCellLoadAdjust

          SUBROUTINE GetCellPerfusionRate(Element, Material, PerfusionRate, n)
              TYPE(Element_t) :: Element
              REAL(KIND=dp) :: PerfusionRate(:)

              INTEGER :: i, k, n
              REAL(KIND=dp), POINTER :: ParPtr(:)
              REAL(KIND=dp) :: DeathPerfusionRate
              TYPE(ValueList_t), POINTER :: Material
              LOGICAL :: Found

              ! This is the normal perfusion rate
              ParPtr => GetReal( Material, 'Perfusion Rate', Found )
              PerfusionRate = ParPtr

              ! This perfusion rate applies when the cell-death threshold is crossed
              DeathPerfusionRate = GetConstReal( Material, &
                  'Death Perfusion Rate', Found )
              IF ( .NOT.Found ) THEN
                  DeathPerfusionRate = 0.0
              END IF

              IF ( ASSOCIATED( CellStateSol ) ) THEN
                  DO i=1,n
                      k = CellStatePerm(Element % NodeIndexes(i))
                      LocalCellState(i) = CellState((k-1)*ADOFs + 2)
                  END DO
              ELSE
                  LocalCellState = 0.0D0
              ENDIF
              !------------------------------------------------------------------------------
              DO i=1,n
                      IF (LocalCellState(i) > DeadThreshold) THEN
                          PerfusionRate = DeathPerfusionRate
                      END IF  
              END DO
          END SUBROUTINE GetCellPerfusionRate

          SUBROUTINE GetCellFinalize(Temperature, dt)
              REAL(KIND=dp) :: Temperature(:), dt
              TYPE(Element_t), POINTER :: Element
              TYPE(ValueList_t), POINTER :: BodyForce
              LOGICAL :: Found, VisualizeHeatSource
              INTEGER :: i, j, k, n, t

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
              !   Add the heat source as a variable for visualization
              !------------------------------------------------------------------------------ 
              VisualizeHeatSource = GetLogical( SolverParams,'Heat Source Visualization', Found)
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
                          n = GetElementNOFNodes()
                          CALL GetElementNodes( ElementNodes )    

                          BodyForce => GetBodyForce()
              !------------------------------------------------------------------------------
                          IF ( ASSOCIATED( BodyForce ) ) THEN
              !------------------------------------------------------------------------------ 
                              LocalHeatSource = 0.0D0
              !------------------------------------------------------------------------------
                                  !------------------------------------------------------------------------------
                                  ! Read the body force value in the input file: 
                                  !------------------------------------------------------------------------------
                                  LocalHeatSource(1:n) = LocalHeatSource(1:n) + &
                                          GetReal( BodyForce, 'Heat Source', Found )
              !------------------------------------------------------------------------------
                                  DO i=1,n
              !------------------------------------------------------------------------------
                                      k = TempPerm(Element % NodeIndexes(i))
                                      HeatSource(k) = LocalHeatSource(i)
              !------------------------------------------------------------------------------
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
              !------------------------------------------------------------------------------      
              ! Write some information messages
              !------------------------------------------------------------------------------      
              !------------------------------------------------------------------------------
              ! Compute solving CPU time, and total assembly and solving CPU time in the 
              ! coupled system iteration (may contain several nonlinear iterations) 
              !------------------------------------------------------------------------------

          END SUBROUTINE

          SUBROUTINE GetCellHeat(C1, U, V, W, HeatCapacity, Element, Material, n)
              TYPE(Element_t) :: Element
              INTEGER :: i, k, n
              REAL(KIND=dp) :: C1(:), U(:), V(:), W(:), HeatCapacity(:), DeathCapacity
              TYPE(ValueList_t), POINTER :: Material
              LOGICAL :: Found

              DeathCapacity = GetConstReal( Material, &
                  'Death Heat Capacity', Found )
              IF ( .NOT.Found ) THEN
                  DeathCapacity = 670.0
              END IF

              IF ( ASSOCIATED( CellStateSol ) ) THEN
                  DO i=1,n
                      k = CellStatePerm(Element % NodeIndexes(i))
                      LocalCellState(i) = CellState((k-1)*ADOFs + 2)
                  END DO
              ELSE
                  LocalCellState = 0.0D0
              ENDIF
              !------------------------------------------------------------------------------
              DO i=1,n
                      IF (LocalCellState(i) > DeadThreshold) THEN
                          C1(i) = 0.0D0
                          U(i) = 0.0
                          V(i) = 0.0
                          W(i) = 0.0
                      END IF  
              END DO
              !------------------------------------------------------------------------------
              !   Death Heat Capacity in died cells
              !------------------------------------------------------------------------------
              DO i = 1,n
              !------------------------------------------------------------------------------
              ! Multiply perfusion coeff by blood density and heat capacity (HeatCapacity(1,i)=product already)
              !RMV CHECK THIS!!
              !------------------------------------------------------------------------------
                   !------------------------------------------------------------------------------
                   !  Check the alive state of the cells
                   !------------------------------------------------------------------------------
                   IF (CellStateModel == 1) THEN
                       IF ((LocalCellState(i) == 0) .AND. (DeathCapacity/=0)) THEN
                           HeatCapacity( i ) = DeathCapacity
                       END IF
                   ELSE
                       IF ((LocalCellState(i) > DeadThreshold) .AND. (DeathCapacity/=0)) THEN
                           HeatCapacity( i ) = DeathCapacity
                       END IF
                   END IF
              END DO
          END SUBROUTINE GetCellHeat

          SUBROUTINE CellAllocations(SolverT, ModelT, SolverParamsT, TimestepT, Temperature, TempPermT)
              TYPE(Solver_t) :: SolverT
              TYPE(Model_t) :: ModelT
              TYPE(ValueList_t), POINTER :: SolverParamsT
              REAL(KIND=dp) :: TimestepT, Temperature(:)
              INTEGER, POINTER :: TempPermT(:)

              INTEGER :: i, j, N, ierr, ios, MaxNbElectricPoints, NbElectricTips
              REAL(KIND=dp), ALLOCATABLE :: x_ElectricTip(:,:), y_ElectricTip(:,:), z_ElectricTip(:,:), &
                  ElectricTipMeasure(:), TotalElectricTipsMeasure, ElectricPowerEpsilon
              INTEGER, ALLOCATABLE :: NbElectricPoints(:)
              REAL(KIND=dp) :: Time, PrevTime, tmpPower, TotalDistanceToTip, DistanceToTip
              CHARACTER(LEN=MAX_NAME_LEN) :: char_MyPe, char_ElectricTip, ElectricTipFile, line, &
                  str1, str2, MaxTemperatureFilename, TestName, TotalPowerFilename
              TYPE(Variable_t), POINTER :: TimeVar
              LOGICAL :: Found = .FALSE., InterpolatedElectricPower

              Solver = SolverT
              Model = ModelT
              SolverParams => SolverParamsT
              Timestep = TimestepT
              TempPerm => TempPermT

              CellStateSol => VariableGet( Solver % Mesh % Variables, 'CellState' )
              IF ( ASSOCIATED( CellStateSol ) ) THEN
                   CellStatePerm => CellStateSol % Perm
                   CellState => CellStateSol % Values
                   ADOFs =  CellStateSol % DOFs
              ELSE
                  CALL Fatal("HeatIncorporateCells", "Cell state variable not defined")
              END IF
              LocalNodes = COUNT( CellStatePerm > 0 )

              N = Solver % Mesh % MaxElementDOFs

              IF (AllocationsDone) THEN
                  DEALLOCATE(&
                      Load, &
                      ElectricPower, &
                      ElementNodes % x, &
                      ElementNodes % y, &
                      ElementNodes % z, &
                      LocalHeatSource, &
                      LocalCellState, &
                      CurrentPower, &
                      PreviousPower, &
                      HeatSource, &
                  )
              END IF

              ALLOCATE(&
                  Load(N), &
                  ElementNodes % x(N), &
                  ElementNodes % y(N), &
                  ElementNodes % z(N), &
                  LocalHeatSource(N), &
                  LocalCellState(N), &
                  HeatSource(LocalNodes), &
                  CurrentPower(LocalNodes), &
                  PreviousPower(LocalNodes), &
                  ElectricPower(LocalNodes), &
              )

              !--------------------------------------------------------------------------
              !      Physical time of the current and previous coupled system iterations: 
              !--------------------------------------------------------------------------             
              Time = 0.0
              PrevTime = 0.0
              !--------------------------------------------------------------------------
              !      If specified in the input file, compute the maximum temperature over the model: 
              !-------------------------------------------------------------------------- 
              ControlMaxTemperature = GetLogical( SolverParams,'Control Max Temperature',Found )      
              IF ( .NOT.Found ) ControlMaxTemperature = .FALSE.
              IF (ControlMaxTemperature) THEN
                  MaxTemperature = 0.0D0
                  DO i=1,LocalNodes
                          IF (MaxTemperature < Temperature(i)) THEN
                              MaxTemperature = Temperature(i)
                          END IF
                  END DO
                  !-------------------------------------------------------------------------- 
                  !      Write the header of the max-temperature control file
                  !--------------------------------------------------------------------------     
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
              END IF ! ControlMaxTemperature

              !!! THIS IS DEPRECATED!
              !
              !--------------------------------------------------------------------------- 
              !       Get electric power geometry if interpolated (multi)line source:
              !--------------------------------------------------------------------------- 
              InterpolatedElectricPower = GetLogical( Model % Simulation, &
                   'Interpolated Electric Power',Found )
              IF (.NOT. Found) InterpolatedElectricPower = .FALSE.

              UseElectricPower = GetLogical( SolverParams, &
                   'Use Electric Power',Found )
              IF (.NOT. Found) UseElectricPower = InterpolatedElectricPower

              !--------------------------------------------------------------------------- 
              IF (InterpolatedElectricPower) THEN
                  !---------------------------------------------------------------------------
                  !           Get the number of tips:
                  !--------------------------------------------------------------------------- 
                  NbElectricTips = GetInteger( Model % Simulation, &
                           'Electric Tips Number', Found )
                  IF ( .NOT.Found ) NbElectricTips = 10
                  !---------------------------------------------------------------------------
                  !           Get the (numerical) width of the tips:
                  !--------------------------------------------------------------------------- 
                  ElectricPowerEpsilon = GetConstReal( Model % Simulation, &
                           'Electric Power Epsilon', Found )
                  IF ( .NOT.Found ) ElectricPowerEpsilon = 1.5
                  !----------------------------------------------------------------------
                  !      Read the coordinates of the points defining the tips in text files
                  !----------------------------------------------------------------------
                  MaxNbElectricPoints = 10        
                  ALLOCATE(x_ElectricTip(NbElectricTips,MaxNbElectricPoints), &
                      y_ElectricTip(NbElectricTips,MaxNbElectricPoints), &
                      z_ElectricTip(NbElectricTips,MaxNbElectricPoints), &
                      ElectricTipMeasure(NbElectricTips), &
                      NbElectricPoints(NbElectricTips)) 

                  x_ElectricTip = 0.0D0
                  y_ElectricTip = 0.0D0
                  z_ElectricTip = 0.0D0
                  ElectricTipMeasure = 0.0D0
                  TotalElectricTipsMeasure = 0.0D0
                  NbElectricPoints = 0
                  !------------------------------------------------------------------------------
                  !   Go through the tips
                  !------------------------------------------------------------------------------     
                  DO j=1,NbElectricTips
                  !------------------------------------------------------------------------------     
                  !   Open the Tips file:
                  !------------------------------------------------------------------------------  
                      ElectricTipFile = GetString( Model % Simulation,'Electric Tips Filename Root',Found )   
                      !------------------------------------------------------------------------------ 
                      IF (Found) THEN
                      !------------------------------------------------------------------------------ 
                          WRITE(char_ElectricTip,*) j
                          char_ElectricTip = ADJUSTL(char_ElectricTip)
                          ElectricTipFile = TRIM(ElectricTipFile) // "_" // TRIM(char_ElectricTip) // ".txt"
                          OPEN(UNIT=10,FILE=ElectricTipFile,STATUS='OLD',ACTION='READ',IOSTAT=ios)
                          !------------------------------------------------------------------------------ 
                          IF(ios/=0) THEN
                              PRINT*,'Could not open file ',ElectricTipFile
                              PRINT*,'I/O Fortran Error number ',ios
                              CALL Fatal( 'NumaHeatSolve', 'Unable to load electric tips file' )
                          ELSE
                              !------------------------------------------------------------------------------ 
                              !   Read the number of points defining the tip geometry:
                              !------------------------------------------------------------------------------ 
                              READ(10,*,END=1) line
                              READ(10,*,END=1) str1, NbElectricPoints(j), str2
                              !------------------------------------------------------------------------------
                              DO i=1,NbElectricPoints(j)  
                                  !------------------------------------------------------------------------------
                                  !       Read the coordinates:
                                  !------------------------------------------------------------------------------
                                  READ(10,*,END=1) x_ElectricTip(j,i), y_ElectricTip(j,i), z_ElectricTip(j,i)
                              !------------------------------------------------------------------------------
                              END DO
                              !------------------------------------------------------------------------------
                              1 CONTINUE
                              CLOSE(10)
                          !------------------------------------------------------------------------------ 
                          END IF
                      !------------------------------------------------------------------------------ 
                      ELSE
                      !------------------------------------------------------------------------------
                      !   If the file can't be found, print an error message and stop the simulation: 
                      !------------------------------------------------------------------------------
                          CALL Info('NumaHeatSolve', &
                              'Please specify electric tips file name root in input file.', Level=1 )
                          CALL Fatal( 'NumaHeatSolve', 'Unable to load electric tips file' )
                      !------------------------------------------------------------------------------
                      END IF ! name of the tip file found
                      !------------------------------------------------------------------------------
                      ! Compute the length of the tip:
                      !------------------------------------------------------------------------------
                      ElectricTipMeasure = 0.0D0
                      !------------------------------------------------------------------------------
                      !   Case of point source
                      !------------------------------------------------------------------------------
                      IF(NbElectricPoints(j)==1) THEN
                      !------------------------------------------------------------------------------
                          ElectricTipMeasure = 1.0D0
                      !------------------------------------------------------------------------------
                      ELSE
                      !------------------------------------------------------------------------------
                          DO i=1,NbElectricPoints(j)-1    
                          !------------------------------------------------------------------------------
                              ElectricTipMeasure(j) = ElectricTipMeasure(j) + &
                                sqrt( (x_ElectricTip(j,i+1)-x_ElectricTip(j,i))**2 +  &
                                      (y_ElectricTip(j,i+1)-y_ElectricTip(j,i))**2 + &
                                      (z_ElectricTip(j,i+1)-z_ElectricTip(j,i))**2 )
                          !------------------------------------------------------------------------------
                          END DO
                      !------------------------------------------------------------------------------
                      END IF
                      !------------------------------------------------------------------------------
                      ! Update the total mesure of the electric source
                      !------------------------------------------------------------------------------
                      TotalElectricTipsMeasure = TotalElectricTipsMeasure + ElectricTipMeasure(j)
                  !------------------------------------------------------------------------------
                  END DO !j
                  !------------------------------------------------------------------------------
                  !   Compute the electric power distribution:
                  !------------------------------------------------------------------------------
                  ElectricPower = 0.0D0
                  !------------------------------------------------------------------------------
                  ! Go through the nodes
                  !------------------------------------------------------------------------------ 
                  DO i=1,LocalNodes
                  !------------------------------------------------------------------------------ 
                      x = Model % Nodes % x(i)
                      y = Model % Nodes % y(i)
                      z = Model % Nodes % z(i)
                      TotalDistanceToTip = 1000000
                      !------------------------------------------------------------------------------ 
                      ! Go through the tips
                      !------------------------------------------------------------------------------ 
                      DO j=1,NbElectricTips
                          !------------------------------------------------------------------------------ 
                          ! Compute the distance to the tip
                          !------------------------------------------------------------------------------ 
                          DistanceToTip = NumaDistanceToElectricTip (x, y, z, x_ElectricTip(j,:), &
                              y_ElectricTip(j,:), z_ElectricTip(j,:), NbElectricPoints(j))
                          !------------------------------------------------------------------------------ 
                          ! The electric power at each node comes from the closest tip
                          !------------------------------------------------------------------------------
                          IF (TotalDistanceToTip>DistanceToTip) THEN
                          !------------------------------------------------------------------------------
                                  !------------------------------------------------------------------------------
                                  ! Test if we are in the neighboorhoof of the tip
                                  !------------------------------------------------------------------------------
                                  IF (DistanceToTip<ElectricPowerEpsilon) THEN
                                      !------------------------------------------------------------------------------ 
                                      ! If source = one point
                                      !------------------------------------------------------------------------------ 
                                      IF (NbElectricPoints(j)==1) THEN
                                          !------------------------------------------------------------------------------ 
                                          !   Constant power distibution
                                          !------------------------------------------------------------------------------ 
                                          ElectricPower(TempPerm(i)) = &
                                                  (1+0.0*COS(Pi*DistanceToTip/ElectricPowerEpsilon))/&
                                                  (4.0*Pi/3.0*ElectricPowerEpsilon**3)
                                          !------------------------------------------------------------------------------ 
                                          !   Smoothed power distibution
                                          !------------------------------------------------------------------------------ 
                                          ! TO BE DONE
                                      !------------------------------------------------------------------------------ 
                                      ELSE
                                          !------------------------------------------------------------------------------ 
                                          ! If source = closed line
                                          !------------------------------------------------------------------------------ 
                                          IF ((x_ElectricTip(j,1)==x_ElectricTip(j,NbElectricPoints(j))) .AND. &
                                          (y_ElectricTip(j,1)==y_ElectricTip(j,NbElectricPoints(j))) .AND. &
                                          (z_ElectricTip(j,1)==z_ElectricTip(j,NbElectricPoints(j)))) THEN
                                              !------------------------------------------------------------------------------ 
                                              !   Constant power distibution
                                              !------------------------------------------------------------------------------ 
                                              ElectricPower(TempPerm(i)) = &
                                                  (1+0.0*COS(Pi*DistanceToTip/ElectricPowerEpsilon))/&
                                                  (TotalElectricTipsMeasure*Pi*ElectricPowerEpsilon**2)
                                              !------------------------------------------------------------------------------ 
                                              !   Smoothed power distibution
                                              !------------------------------------------------------------------------------ 
                                              ! TO BE DONE
                                          !------------------------------------------------------------------------------ 
                                          ELSE
                                              !------------------------------------------------------------------------------ 
                                              !   Constant power distibution
                                              !------------------------------------------------------------------------------     
                                              ElectricPower(TempPerm(i)) = &
                                                  (1+0.0*COS(Pi*DistanceToTip/ElectricPowerEpsilon))/&
                                                  (TotalElectricTipsMeasure*Pi*ElectricPowerEpsilon**2+ &
                                                  !------------------------------------------------------------------------------ 
                                                  ! 0.5 coeff for particular case of electric probe 
                                                  !------------------------------------------------------------------------------ 
                                                  0.5*4.0*Pi/3.0*ElectricPowerEpsilon**3)
                                                  !------------------------------------------------------------------------------ 
                                                  !   Smoothed power distibution
                                                  !------------------------------------------------------------------------------ 
                                                  ! TO BE DONE
                                          !------------------------------------------------------------------------------ 
                                          END IF ! line type
                                          !------------------------------------------------------------------------------                         
                                      END IF ! point or line source
                                      !------------------------------------------------------------------------------ 
                                  END IF !closest tip
                          !------------------------------------------------------------------------------
                          END IF !neighbourhood of the tip
                          !------------------------------------------------------------------------------ 
                      END DO !j
                      !------------------------------------------------------------------------------
                      ! Add the electric power as a variable for vizualization only   
                      !------------------------------------------------------------------------------
                      ElectricPowerVisualization => ElectricPower(1:LocalNodes)
                      CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
                          Solver, 'Electric Power', 1, &
                          ElectricPowerVisualization, TempPerm )  
                      !------------------------------------------------------------------------------     
                  END DO !i
                  !------------------------------------------------------------------------------ 
              END IF !InterpolatedElectricPower

              !------------------------------------------------------------------------------ 
              !       Read the death model in input file
              !------------------------------------------------------------------------------ 

              DeadThreshold = GetConstReal( SolverParams, 'Dead Threshold', Found )
              IF ( .NOT.Found ) DeadThreshold = 0.8
              !------------------------------------------------------------------------------ 
              !   Read some solver options in the input file
              !------------------------------------------------------------------------------

                            !
                            !..........
                            !
              !------------------------------------------------------------------------------ 
              !   Read the dead cells criteria and characteristics in the input file

              !------------------------------------------------------------------------------ 
              !  Get current (physical) time
              !------------------------------------------------------------------------------
              TimeVar => VariableGet( Solver % Mesh % Variables, 'Time' )
              Time = TimeVar % Values(1)  
              !------------------------------------------------------------------------------         
              !   Save Max temperature of the previous time in a file 
              !   Control that this is a new global time iteration, i.e. not only the nonlinear and 
              !  coupled system iterations have changed
              !------------------------------------------------------------------------------             
              IF ( (ControlMaxTemperature) .AND. (PrevTime < Time) ) THEN
                  IF(ParEnv % PEs>1) THEN
                      WRITE(char_MyPe,*) ParEnv % MyPe
                      char_MyPe = ADJUSTL(char_MyPe)
                      MaxTemperatureFilename = 'maxtemp'//'_'//TRIM(TestName)//'_'//TRIM(char_MyPe)//'.dat'       
                  ELSE 
                      MaxTemperatureFilename = 'maxtemp'//'_'//TRIM(TestName)//'.dat'         
                  END IF
                  OPEN(UNIT=1,FILE=MaxTemperatureFilename, FORM='FORMATTED', ACTION='WRITE', POSITION='APPEND', &
                      IOSTAT=ios)
                  WRITE(UNIT=1,FMT='(F13.4)', ADVANCE='no') PrevTime
                  WRITE(UNIT=1,FMT='(F13.4)', ADVANCE='yes') MaxTemperature
                  CLOSE(1)
              END IF
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
                  WRITE(UNIT=1,FMT='(F16.4)', ADVANCE='yes') TotalPower
                  CLOSE(1)
              !------------------------------------------------------------------------------
              END IF

              !------------------------------------------------------------------------------ 
              !       Compute and print allocation time:
              !------------------------------------------------------------------------------
              CALL Info('NumaHeatSolve',Message,Level=4 )
              AllocationsDone = .TRUE.
          END SUBROUTINE CellAllocations

          FUNCTION NumaDistanceToElectricTip (x, y, z, xtip, ytip, ztip, nbpoints ) RESULT( Distance )
              !------------------------------------------------------------------------------ 
              !******************************************************************************
              !
              !   Compute the distance of the point (x,y,z) from a given Tip defined by using
              !     linear interpolation between given points + parametric representation
              !
              !   ARGUMENTS:
              !
              !       REAL(KIND=dp) :: x,y,z
              !           INPUT: Coordinates of the point at which we compute the distance
              !
              !       REAL(KIND=dp) :: xtip(nbpoints),ytip(nbpoints),ztip(nbpoints)
              !           INPUT: Coordinates of the points used for the linear interpolation 
              !
              !   INTEGER :: nbpoints
              !           INPUT: Number of interpolation points
              !
              !******************************************************************************
              REAL(KIND=dp) :: Distance, x, y, z, xtip(nbpoints), ytip(nbpoints), ztip(nbpoints)
              INTEGER :: nbpoints 
              !------------------------------------------------------------------------------ 
              !   Local variables
              !------------------------------------------------------------------------------ 
              INTEGER :: i,j, S_MAX 
              REAL(KIND=dp) :: s, d, x_s, y_s, z_s
              !------------------------------------------------------------------------------   
              Distance = 100000.0D0   
              !------------------------------------------------------------------------------ 
              !   Case of point source
              !------------------------------------------------------------------------------ 
              IF (nbpoints==1) THEN
              !------------------------------------------------------------------------------ 
                  Distance = sqrt( (x-xtip(1))**2 + (y-ytip(1))**2 + (z-ztip(1))**2 )
              ELSE
                  !------------------------------------------------------------------------------ 
                  !   For each linear part, compute the minimal distance in function of parameter s
                  !------------------------------------------------------------------------------  
                  DO i=1,nbpoints-1
                  !------------------------------------------------------------------------------
                      s = 0.0
                      S_MAX = 100
                      !------------------------------------------------------------------------------
                      DO j = 1,S_MAX-1
                      !------------------------------------------------------------------------------
                          x_s = (xtip(i+1)-xtip(i))*s + xtip(i)
                          y_s = (ytip(i+1)-ytip(i))*s + ytip(i)
                          z_s = (ztip(i+1)-ztip(i))*s + ztip(i)

                          d = sqrt( (x-x_s)**2 + (y-y_s)**2 + (z-z_s)**2 )
                          IF (d<Distance) THEN 
                              Distance = d
                          END IF
                          s = j*1.0/(S_MAX-1)
                      !------------------------------------------------------------------------------
                      END DO
                  !------------------------------------------------------------------------------
                  END DO
              !------------------------------------------------------------------------------
              END IF

          END FUNCTION NumaDistanceToElectricTip
END MODULE HeatIncorporateCells
