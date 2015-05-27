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

SUBROUTINE BulkAssembly(Solver, Model, TempSol, StiffMatrix, ForceVector, TransientSimulation, dt, TotalPower, Integ_Force, &
        TempPerm, Temperature, PreviousPower, CurrentPower, &
        IntegErrorToTargetTemperature, DerivErrorToTargetTemperature, TargetTemperature, Time)

    USE MaterialModels
    USE DefUtils
    USE NumaAdaptive
    IMPLICIT NONE

    TYPE(Matrix_t), POINTER :: StiffMatrix
    TYPE(Model_t)  :: Model
    TYPE(Solver_t), TARGET :: Solver
    TYPE(Variable_t), POINTER :: TempSol
    REAL(KIND=dp) :: dt, TotalPower, IntegErrorToTargetTemperature(:),DerivErrorToTargetTemperature(:), PreviousPower(:), &
        CurrentPower(:), TargetTemperature, Integ_Force, Time
    REAL(KIND=dp), POINTER :: Temperature(:), ForceVector(:)
    INTEGER, POINTER :: TempPerm(:)
    LOGICAL :: TransientSimulation

    TYPE(Nodes_t)   :: ElementNodes
    TYPE(Element_t), POINTER :: Element
    TYPE(Variable_t), POINTER :: CellStateSol
    TYPE(ValueList_t), POINTER :: BodyForce
    LOGICAL :: ControlVelocitySize = .FALSE., CellsDeath = .FALSE., ElectricPowerCutOff = .FALSE., Found
        !InterpolatedElectricPower = .FALSE., UseNormedJH = .FALSE.
    INTEGER :: CellStateModel, ADOFs, bf_id, n, i, j, k, t, JHDOFs, LocalNodes, ActiveCells = 0
    INTEGER, POINTER :: CellStatePerm(:)
        !JouleHeatingPerm(:)
    REAL(KIND=dp) :: at, at0, BodyTemperature, DeathCapacity, MaxVelocitySize, DeadThreshold, &
        PowerControl_Kp, PowerControl_Ki, PowerControl_Kd
    REAL(KIND=dp), POINTER :: Hwrk(:,:,:), CellState(:), CellStatePrev(:) !, JouleHeating(:)
    REAL(KIND=dp), ALLOCATABLE :: LocalCellState(:,:), ats(:), LOAD(:), &
        !U(:), V(:), W(:), C1(:), &
        zv(:), LocalMASS(:,:), LocalSTIFF(:,:), PowerForce(:), LocalFORCE(:), &
        PerfusionCoeff(:), HeatCapacity(:), HeatCapacityConvect(:), power(:), Density(:), &
        C0(:), CellwiseHeatSource(:), OldHeatCapacity(:), OldPerfusionCoeff(:)

    LOGICAL :: Stabilize = .FALSE., AllocationsDone = .FALSE., &
        UseBubbles = .FALSE., TemperatureControlledPower = .FALSE., &
        RecalculateElectricTips = .FALSE., CellStateChanged = .FALSE., &
        AssumeOnlyCellStateChanges = .FALSE., ReassembleStiffness = .TRUE., &
        FirstTime = .TRUE., ReassemblePower

    REAL(KIND=dp) :: &
        CPUTime,RealTime, constpower = 0, constpowerprev

    REAL(KIND=dp), POINTER :: &
        ElectricPower(:)

    REAL(KIND=dp), ALLOCATABLE ::  &
        HeatConductivity(:,:,:), &
        !InterMaterialCoeff(:), &
        !VolumeFraction(:), &
        TimeForce(:), &
        HeatSourceCache(:,:), &
        PowerProfileVector(:)

    SAVE LOAD, ElementNodes, HeatConductivity, HeatCapacity, OldHeatCapacity, Density, zv, &
        !U, V, W, C1, &
        AllocationsDone, LocalNodes, C0, Hwrk, LocalMASS, LocalSTIFF, PowerForce, LocalFORCE, &
        !IntermaterialCoeff, &
        HeatCapacityConvect, LocalCellState, AssumeOnlyCellStateChanges, &
        DeathCapacity, power, ElectricPower, CellStateModel, ElectricPowerCutOff, &
        TimeForce, OldPerfusionCoeff, PerfusionCoeff, BodyTemperature, DeadThreshold, Stabilize, &
        UseBubbles, CellsDeath, ControlVelocitySize, HeatSourceCache, &
        !InterpolatedElectricPower, &
        MaxVelocitySize, TemperatureControlledPower, PowerControl_Kp, &
        ActiveCells, FirstTime, PowerProfileVector, constpower, &
        PowerControl_Ki, PowerControl_Kd, ats, RecalculateElectricTips !, VolumeFraction

    INTERFACE
        FUNCTION TotalPowerCompose( LoadVector,Element,n,Nodes) RESULT(TotalPower)
            USE MaterialModels
            USE DefUtils
            USE NumaAdaptive
            IMPLICIT NONE
                REAL(KIND=dp) :: LoadVector(:), TotalPower
                INTEGER :: n
                TYPE(Nodes_t) :: Nodes
                TYPE(Element_t), POINTER :: Element
        END FUNCTION

        FUNCTION NumaReadElectricPower(model, time) RESULT( Power )
        !------------------------------------------------------------------------------ 
            USE DefUtils
          USE GeneralUtils
            IMPLICIT None
        !------------------------------------------------------------------------------   
            TYPE(Model_t) :: model
            REAL(KIND=dp) :: Power,time
        END FUNCTION

        FUNCTION CheckTipsUpdate(Model, Time) RESULT(RecalculateElectricTips)
            USE Types
            IMPLICIT NONE
            TYPE(Model_t)  :: Model
            REAL(KIND=dp) :: Time
            LOGICAL RecalculateElectricTips
        END FUNCTION

        SUBROUTINE TemperatureCompose( MassMatrix,StiffMatrix,ForceVector,  &
                LoadVector, &
                !NodalInterMaterialCoeff, &
                NodalCT,NodalC0,NodalC1,NodalC2, &
                UX,UY,UZ,NodalDensity, &
                NodalPerfusionCoeff, &
                Stabilize, UseBubbles,Element,n,Nodes, Integ_Force, &
                LoadOnly)
            USE MaterialModels
            USE DefUtils
            USE NumaAdaptive
            IMPLICIT NONE

            REAL(KIND=dp) :: ForceVector(:), &
                MassMatrix(:,:),StiffMatrix(:,:),LoadVector(:),UX(:),UY(:),UZ(:), &
                NodalDensity(:), NodalC2(:,:,:), &
                !NodalInterMaterialCoeff(:), &
                NodalC0(:), NodalC1(:), NodalCT(:), &
                Integ_Force, NodalPerfusionCoeff(:)
            LOGICAL :: Stabilize, UseBubbles, LoadOnly

            INTEGER :: n

            TYPE(Nodes_t) :: Nodes
            TYPE(Element_t), POINTER :: Element
        END SUBROUTINE
    END INTERFACE

    TempPerm    => TempSol % Perm
    Temperature => TempSol % Values
    LocalNodes = COUNT( TempPerm > 0 )

    CellStateSol => VariableGet( Solver % Mesh % Variables, 'CellState' )
    IF ( ASSOCIATED( CellStateSol ) ) THEN
        CellStatePerm => CellStateSol % Perm
        CellState => CellStateSol % Values
        CellStatePrev => CellStateSol % PrevValues(:,1)
        ADOFs =  CellStateSol % DOFs
    END IF

    IF ( .NOT. AllocationsDone .OR. Solver % Mesh % Changed ) THEN
        CALL Allocations()
    END IF

    at  = CPUTime()
    at0 = RealTime()
    ats = 0.0
    zv = 0.0

    N = Solver % Mesh % MaxElementNodes
    IF (ActiveCells /= Solver % NumberOfActiveElements) THEN
        RecalculateElectricTips = .TRUE.

        IF (ActiveCells > 0) THEN
            DEALLOCATE(HeatSourceCache)
        END IF

        ALLOCATE(HeatSourceCache(Solver % NumberOfActiveElements, N))

        ActiveCells = Solver % NumberOfActiveElements
        PRINT *, 'B', ActiveCells
    ELSE
        RecalculateElectricTips = CheckTipsUpdate(Model, Time)
        PRINT *, 'A'
    END IF

    ReassemblePower = FirstTime .OR. RecalculateElectricTips

    IF (ReassemblePower) THEN
        PRINT *, "Reassembling Power"
        PowerProfileVector = 0
    END IF

    IF (FirstTime) THEN
        PRINT *, 'First Time'
    END IF

    constpowerprev = constpower
    constpower = NumaReadElectricPower(model, Time)

    DO t=1,Solver % NumberOfActiveElements
    !------------------------------------------------------------------------------             
        at = CPUTime()
        IF ( RealTime() - at0 > 1.0 ) THEN
            WRITE(Message,'(a,i3,a)' ) '   Assembly: ', INT(100.0 - 100.0 * &
                (Solver % NumberOfActiveElements-t) / &
                (1.0*Solver % NumberOfActiveElements)), ' % done'          
            CALL Info( 'NumaHeatSolve', Message, Level=5 )
            at0 = RealTime()
        END IF
        !------------------------------------------------------------------------------
        ! Check if this element belongs to a body where temperature 
        ! should be calculated
        !------------------------------------------------------------------------------
        Element => GetActiveElement(t)
        !------------------------------------------------------------------------------
        ! Get the element nodes characteristics
        !------------------------------------------------------------------------------
        n = GetElementNOFNodes()
        CALL GetElementNodes( ElementNodes )            
        ats(9) = ats(9) - at + CPUTime()
        at = CPUTime()
        !------------------------------------------------------------------------------
        ! Get element material parameters from the input file
        !------------------------------------------------------------------------------
        HeatCapacity = 1.0d0
        HeatConductivity = 0.0d0
        Density = 1.0d0
        !InterMaterialCoeff = 0.0D0
        !VolumeFraction = 0.0D0
        !RMV!!! Should this be zero?
        HeatCapacityConvect = 1.0D0
        PerfusionCoeff = 0.0D0         

        !------------------------------------------------------------------------------
        ! Volume fraction: Vi/V
        !------------------------------------------------------------------------------
        !VolumeFraction(1:n) = GetReal( Model % &
        !Materials(MIN(1,Model % NumberOfMaterials)) % Values, &
        !    'Volume Fraction', Found )
        !IF (.NOT. Found) THEN
        !    VolumeFraction(1:n)  = 0.1d0
        !END IF
        !------------------------------------------------------------------------------
        ! Intermaterial Transfer Coefficient: hA
        !------------------------------------------------------------------------------
        !InterMaterialCoeff(1:n) = GetReal( Model % &
        !    Materials(MIN(1,Model % NumberOfMaterials)) % Values, &
        !    'Convective Transfer Coefficient', Found )
        !IF (.NOT. Found) InterMaterialCoeff(1:n) = 24.4
        !------------------------------------------------------------------------------
        ! Special treament of InterMaterialCoeff for VolumeFraction = 0 or 1 (in these
        ! cases only 1 material, so no convective transfer)
        ! Has to be modified if TDOFs > 2!
        !------------------------------------------------------------------------------ 
        !DO i=1,n
        !    IF ((VolumeFraction(i) == 0.0) .OR. (VolumeFraction(i) == 1.0) ) THEN
        !        InterMaterialCoeff(i) = 0.0
        !    ELSE
        !        InterMaterialCoeff(i) = InterMaterialCoeff(i) / VolumeFraction(i)
        !    END IF
        !END DO
        !------------------------------------------------------------------------------
        ! Heat capacity: c 
        !------------------------------------------------------------------------------
        HeatCapacity(1:n) = GetReal( Model % &
            Materials(MIN(1, Model % NumberOfMaterials)) % Values, &
            'Heat Capacity', Found )
        IF ( .NOT.Found ) THEN
            IF (1==1) HeatCapacity(1:n)= 4180.0
            IF (1==2) HeatCapacity(1:n)= 3600.0
        END IF
        !------------------------------------------------------------------------------
        ! Heat Conductivity: k (might be a tensor)
        !------------------------------------------------------------------------------
        CALL ListGetRealArray( Model % Materials(MIN(1,Model % &
            NumberOfMaterials)) % Values, &
            'Heat Conductivity',Hwrk,n, Element % NodeIndexes, Found )
        IF ( .NOT. Found ) THEN 
            IF (ASSOCIATED(Hwrk)) DEALLOCATE( Hwrk )
            ALLOCATE(Hwrk(1,1,n))
            Hwrk = 0.5121
        END IF
        !------------------------------------------------------------------------------  
        ! Distinguish cases in fonction of the dimension of the entry in the input file
        ! In any case HeatConductivity will be a tensor 
        !------------------------------------------------------------------------------ 
        ! Case entry = scalar   
        !------------------------------------------------------------------------------
        IF ( SIZE(Hwrk,1) == 1 ) THEN
            DO i=1,3
                HeatConductivity( i,i,1:n ) = Hwrk( 1,1,1:n ) 
            END DO
        !------------------------------------------------------------------------------ 
        ! Case entry = vector   
        !------------------------------------------------------------------------------
        ELSE IF ( SIZE(Hwrk,2) == 1 ) THEN
            DO i=1,MIN(3,SIZE(Hwrk,1))
                HeatConductivity(i,i,1:n) = Hwrk(i,1,1:n)
                HeatConductivity(i,i,1:n) = Hwrk(i,1,1:n) 
            END DO
        !------------------------------------------------------------------------------ 
        ! Case entry = tensor   
        !------------------------------------------------------------------------------
        ELSE
            DO i=1,MIN(3,SIZE(Hwrk,1))
                DO j=1,MIN(3,SIZE(Hwrk,2))
                    HeatConductivity( i,j,1:n ) = Hwrk(i,j,1:n)
                    HeatConductivity( i,j,1:n ) = Hwrk(i,j,1:n) 
                END DO
            END DO
        END IF
        IF (ASSOCIATED(Hwrk)) DEALLOCATE( Hwrk )
        !------------------------------------------------------------------------------
        ! Density: rho
        !------------------------------------------------------------------------------
        Density(1:n) = GetReal( Model % &
            Materials(MIN(1,Model % NumberOfMaterials)) % Values, 'Density',Found )
        IF (.NOT. Found) Density(1:n) = 1060
        ats(8) = ats(8) - at + CPUTime()
        at = CPUTime()
        !------------------------------------------------------------------------------
        ! State of cells
        !------------------------------------------------------------------------------
        IF ( ASSOCIATED( CellStateSol ) ) THEN
            CellStateChanged = .FALSE.
            DO i=1,n
                k = CellStatePerm(Element % NodeIndexes(i))
                LocalCellState(1,i) = CellState((k-1)*ADOFs + 1)
                LocalCellState(2,i) = CellState((k-1)*ADOFs + 2)

                !TODO Make 100% cells can't mathematically come back to life or add appropriate condition
                IF (CellStateModel /= 1) THEN
                    CellStateChanged = CellStateChanged .OR. &
                        (CellState((k-1)*ADOFs + 2) > DeadThreshold .AND. CellStatePrev((k-1)*ADOFs + 2) < DeadThreshold)
                END IF
            END DO
        ELSE
            IF (CellStateModel==1) THEN
                LocalCellState(1,1:n) = 1.0D0
                LocalCellState(2,1:n) = 1.0D0
            ELSE
                LocalCellState(1,1:n) = 1.0D0
                LocalCellState(2,1:n) = 0.0D0
            END IF
        ENDIF
        ats(1) = ats(1) - at + CPUTime()
        at = CPUTime()
        !------------------------------------------------------------------------------
        !  Check for convection model and get the convection velocities
        !  C1 = 0 : no convection
        !   C1 = 1 : convection in at least one direction
        !------------------------------------------------------------------------------
        !C1 = 0.0D0
        !U = 0.0D0
        !V = 0.0D0
        !W = 0.0D0
        !------------------------------------------------------------------------------
        ! Read the velocity field in the input file 
        !------------------------------------------------------------------------------
        !U(1:n) = GetReal( Model % Materials(MIN(1,Model % NumberOfMaterials)) % Values, &
        !    'Convection Velocity 1', Found )
        !V(1:n) = GetReal( Model % Materials(MIN(1,Model % NumberOfMaterials)) % Values, &
        !    'Convection Velocity 2', Found )
        !W(1:n) = GetReal( Model % Materials(MIN(1,Model % NumberOfMaterials)) % Values, &
        !    'Convection Velocity 3', Found )    
        
        !------------------------------------------------------------------------------
        !Scale the velocity size to avoid unstabilities if specified:
        !------------------------------------------------------------------------------
        !IF (ControlVelocitySize) THEN
        !    DO i=1,n
        !        IF(ABS(U(i))>MaxVelocitySize) U(i)=U(i)*MaxVelocitySize/ABS(U(i))
        !        IF(ABS(V(i))>MaxVelocitySize) V(i)=V(i)*MaxVelocitySize/ABS(V(i))
        !        IF(ABS(W(i))>MaxVelocitySize) W(i)=W(i)*MaxVelocitySize/ABS(W(i))
        !    END DO
        !!------------------------------------------------------------------------------
        !END IF
        !------------------------------------------------------------------------------
        ! Update C1
        !------------------------------------------------------------------------------
        !DO i=1,n
        !    IF ( (U(i) /= 0.0D0) .OR. (V(i) /= 0.0D0) .OR. (W(i) /= 0.0D0)) THEN 
        !        C1(i) = 1.0
        !    END IF
        !END DO

        ats(2) = ats(2) - at + CPUTime()
        at = CPUTime()
        !------------------------------------------------------------------------------
        !  No more convection in died cells...
        !------------------------------------------------------------------------------
        IF (CellsDeath) THEN
        !------------------------------------------------------------------------------
            !DO i=1,n
            !    !------------------------------------------------------------------------------
            !    !  Adapt some parameters in function of the alive state of the cells
            !    !------------------------------------------------------------------------------
            !    IF (CellStateModel==1) THEN
            !        IF (LocalCellState(1,i) == 0) THEN
            !            C1(i) = 0.0
            !            U(i) = 0.0
            !            V(i) = 0.0
            !            W(i) = 0.0
            !        END IF
            !    ELSE
            !        IF (LocalCellState(2,i) > DeadThreshold) THEN
            !            C1(i) = 0.0
            !            U(i) = 0.0
            !            V(i) = 0.0
            !            W(i) = 0.0
            !        END IF
            !    END IF  
            !END DO
            !------------------------------------------------------------------------------
            !   Death Heat Capacity in died cells
            !------------------------------------------------------------------------------
            DO i = 1,n
                !------------------------------------------------------------------------------
                !  Check the alive state of the cells
                !------------------------------------------------------------------------------
                IF (CellStateModel==1) THEN
                    IF ((LocalCellState(1,i) == 0) .AND. (DeathCapacity/=0)) THEN
                        HeatCapacity( i ) = DeathCapacity
                    END IF
                ELSE
                    IF ((LocalCellState(2,i) > DeadThreshold) .AND. (DeathCapacity/=0)) THEN
                        IF (CellStateChanged) THEN
                            OldHeatCapacity(i) = HeatCapacity(i)
                        END IF
                        HeatCapacity( i ) = DeathCapacity
                    END IF
                END IF
            END DO  
        !------------------------------------------------------------------------------
        END IF ! Cells Death modelling
        !------------------------------------------------------------------------------
        ! Compute time derivative coefficient = c * rho 
        !------------------------------------------------------------------------------
        DO i=1,n
            HeatCapacity(i) = Density(i) * HeatCapacity(i)
            OldHeatCapacity(i) = Density(i) * OldHeatCapacity(i)
        END DO


        !------------------------------------------------------------------------------
        ! Get perfusion coefficient for classical bio-heat eq with perfusion
        !------------------------------------------------------------------------------
        PerfusionCoeff(1:n) = GetReal( Model % Materials(Model % NumberOfMaterials) % Values, &
            'Tissue Perfusion Coefficient', Found )
        IF ( .NOT.Found ) PerfusionCoeff=0.0D0
        !------------------------------------------------------------------------------
        ! If classical bio-heat eq with perfusion, no porous flow and no heat transfer 
        ! between blood and tissue. No blood equation.
        !------------------------------------------------------------------------------
        !IF (ANY(PerfusionCoeff/=0.0D0)) THEN
        !    C1 = 0.0D0
        !    U = 0.0D0
        !    V = 0.0D0
        !    W = 0.0D0
        !    !InterMaterialCoeff = 0.0D0
        !    !VolumeFraction(1:n) = 0.0D0
        !END IF
        !------------------------------------------------------------------------------
        ! Perfusion stops when cell dies 
        !------------------------------------------------------------------------------
        IF (CellsDeath) THEN
        !------------------------------------------------------------------------------
            DO i=1,n
                IF (CellStateModel==1) THEN
                    IF (LocalCellState(2,i) == 0) THEN
                        PerfusionCoeff(i) = 0.0D0
                    END IF
                ELSE
                    IF (LocalCellState(2,i) > DeadThreshold) THEN
                        IF (CellStateChanged) THEN
                            OldPerfusionCoeff(i) = PerfusionCoeff(i)
                        END IF
                        PerfusionCoeff(i) = 0.0D0
                    END IF
                END IF      
            END DO
        !------------------------------------------------------------------------------
        END IF
        !------------------------------------------------------------------------------
        ! Multiply perfusion coeff by blood density and heat capacity (HeatCapacity(1,i)=product already)
        !------------------------------------------------------------------------------
        DO i=1,n
            PerfusionCoeff(i) = PerfusionCoeff(i) * HeatCapacity(i)  
            OldPerfusionCoeff(i) = OldPerfusionCoeff(i) * OldHeatCapacity(i)  
        END DO
        !------------------------------------------------------------------------------
        ! Add body forces, if any
        !------------------------------------------------------------------------------ 
        ats(3) = ats(3) - at + CPUTime()
        at = CPUTime()
        LOAD = 0.0D0
        power = 0.0D0
        !------------------------------------------------------------------------------
        BodyForce => GetBodyForce()
        IF ( ASSOCIATED( BodyForce ) ) THEN
            bf_id = GetBodyForceId()

            !TODO: It would be much more efficient to cache a nodal value, but this maintains parity with previous version
            IF (RecalculateElectricTips) THEN
                HeatSourceCache(t,:) = GetReal( BodyForce, 'Heat Source', Found )
                IF (.not. Found) THEN
                    HeatSourceCache(t,:) = GetReal( BodyForce, 'Heat Source 1', Found )
                END IF
            END IF

            !------------------------------------------------------------------------------
            ! Given heat source = Vi/V * f
            !------------------------------------------------------------------------------
            IF(TemperatureControlledPower) THEN
            !------------------------------------------------------------------------------
            !   If temperature-controlled power, get the power from computation: 
            !------------------------------------------------------------------------------
                DO i = 1,n
                !------------------------------------------------------------------------------
                    k = TempPerm(Element % NodeIndexes(i))
                    power(i) = PreviousPower(k) + &
                        PowerControl_Kp * (TargetTemperature-Temperature(k)) + & 
                        PowerControl_Ki * IntegErrorToTargetTemperature(k) + & 
                        PowerControl_Kd * DerivErrorToTargetTemperature(k)
                    !------------------------------------------------------------------------------
                    ! Control of max and min power: 
                    !------------------------------------------------------------------------------
                    power(i) = MIN(power(i),100000000.0)
                    power(i) = MAX(power(i),0.0)
                    !------------------------------------------------------------------------------
                    CurrentPower(k) = power(i)
                    !------------------------------------------------------------------------------
                    ! If T>373K, conductivity=0, modelised by power=0 in heat equation: 
                    !------------------------------------------------------------------------------
                    IF ((ElectricPowerCutOff) .AND. (Temperature(k)>373)) THEN
                        power(i) = 0.0
                    END IF
                !------------------------------------------------------------------------------ 
                END DO !i
            !------------------------------------------------------------------------------
            ELSE
                !------------------------------------------------------------------------------
                ! Read the electric power used as a coefficient of heat source: 
                !------------------------------------------------------------------------------
                !power(1:n) = power(1:n) + & 
                !    GetReal( BodyForce, 'Electric Power', Found )
                !------------------------------------------------------------------------------
                ! If T>373K, conductivity=0, modelised by power=0:
                !------------------------------------------------------------------------------
                IF (ElectricPowerCutOff) THEN
                    DO i = 1,n
                        k = TempPerm(Element % NodeIndexes(i))
                        IF (Temperature(k)>373) THEN
                            power(i) = 0.0
                        END IF
                    END DO
                END IF
            !------------------------------------------------------------------------------
            END IF
            !------------------------------------------------------------------------------
            ! Read the body force value in the input file and modify Load
            !------------------------------------------------------------------------------
            !IF (InterpolatedElectricPower) THEN
            !    DO i = 1,n
            !        k = TempPerm(Element % NodeIndexes(i))
            !        LOAa(i) = LOAD(i) + power(i) * ElectricPower(k)
            !    END DO
            !ELSEIF (UseNormedJH) THEN
            !    DO i = 1,n
            !        k = JouleHeatingPerm(Element % NodeIndexes(i))
            !        LOAD(i) = LOAD(i) + power(i) * JouleHeating((k-1)*JHDOFs + 1)
            !    END DO
            !ELSE
                !LOAD(1:n) = LOAD(1:n) + power(1:n) * HeatSourceCache(t,1:n) ! & 
                !    GetReal( BodyForce, 'Heat Source', Found ) 
                !IF (.not. Found) THEN
                !    LOAD(1:n) = LOAD(1:n) + power(1:n) * & 
                !        GetReal( BodyForce, 'Heat Source 1', Found ) 
                !END IF
            !END IF
        !------------------------------------------------------------------------------
        END IF ! body force
        ats(4) = ats(4) - at + CPUTime()
        at = CPUTime()
        !------------------------------------------------------------------------------
        ! Integrate power over element
        !------------------------------------------------------------------------------
        !RMV TotalPower = TotalPower + TotalPowerCompose( LOAD, Element, n, ElementNodes )
        ats(5) = ats(5) - at + CPUTime()
        at = CPUTime()

        !------------------------------------------------------------------------------
        ! Get element local matrices, and RHS vectors
        !------------------------------------------------------------------------------
        C0 = 0.0d0        
        !------------------------------------------------------------------------------         
        ReassembleStiffness = .FALSE.
        IF (FirstTime .OR. .NOT. AssumeOnlyCellStateChanges .OR. CellStateModel == 1) THEN
            !------------------------------------------------------------------------------
            ! Add one part of the perfusion term: rho(b)*cp(b)*T(body)
            !------------------------------------------------------------------------------     
            LOAD(1:n) = LOAD(1:n) + PerfusionCoeff(1:n) * BodyTemperature 

            CALL TemperatureCompose( &
                LocalMASS, LocalSTIFF, LocalFORCE, LOAD, &
                HeatCapacity, C0, HeatCapacityConvect, HeatConductivity, &
                zv, zv, zv, Density, PerfusionCoeff, Stabilize, UseBubbles, Element, n, ElementNodes, Integ_Force, &
                .FALSE.)
            ReassembleStiffness = .TRUE.
            ats(15) = ats(15) - at + CPUTime()
        ELSE IF (CellStateChanged) THEN
            PerfusionCoeff = PerfusionCoeff - OldPerfusionCoeff
            HeatCapacity = HeatCapacity - OldHeatCapacity

            !------------------------------------------------------------------------------
            ! Add one part of the perfusion term: rho(b)*cp(b)*T(body)
            !------------------------------------------------------------------------------     
            LOAD(1:n) = LOAD(1:n) + PerfusionCoeff(1:n) * BodyTemperature 

            CALL TemperatureCompose( &
                LocalMASS, LocalSTIFF, LocalFORCE, LOAD, &
                HeatCapacity, C0, HeatCapacityConvect, HeatConductivity, &
                zv, zv, zv, Density, PerfusionCoeff, Stabilize, UseBubbles, Element, n, ElementNodes, Integ_Force, &
                .FALSE.)
            ReassembleStiffness = .TRUE.
            ats(16) = ats(16) - at + CPUTime()
        END IF
        at = CPUTime()
        !------------------------------------------------------------------------------
        ! If time dependent simulation, add mass matrix to stiff matrix
        !------------------------------------------------------------------------------
        IF ( ReassembleStiffness .AND. TransientSimulation ) THEN
                CALL Add1stOrderTime( LocalMASS, LocalSTIFF, LocalFORCE, dt, n, 1, &
                    TempPerm(Element % NodeIndexes(1:n)), Solver )   
        END IF 

        !BUBBLES:
        !IF (UseBubbles) THEN
        !    TimeForce  = 0.0
        !    CALL Condensate( n, LocalSTIFF, LocalFORCE, TimeForce )
        !END IF
  
        ats(6) = ats(6) - at + CPUTime()
        at = CPUTime()
        !------------------------------------------------------------------------------
        ! Update global matrices from local matrices
        !------------------------------------------------------------------------------
        IF (ReassembleStiffness) THEN
            CALL UpdateGlobalEquations( StiffMatrix, LocalSTIFF, &
            forcevector, LocalFORCE, n, 1, TempPerm(Element % NodeIndexes) )
        END IF

        IF (ReassemblePower) THEN
            CALL TemperatureCompose( &
                LocalMASS, LocalSTIFF, PowerForce, HeatSourceCache(t,:), &
                HeatCapacity, C0, HeatCapacityConvect, HeatConductivity, &
                zv, zv, zv, Density, PerfusionCoeff, Stabilize, UseBubbles, Element, n, ElementNodes, Integ_Force, &
                .TRUE.)

            CALL UpdateGlobalForce(&
            PowerProfileVector, PowerForce, n, 1, TempPerm(Element % NodeIndexes) )
        END IF
        ats(7) = ats(7) - at + CPUTime()
        at = CPUTime()
    !------------------------------------------------------------------------------
    END DO     !  Bulk elements

    ForceVector = ForceVector + (constpower - constpowerprev) * PowerProfileVector

    FirstTime = .FALSE.
    RecalculateElectricTips = .FALSE.
    PRINT *, ats

    CONTAINS
        SUBROUTINE Allocations()

            INTEGER :: N, j, ios, istat, MaxNbElectricPoints = 10, NbElectricTips
            CHARACTER(LEN=100) :: str1, str2, char_ElectricTip, ElectricTipFile, line, &
                TestName
            REAL(KIND=dp) :: alloctime, allocrealtime, DistanceToTip, ElectricPowerEpsilon, &
                TotalDistanceToTip, TotalElectricTipsMeasure, xProjTip, yProjTip, zProjTip
            !TYPE(Variable_t), POINTER :: JouleHeatingSol
            INTEGER, ALLOCATABLE :: NbElectricPoints(:)
            REAL(KIND=dp), ALLOCATABLE :: &
                x, y, z, &
                x_ElectricTip(:,:),y_ElectricTip(:,:),z_ElectricTip(:,:), ElectricTipMesure(:)
            REAL(KIND=dp), POINTER :: VolF(:), ElectricPowerVisualization(:)
            !LOGICAL :: VarVolumeFraction
            TYPE(ValueList_t),POINTER :: SolverParams
            REAL, EXTERNAL :: NumaDistanceToElectricTip

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
                    !U, V, W,                      &
                    ElementNodes % x,             &
                    ElementNodes % y,             &
                    ElementNodes % z,             &
                    Density,                 &
                    C0,                            &
                    HeatCapacity,                 &
                    OldHeatCapacity,                 &
                    HeatConductivity,             &
                    LOAD,                         &
                    power,                                          &
                    LocalMASS, LocalSTIFF,        &
                    !InterMaterialCoeff,           &
                    HeatCapacityConvect,          &
                    !C1,                           &
                    zv, &
                    !VolumeFraction,              &
                    LocalCellState,        &
                    ElectricPower,                          &
                    TimeForce,                                   &
                    PerfusionCoeff,                         &               
                    PowerProfileVector, &
                    OldPerfusionCoeff,                         &               
                    ats, &
                    VolF)
            END IF

            ALLOCATE(                                         &
                !U( N ),   V( N ),         &
                !W( N ),                         &
                ElementNodes % x( N ),                &
                ElementNodes % y( N ),                &
                ElementNodes % z( N ),                &
                Density( N ),   &
                C0(N),                                &
                HeatCapacity( N ),              &
                OldHeatCapacity( N ),              &
                HeatConductivity( 3,3,N ),      &
                LOAD( N ),                      &
                power( N ),                     &  
                LocalSTIFF( 2*N,2*N ),    & 
                LocalMASS( 2*N,2*N ),     & 
                LocalFORCE( 2*N ),              & 
                PowerFORCE( 2*N ),              & 
                !InterMaterialCoeff( N ),        & 
                !C1(N), &
                zv(N), &
                ats(100),                          & 
                HeatCapacityConvect( N ),       &
                !VolumeFraction(N),              &
                LocalCellState(ADOFs, N),              &
                ElectricPower(LocalNodes),      &
                TimeForce(2*N),                                         &
                PerfusionCoeff( N ),                        &
                OldPerfusionCoeff( N ),                        &
                VolF(LocalNodes), &
                PowerProfileVector(LocalNodes), &
                STAT=istat                            )

            IF ( istat /= 0 ) THEN
                CALL Fatal( 'NumaHeatSolve', 'Memory allocation error' )
            END IF

            NULLIFY( Hwrk )
            AllocationsDone = .TRUE.

            TemperatureControlledPower = GetLogical( Model % Simulation, &
                'Temperature Controlled Electric Power',Found )
            IF ( .NOT.Found ) TemperatureControlledPower = .FALSE.
    !------------------------------------------------------------------------------ 
    !       Get the test name
    !------------------------------------------------------------------------------
            TestName = GetString( CurrentModel % Simulation,'Test Name', Found ) 
    !------------------------------------------------------------------------------ 
    !   If space dependent volume fraction specified in input file, add the 
    !       volume fraction of blood as a new variable:
    !------------------------------------------------------------------------------ 
            VolF = 0.0D0
    !------------------------------------------------------------------------------ 
    !   Check if space dependent volume fraction specified in input file 
    !------------------------------------------------------------------------------ 
            !VarVolumeFraction = GetLogical( SolverParams,'Variable Volume Fraction', Found )
            !IF ( .NOT. Found ) VarVolumeFraction = .FALSE.
            !IF(VarVolumeFraction) THEN
    !------------------------------------------------------------------------------ 
    !       Add the volume fraction of blood as a new variable
    !------------------------------------------------------------------------------     
            !    CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
            !        Solver, 'Blood Volume fraction', 1, &
            !        VolF, TempPerm )
            !END IF
    !------------------------------------------------------------------------------ 
    !   Check if Cell Death Modelling specified in input file 
    !------------------------------------------------------------------------------ 
            CellsDeath = GetLogical( SolverParams,'Cell Death Modelling', Found )
            IF ( .NOT. Found ) CellsDeath = .TRUE.

            AssumeOnlyCellStateChanges = GetLogical( SolverParams,'Assume Only Cell State Changes', Found )
            IF ( .NOT. Found ) AssumeOnlyCellStateChanges = .FALSE.
    !------------------------------------------------------------------------------             
    !       Get electric power geometry if interpolated (multi)line source:
    !------------------------------------------------------------------------------ 
    !!        InterpolatedElectricPower = GetLogical( Model % Simulation, &
    !            'Interpolated Electric Power', Found )
    !        IF (.NOT. Found) InterpolatedElectricPower = .FALSE.

    !        !JouleHeatingSol => VariableGet(Solver % Mesh % Variables, 'Normed Joule Heating' )
    !        !IF ( ASSOCIATED( JouleHeatingSol ) ) THEN
    !        !    JouleHeatingPerm => JouleHeatingSol % Perm
    !        !    JouleHeating => JouleHeatingSol % Values
    !        !    JHDOFs =  JouleHeatingSol % DOFs
    !        !    UseNormedJH = .TRUE.
    !        !END IF

    !!------------------------------------------------------------------------------ 
    !        IF (InterpolatedElectricPower) THEN
    !!------------------------------------------------------------------------------
    !!           Get the number of tips:
    !!------------------------------------------------------------------------------ 
    !            NbElectricTips = GetInteger( Model % Simulation, &
    !                'Electric Tips Number', Found )
    !            IF ( .NOT.Found ) NbElectricTips = 10
    !!------------------------------------------------------------------------------
    !!           Get the (numerical) width of the tips:
    !!------------------------------------------------------------------------------ 
    !            ElectricPowerEpsilon = GetConstReal( Model % Simulation, &
    !                'Electric Power Epsilon', Found )
    !            IF ( .NOT.Found ) ElectricPowerEpsilon = 1.5

    !            ALLOCATE(x_ElectricTip(NbElectricTips,MaxNbElectricPoints), &
    !                y_ElectricTip(NbElectricTips,MaxNbElectricPoints), &
    !                z_ElectricTip(NbElectricTips,MaxNbElectricPoints), &
    !                ElectricTipMesure(NbElectricTips), &
    !                NbElectricPoints(NbElectricTips)) 
    !            x_ElectricTip = 0.0D0
    !            y_ElectricTip = 0.0D0
    !            z_ElectricTip = 0.0D0
    !!------------------------------------------------------------------------------
    !!           Read the coordinates of the points defining the tips in text files
    !!------------------------------------------------------------------------------
    !            ElectricTipMesure = 0.0D0
    !            TotalElectricTipsMeasure = 0.0D0
    !            NbElectricPoints = 0
    !            !------------------------------------------------------------------------------
    !            !   Go through the tips
    !            !------------------------------------------------------------------------------     
    !            DO j=1,NbElectricTips
    !            !------------------------------------------------------------------------------     
    !            !   Open the Tips file:
    !            !------------------------------------------------------------------------------  
    !                ElectricTipFile = GetString( Model % Simulation,'Electric Tips Filename Root',Found )   
    !                !------------------------------------------------------------------------------ 
    !                IF (Found) THEN
    !                !------------------------------------------------------------------------------ 
    !                    WRITE(char_ElectricTip,*) j
    !                    char_ElectricTip = ADJUSTL(char_ElectricTip)
    !                    ElectricTipFile = TRIM(ElectricTipFile) // "_" // TRIM(char_ElectricTip) // ".txt"
    !                    OPEN(UNIT=10,FILE=ElectricTipFile,STATUS='OLD',ACTION='READ',IOSTAT=ios)
    !                    !------------------------------------------------------------------------------ 
    !                    IF(ios/=0) THEN
    !                        PRINT*,'Could not open file ',ElectricTipFile
    !                        PRINT*,'I/O Fortran Error number ',ios
    !                        CALL Fatal( 'NumaHeatSolve', 'Unable to load electric tips file' )
    !                    ELSE
    !                        !------------------------------------------------------------------------------ 
    !                        !   Read the number of points defining the tip geometry:
    !                        !------------------------------------------------------------------------------ 
    !                        READ(10,*,END=1) line
    !                        READ(10,*,END=1) str1, NbElectricPoints(j), str2
    !                        !------------------------------------------------------------------------------
    !                        DO i=1,NbElectricPoints(j)  
    !                            !------------------------------------------------------------------------------
    !                            !       Read the coordinates:
    !                            !------------------------------------------------------------------------------
    !                            READ(10,*,END=1) x_ElectricTip(j,i), y_ElectricTip(j,i), z_ElectricTip(j,i)
    !                        !------------------------------------------------------------------------------
    !                        END DO
    !                        !------------------------------------------------------------------------------
    !                        1 CONTINUE
    !                        CLOSE(10)
    !                    !------------------------------------------------------------------------------ 
    !                    END IF
    !                !------------------------------------------------------------------------------ 
    !                ELSE
    !                !------------------------------------------------------------------------------
    !                !   If the file can't be found, print an error message and stop the simulation: 
    !                !------------------------------------------------------------------------------
    !                    CALL Info('NumaHeatSolve', &
    !                        'Please specify electric tips file name root in input file.', Level=1 )
    !                    CALL Fatal( 'NumaHeatSolve', 'Unable to load electric tips file' )
    !                !------------------------------------------------------------------------------
    !                END IF ! name of the tip file found
    !                !------------------------------------------------------------------------------
    !                ! Compute the length of the tip:
    !                !------------------------------------------------------------------------------
    !                ElectricTipMesure = 0.0D0
    !                !------------------------------------------------------------------------------
    !                !   Case of point source
    !                !------------------------------------------------------------------------------
    !                IF(NbElectricPoints(j)==1) THEN
    !                !------------------------------------------------------------------------------
    !                    ElectricTipMesure = 1.0D0
    !                !------------------------------------------------------------------------------
    !                ELSE
    !                !------------------------------------------------------------------------------
    !                    DO i=1,NbElectricPoints(j)-1    
    !                    !------------------------------------------------------------------------------
    !                        ElectricTipMesure(j) = ElectricTipMesure(j) + sqrt( (x_ElectricTip(j,i+1)-x_ElectricTip(j,i))**2 +  &
    !                            (y_ElectricTip(j,i+1)-y_ElectricTip(j,i))**2 + &
    !                            (z_ElectricTip(j,i+1)-z_ElectricTip(j,i))**2 )
    !                    !------------------------------------------------------------------------------
    !                    END DO
    !                !------------------------------------------------------------------------------
    !                END IF
    !                !------------------------------------------------------------------------------
    !                ! Update the total mesure of the electric source
    !                !------------------------------------------------------------------------------
    !                TotalElectricTipsMeasure = TotalElectricTipsMeasure + ElectricTipMesure(j)
    !            !------------------------------------------------------------------------------
    !            END DO !j
    !            !------------------------------------------------------------------------------
    !            !   Compute the electric power distribution:
    !            !------------------------------------------------------------------------------
    !            ElectricPower = 0.0D0
    !            !------------------------------------------------------------------------------
    !            ! Go through the nodes
    !            !------------------------------------------------------------------------------ 
    !            DO i=1,LocalNodes
    !            !------------------------------------------------------------------------------ 
    !                x = Model % Nodes % x(i)
    !                y = Model % Nodes % y(i)
    !                z = Model % Nodes % z(i)
    !                TotalDistanceToTip = 1000000
    !                !------------------------------------------------------------------------------ 
    !                ! Go through the tips
    !                !------------------------------------------------------------------------------ 
    !                DO j=1,NbElectricTips
    !                    !------------------------------------------------------------------------------ 
    !                    ! Compute the distance to the tip
    !                    !------------------------------------------------------------------------------ 
    !                    DistanceToTip = NumaDistanceToElectricTip (x, y, z, x_ElectricTip(j,:), &
    !                        y_ElectricTip(j,:), z_ElectricTip(j,:), nbElectricpoints(j), &
    !                        xProjTip, yProjTip, zProjTip )
    !                    !------------------------------------------------------------------------------ 
    !                    ! The electric power at each node comes from the closest tip
    !                    !------------------------------------------------------------------------------
    !                    IF (TotalDistanceToTip>DistanceToTip) THEN
    !                    !------------------------------------------------------------------------------
    !                            !------------------------------------------------------------------------------
    !                            ! Test if we are in the neighboorhoof of the tip
    !                            !------------------------------------------------------------------------------
    !                            IF (DistanceToTip<ElectricPowerEpsilon) THEN
    !                                !------------------------------------------------------------------------------ 
    !                                ! If source = one point
    !                                !------------------------------------------------------------------------------ 
    !                                IF (nbElectricpoints(j)==1) THEN
    !                                    !------------------------------------------------------------------------------ 
    !                                    !   Constant power distibution
    !                                    !------------------------------------------------------------------------------ 
    !                                    ElectricPower(TempPerm(i)) = &
    !                                            (1+0.0*COS(Pi*DistanceToTip/ElectricPowerEpsilon))/&
    !                                            (4.0*Pi/3.0*ElectricPowerEpsilon**3)
    !                                    !------------------------------------------------------------------------------ 
    !                                    !   Smoothed power distibution
    !                                    !------------------------------------------------------------------------------ 
    !                                    ! TO BE DONE
    !                                !------------------------------------------------------------------------------ 
    !                                ELSE
    !                                    !------------------------------------------------------------------------------ 
    !                                    ! If source = closed line
    !                                    !------------------------------------------------------------------------------ 
    !                                    IF ((x_ElectricTip(j,1)==x_ElectricTip(j,nbElectricpoints(j))) .AND. &
    !                                    (y_ElectricTip(j,1)==y_ElectricTip(j,nbElectricpoints(j))) .AND. &
    !                                    (z_ElectricTip(j,1)==z_ElectricTip(j,nbElectricpoints(j)))) THEN
    !                                        !------------------------------------------------------------------------------ 
    !                                        !   Constant power distibution
    !                                        !------------------------------------------------------------------------------ 
    !                                        ElectricPower(TempPerm(i)) = &
    !                                            (1+0.0*COS(Pi*DistanceToTip/ElectricPowerEpsilon))/&
    !                                            (TotalElectricTipsMeasure*Pi*ElectricPowerEpsilon**2)
    !                                        !------------------------------------------------------------------------------ 
    !                                        !   Smoothed power distibution
    !                                        !------------------------------------------------------------------------------ 
    !                                        ! TO BE DONE
    !                                    !------------------------------------------------------------------------------ 
    !                                    ELSE
    !                                        !------------------------------------------------------------------------------ 
    !                                        !   Constant power distibution
    !                                        !------------------------------------------------------------------------------     
    !                                        ElectricPower(TempPerm(i)) = &
    !                                            (1+0.0*COS(Pi*DistanceToTip/ElectricPowerEpsilon))/&
    !                                            (TotalElectricTipsMeasure*Pi*ElectricPowerEpsilon**2+ &
    !                                            !------------------------------------------------------------------------------ 
    !                                            ! 0.5 coeff for particular case of electric probe 
    !                                            !------------------------------------------------------------------------------ 
    !                                            0.5*4.0*Pi/3.0*ElectricPowerEpsilon**3)
    !                                            !------------------------------------------------------------------------------ 
    !                                            !   Smoothed power distibution
    !                                            !------------------------------------------------------------------------------ 
    !                                            ! TO BE DONE
    !                                    !------------------------------------------------------------------------------ 
    !                                    END IF ! line type
    !                                    !------------------------------------------------------------------------------                         
    !                                END IF ! point or line source
    !                                !------------------------------------------------------------------------------ 
    !                            END IF !closest tip
    !                    !------------------------------------------------------------------------------
    !                    END IF !neighbourhood of the tip
    !                    !------------------------------------------------------------------------------ 
    !                END DO !j
    !                !------------------------------------------------------------------------------
    !                ! Add the electric power as a variable for vizualization only   
    !                !------------------------------------------------------------------------------
    !                ElectricPowerVisualization => ElectricPower(2:LocalNodes+1)
    !                CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
    !                Solver, 'Electric power', 1, &
    !                ElectricPowerVisualization, TempPerm )  
    !                !------------------------------------------------------------------------------     
    !            END DO !i
    !        !------------------------------------------------------------------------------ 
    !        END IF !InterpolatedElectricPower

    !------------------------------------------------------------------------------ 
    !       Read the death model in input file
    !------------------------------------------------------------------------------ 
            CellStateModel = GetInteger( Model % Simulation,'Cell State Model', Found )
            IF ( .NOT.Found ) CellStateModel = 2


            IF (CellStateModel == 2) THEN
                DeadThreshold = GetConstReal( SolverParams, 'Model 2 Dead Threshold', Found )
            END IF
            IF ( .NOT.Found ) DeadThreshold = 0.8
    !------------------------------------------------------------------------------
    !       Check if electric power has to be cut off when T>373 to approx water evaporation
    !------------------------------------------------------------------------------
            ElectricPowerCutOff = ListGetLogical( SolverParams, 'Electric Power Cut Off ', Found )
            IF ( .NOT.Found ) ElectricPowerCutOff = .TRUE.
    !---    --------------------------------------------------------------------------- 
    !       Read some solver options in the input file
    !---    ---------------------------------------------------------------------------
            Stabilize = GetLogical( SolverParams,'Stabilize',Found )
            IF ( .NOT.Found ) Stabilize = .TRUE.

            UseBubbles = GetLogical( SolverParams,'Bubbles',Found )
            IF ( .NOT.Found ) UseBubbles = .FALSE.
    !---    --------------------------------------------------------------------------- 
    !       Read the died cells criteria and characteristics in the input file
    !---    --------------------------------------------------------------------------- 
    !           Specified heat capacity after death
    !---    ---------------------------------------------------------------------------                 
            DeathCapacity = GetConstReal( Model % &
                Materials(MIN(1,Model % NumberOfMaterials)) % Values, &
                'Death Heat Capacity', Found )
            IF ( .NOT.Found ) THEN
                DeathCapacity = 670.0D0
                CALL Error( 'NumaHeatSolve',  "Death Heat Capacity not found")
            END IF
    !---    ---------------------------------------------------------------------------
    !       Get body temperature from input file (for perfusion)
    !---    ---------------------------------------------------------------------------
            BodyTemperature = GetConstReal( Model % Materials(Model % NumberOfMaterials) % Values, &
                'Body Temperature', Found )
            IF ( .NOT.Found ) BodyTemperature=310.0D0

    !------------------------------------------------------------------------------ 
    !   Check if velocity size has to be scaled to avoid unstabilities 
    !------------------------------------------------------------------------------ 
            ControlVelocitySize = GetLogical( SolverParams,'Control Velocity Size',Found )
            IF ( .NOT.Found ) ControlVelocitySize = .FALSE.
            IF(ControlVelocitySize) THEN
    !------------------------------------------------------------------------------ 
    !           Get the max size for each component:
    !------------------------------------------------------------------------------     
                MaxVelocitySize = GetConstReal( SolverParams, 'Max Velocity Size', Found )
                IF ( .NOT.Found ) MaxVelocitySize=2.0D0
            END IF
    !------------------------------------------------------------------------------ 
    !   Check if the electric power has to be controlled in function of temperature 
    !------------------------------------------------------------------------------
            TemperatureControlledPower = GetLogical( Model % Simulation, &
                'Temperature Controlled Electric Power',Found )
            IF ( .NOT.Found ) TemperatureControlledPower = .FALSE.
    !------------------------------------------------------------------------------
            IF(TemperatureControlledPower) THEN
    !------------------------------------------------------------------------------ 
    !       Get the PID parameters:
    !------------------------------------------------------------------------------     
                PowerControl_Kp = GetConstReal( Model % Simulation, &
                    'Proportional Gain For Electric Power Control', Found )
                IF ( .NOT.Found ) PowerControl_Kp = 0.0

                PowerControl_Kd = GetConstReal( Model % Simulation, &
                    'Derivative Gain For Electric Power Control', Found )
                IF ( .NOT.Found ) PowerControl_Kd = 0.0

                PowerControl_Ki = GetConstReal( Model % Simulation, &
                    'Integral Gain For Electric Power Control', Found )
                IF ( .NOT.Found ) PowerControl_Ki = 0.0

                PRINT *,'PID Temperature Controlled Electric Power, with parameters:'
                PRINT *,'- PID Proportional gain = ',PowerControl_Kp            
                PRINT *,'- PID Derivative gain = ',PowerControl_Kd  
                PRINT *,'- PID Integral gain = ',PowerControl_Ki    
                PRINT *,'- PID Target Temperature (K) = ',TargetTemperature 
    !------------------------------------------------------------------------------     
    !           Initialize the corresponding variables:
    !------------------------------------------------------------------------------     
                CurrentPower = 0.0D0
    !------------------------------------------------------------------------------     
            END IF !TemperatureControlledPower
    !------------------------------------------------------------------------------ 
    !       Compute and print allocation time:
    !------------------------------------------------------------------------------
            alloctime = CPUTime() - alloctime
            allocrealtime = RealTime() - allocrealtime
            WRITE(Message,'(a,F8.2,F8.2)') 'Allocation time (CPU,REAL): (s)', alloctime,allocrealtime
            CALL Info('NumaHeatSolve',Message,Level=4 )
            END SUBROUTINE Allocations
END SUBROUTINE BulkAssembly
