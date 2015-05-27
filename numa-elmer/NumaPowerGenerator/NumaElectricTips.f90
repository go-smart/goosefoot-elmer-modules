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
MODULE NumaElectricTips
    USE DefUtils

    IMPLICIT NONE

    TYPE PointSet_t
        INTEGER :: nbtips
        CHARACTER(LEN=100) :: suffix
        REAL(KIND=dp) :: tipsum, ratio
        REAL(KIND=dp), ALLOCATABLE :: xTip(:), yTip(:), zTip(:), coeffTip(:), coordinatesBasis(:,:)
        INTEGER, ALLOCATABLE :: elements(:), controlledByThermocouple(:)
    END TYPE

    LOGICAL, SAVE :: NonUniformElectricPower = .FALSE., GaussianCalculated = .FALSE.
    INTEGER :: NSUFFICES
    PARAMETER (NSUFFICES=2)
    TYPE(PointSet_t), TARGET, DIMENSION(NSUFFICES), SAVE :: pointsets
    TYPE(PointSet_t), TARGET, SAVE :: thermocouples
    REAL(KIND=dp), SAVE :: coeffTotal
    REAL(KIND=dp), POINTER, SAVE :: Gaussian(:)
    LOGICAL, POINTER, SAVE :: ThermocoupleStates(:)
    LOGICAL, ALLOCATABLE, SAVE :: thermocouplesPreviousState

  CONTAINS
    SUBROUTINE AddGaussianVariable(Solver)
        TYPE(Solver_t) :: Solver
        INTEGER :: LocalNodes

        IF (.NOT. ASSOCIATED(Gaussian)) THEN
            LocalNodes = COUNT(Solver % Variable % Perm > 0)
            ALLOCATE(Gaussian(LocalNodes))
            Gaussian = 0.0_dp
            CALL VariableAdd(Solver % Mesh % Variables, Solver % Mesh, Solver, &
                'Electric Distribution', 1, Gaussian)
        END IF
    END SUBROUTINE

    FUNCTION GetThermocoupleTemperatures(Temperature, Model, Solver) RESULT(TemperaturePtr)
        TYPE(Variable_t) :: Temperature
        TYPE(Model_t) :: Model
        TYPE(Solver_t) :: Solver
        REAL(KIND=dp), POINTER :: TemperaturePtr(:)

        REAL(KIND=dp), TARGET, ALLOCATABLE :: ThermocoupleTemperatures(:)
        TYPE(Element_t), POINTER :: CurrentElement
        INTEGER, POINTER :: NodeIndexes(:)
        LOGICAL :: AllocationsDone = .FALSE.
        INTEGER :: ierr, n, maxn, i
        REAL(KIND=dp) :: ParVal
        REAL(KIND=dp), ALLOCATABLE :: ElementValues(:)

        SAVE ThermocoupleTemperatures, ElementValues, AllocationsDone

        CALL ReadElectricTips(Model, Solver)

        IF (.NOT. AllocationsDone) THEN
            maxn = Solver % Mesh % MaxElementNodes

            ALLOCATE( &
                ThermocoupleTemperatures(thermocouples % nbtips), &
                ThermocoupleStates(thermocouples % nbtips), &
                ElementValues(maxn) &
            )

            ThermocoupleStates(:) = .TRUE.

            AllocationsDone = .TRUE.
        END IF

        DO i = 1, thermocouples % nbtips
            IF (thermocouples % elements(i) < 0) THEN
                ThermocoupleTemperatures(i) = -1000  ! Below absolute zero, even in Celsius
            ELSE
                CurrentElement => Solver % Mesh % Elements(thermocouples % elements(i))
                n = CurrentElement % TYPE % NumberOfNodes
                NodeIndexes => CurrentElement % NodeIndexes

                IF (ALL(Temperature % Perm(NodeIndexes(1:n)) > 0)) THEN
                    ElementValues(1:n) = Temperature % Values(Temperature % Perm(NodeIndexes(1:n)))
                    ThermocoupleTemperatures(i) = SUM(thermocouples % coordinatesBasis(i,1:n) * ElementValues(1:n))
                END IF

                IF (ParEnv % PEs > 1) THEN
                    CALL MPI_ALLREDUCE(ThermocoupleTemperatures(i), ParVal, 1, &
                        MPI_DOUBLE_PRECISION, MPI_MAX, MPI_COMM_WORLD, ierr)
                    ThermocoupleTemperatures(i) = ParVal
                END IF
            END IF
        END DO

        TemperaturePtr => ThermocoupleTemperatures
    END FUNCTION

    SUBROUTINE ReadElectricTips(Model, Solver)
        TYPE(Model_t) :: Model
        TYPE(Solver_t) :: Solver

        INTEGER :: dim, k, i, ios, istat,NbTipsLocationTimes,TipsLocationTimesIndex=-1, &
            OldTipsLocationTimesIndex, n, l, controlledByThermocouple
        REAL(KIND=dp), ALLOCATABLE :: TipsLocationTimesArray(:,:), ElementValues(:)
        REAL(KIND=dp), POINTER :: ParPtr(:,:)
        CHARACTER :: line*100, str1*10, str2*10, TipsFile*200,Char_TipsLocationTimesIndex*100
        CHARACTER(LEN=99), DIMENSION(NSUFFICES) :: suffix = [CHARACTER(LEN=99) :: "ends", "middles" ]
        REAL(KIND=dp) :: Time, tipsum(NSUFFICES) = (/ 10.0, 11.0 /), tipratio(NSUFFICES) = (/ 1.0, 1.5 /), &
            TempCoordinates(3), ParallelHits, MinDist, Dist, LocalCoords(3)
        TYPE(Variable_t), POINTER :: PhaseVar, TimeVar
        TYPE(Nodes_t) :: ElementNodes
        TYPE(Element_t), POINTER :: CurrentElement
        TYPE(Mesh_t), POINTER :: Mesh
        INTEGER, POINTER :: NodeIndexes(:)
        LOGICAL :: Hit, AllocationsDone = .FALSE., contributes = .FALSE.

        LOGICAL :: FirstTime = .TRUE., Found, MultiTipsLocation = .FALSE., NonUniformElectricPower_
        TYPE(PointSet_t), POINTER :: ps

        SAVE FirstTime, TipsLocationTimesIndex, AllocationsDone, ElementValues, ElementNodes

        Mesh => Solver % Mesh

        n = Mesh % MaxElementNodes
        IF (.NOT. AllocationsDone) THEN
            ALLOCATE( &
                ElementValues(n), &
                ElementNodes % x(n), &
                ElementNodes % y(n), &
                ElementNodes % z(n) &
            )
            AllocationsDone = .TRUE.
        END IF

        !------------------------------------------------------------------------------ 
        ! Check if the electric tips location change over time:
        !------------------------------------------------------------------------------
        MultiTipsLocation = GetLogical( Model % Simulation, &
            'Multi Electric Tips Location',Found )
        !------------------------------------------------------------------------------
        IF(MultiTipsLocation) THEN
        !------------------------------------------------------------------------------
            PhaseVar => VariableGet(Solver % Mesh % Variables, "Phase", Found)
            OldTipsLocationTimesIndex = TipsLocationTimesIndex
            IF (Found) THEN
                TipsLocationTimesIndex = FLOOR(PhaseVar % Values(1) + 0.5)
            ELSE
                ! Get the number of different locations:
                !------------------------------------------------------------------------------
                NbTipsLocationTimes = GetInteger( Model % Simulation, &
                    'Electric Tips Location Times Nb', Found )
                !TODO check that this has fixed the unallocated read valgrind shows on 1233
                IF (.NOT. Found) THEN
                    NbTipsLocationTimes = 1
                END IF
                !------------------------------------------------------------------------------
                ! Get the times at which locations change:
                !------------------------------------------------------------------------------
                ALLOCATE(TipsLocationTimesArray(NbTipsLocationTimes,1),STAT=istat)
                ParPtr => ListGetConstRealArray( Model % Simulation, &
                    'Multi Electric Tips Location Times', Found )
                TipsLocationTimesArray = ParPtr
                !------------------------------------------------------------------------------
                ! Check if a new location has to be considered:
                !------------------------------------------------------------------------------
                TimeVar => VariableGet(Model % Variables, "Time")
                Time = TimeVar % Values(1)
                DO WHILE(TipsLocationTimesIndex<=NbTipsLocationTimes .AND. Time>TipsLocationTimesArray(TipsLocationTimesIndex,1))
                    TipsLocationTimesIndex = TipsLocationTimesIndex + 1
                END DO
                !------------------------------------------------------------------------------
                DEALLOCATE(TipsLocationTimesArray)
            END IF
            IF(OldTipsLocationTimesIndex /= TipsLocationTimesIndex) THEN
                FirstTime=.TRUE.
            END IF
        !------------------------------------------------------------------------------
        END IF

        !------------------------------------------------------------------------------ 
        ! Check if the electric power is constant over the tips
        !------------------------------------------------------------------------------
        NonUniformElectricPower = GetLogical( Model % Simulation, &
            'Non Uniform Electric Power',Found )
        IF (.NOT. Found) NonUniformElectricPower = .FALSE.

        !------------------------------------------------------------------------------ 
        ! If first time, read the file and fill the arrays of sources:
        !------------------------------------------------------------------------------
        IF(FirstTime) THEN
        !------------------------------------------------------------------------------     
            DO k = 1, NSUFFICES
                pointsets(k) % suffix = suffix(k)
                pointsets(k) % tipsum = tipsum(k)
                pointsets(k) % ratio = tipratio(k)
            END DO

            thermocouples % suffix = "thermocouples"
            coeffTotal = 0.0_dp

            DO k=1, NSUFFICES + 1
                IF (k <= NSUFFICES) THEN
                    ps => pointsets(k)
                    NonUniformElectricPower_ = NonUniformElectricPower
                    contributes = .TRUE.
                ELSE
                    ps => thermocouples
                    NonUniformElectricPower_ = .FALSE.
                    contributes = .FALSE.
                END IF

                !   Open the Tips file:
                !------------------------------------------------------------------------------
                TipsFile = GetString( Model % Simulation,'Electric Tips Filename Root',Found )

                !------------------------------------------------------------------------------ 
                ! If variable location, add the right extension to file name:
                !------------------------------------------------------------------------------ 
                IF(MultiTipsLocation) THEN 
                    WRITE(Char_TipsLocationTimesIndex,*) MAX(TipsLocationTimesIndex-1,1)
                    Char_TipsLocationTimesIndex = ADJUSTL(Char_TipsLocationTimesIndex)
                    TipsFile = TRIM(TipsFile) // "_"
                    TipsFile = TRIM(TipsFile) // Char_TipsLocationTimesIndex                
                END IF

                IF (Found) THEN
                !------------------------------------------------------------------------------ 
                    TipsFile = TRIM(TipsFile) // "-" // TRIM(ps % suffix) // ".txt"
                    OPEN(UNIT=10,FILE=TipsFile,STATUS='OLD',ACTION='READ',IOSTAT=ios)
                    !------------------------------------------------------------------------------ 
                    IF(ios/=0) THEN
                        PRINT*,'Could not open file ',TipsFile
                        PRINT*,'I/O Fortran Error number ',ios
                        CALL Fatal( 'NumaReadElectricTips', 'Unable to load electric ' // TRIM(ps % suffix) // '.txt tips file' )
                    ELSE
                        !------------------------------------------------------------------------------ 
                        !   Read the number of points and allocate the arrays:
                        !------------------------------------------------------------------------------ 
                        READ(10,*,END=1) line
                        READ(10,*,END=1) str1, ps % nbtips, str2
                        IF ( .NOT. ALLOCATED( ps % xTip ) ) THEN
                            ALLOCATE( &
                              ps % xTip(ps % nbtips), &
                              ps % yTip(ps % nbtips), &
                              ps % zTip(ps % nbtips), &
                              ps % controlledByThermocouple(ps % nbtips), &
                              STAT=istat &
                            )
                            ALLOCATE(ps % coeffTip(ps % nbtips))
                        END IF
                        !------------------------------------------------------------------------------
                        ps % coeffTip = 1.0_dp

                        DO i=1,ps % nbtips
                            !------------------------------------------------------------------------------
                            !       Read the coordinates:
                            !------------------------------------------------------------------------------
                            IF (NonUniformElectricPower_) THEN
                                READ(10,*,END=1) ps % xTip(i), ps % yTip(i), ps % zTip(i), ps % coeffTip(i), &
                                    ps % controlledByThermocouple(i)
                            ELSE
                                READ(10,*,END=1) ps % xTip(i), ps % yTip(i), ps % zTip(i), &
                                    ps % controlledByThermocouple(i)
                            END IF

                            !IF (controlledByThermocouple > 0) THEN
                            !    ps % controlledByThermocouple(i) = controlledByThermocouple
                            !END IF
                        !------------------------------------------------------------------------------
                        END DO

                        IF (contributes) THEN
                            coeffTotal = coeffTotal + SUM(ps % coeffTip)
                        END IF

                        !------------------------------------------------------------------------------
                        ! Check that the non uniform electric power is set correctly:
                        !------------------------------------------------------------------------------
                        IF (NonUniformElectricPower_) THEN
                            !IF (SUM(ps % coeffTip)/= ps % tipsum) THEN
                            !    PRINT*,'Check coefficients for non uniform electric power for ' // TRIM(ps % suffix) // &
                            !        ' points! Value 1 set everywhere.'
                            !    ps % coeffTip = 1.0
                            !ELSE
                                PRINT*,'Non uniform electric power at ' // TRIM(ps % suffix) // ' points:'
                                DO i=1,ps % nbtips
                                    PRINT*,'Point ',i,': coeff=', ps % coeffTip(i)
                                END DO
                            !END IF
                        END IF
                        !------------------------------------------------------------------------------ 
                        !   Close the Tips file:
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
                    CALL Info('NumaReadElectricTips', &
                        'Please specify electric tips file name root in input file.', Level=1 )
                    CALL Fatal( 'NumaReadElectricTips', 'Unable to load electric ' // TRIM(ps % suffix) // ' tips file' )
                !------------------------------------------------------------------------------
                END IF ! name of the tips file found
                !------------------------------------------------------------------------------
            END DO

            ps => thermocouples
            IF ( .NOT. ALLOCATED( ps % elements ) ) THEN
                ALLOCATE( &
                  ps % elements(ps % nbtips), &
                  ps % coordinatesBasis(ps % nbtips, n) &
                )
            END IF

            ! Calculate necessary variables for evaluating thermocouple temperatures
            DO i = 1, ps % nbtips
                ps % coordinatesBasis(i,:) = 0.0_dp

                Hit = .FALSE.

                TempCoordinates(1) = ps % xTip(i)
                TempCoordinates(2) = ps % yTip(i)
                TempCoordinates(3) = ps % zTip(i)

                MinDist = HUGE(MinDist)

                l = 0

                DO k = 1, Mesh % NumberOfBulkElements
                    CurrentElement => Mesh % Elements(k)
                    n = CurrentElement % TYPE % NumberOfNodes
                    NodeIndexes => CurrentElement % NodeIndexes

                    ElementNodes % x(1:n) = Mesh % Nodes % x(NodeIndexes)
                    ElementNodes % y(1:n) = Mesh % Nodes % y(NodeIndexes)
                    ElementNodes % z(1:n) = Mesh % Nodes % z(NodeIndexes)

                    Hit = PointInElement(CurrentElement, ElementNodes, TempCoordinates, &
                        LocalCoords, LocalDistance=Dist)

                    IF (Dist < MinDist) THEN
                        MinDist = Dist
                        l = k
                    END IF

                    IF ( Hit ) THEN
                        EXIT
                    END IF
                END DO

                IF (Hit) THEN
                    ParallelHits = 1.0_dp
                ELSE
                    ParallelHits = 0.0_dp
                END IF

                ParallelHits = ParallelReduction(ParallelHits)

                IF (ParallelHits < 0.5_dp) THEN
                    ! This thermocouple is not in an element. We take that as indication that it is in a blood vessel or outside the
                    ! organ domain, and should be disregarded.
                    ps % elements(i) = -1
                ELSE
                    ps % elements(i) = k

                    ElementValues = 0.0_dp
                    DO l = 1, n
                        ElementValues(l) = 1.0d0
                        ps % coordinatesBasis(i, l) = InterpolateInElement(CurrentElement, ElementValues, &
                            LocalCoords(1), LocalCoords(2), LocalCoords(3))
                        ElementValues(l) = 0.0d0
                    END DO
                END IF
            END DO
            FirstTime = .FALSE.
            !------------------------------------------------------------------------------ 
        END IF ! FirstTime
    !------------------------------------------------------------------------------ 
    END SUBROUTINE ReadElectricTips

    FUNCTION GaussianTipDistribution(Solver, model, n) RESULT(Source)
        USE DefUtils
        IMPLICIT None

        INTEGER :: n
        TYPE(Solver_t) :: Solver
        TYPE(Model_t) :: model
        REAL(KIND=dp) :: Source

        TYPE(Variable_t), POINTER :: ElectricDistributionVar

        IF (.NOT. GaussianCalculated) THEN
            CALL GaussianTipCheckCalculation(Model)
        END IF

        ElectricDistributionVar => VariableGet(Solver % Mesh % Variables, "Electric Distribution")
        Source = Gaussian(ElectricDistributionVar % Perm(n))

    END FUNCTION

    SUBROUTINE GaussianInvalidate()
        GaussianCalculated = .FALSE.
    END SUBROUTINE

    SUBROUTINE UpdateThermocoupleHeatingState(i, tipState)
        INTEGER :: i
        LOGICAL :: tipState
        CALL GaussianInvalidate()

        ThermocoupleStates(i) = tipState

    END SUBROUTINE

    FUNCTION GetThermocoupleHeatingState(i) RESULT(tipState)
        INTEGER :: i
        LOGICAL :: tipState

        tipState = ThermocoupleStates(i)

    END FUNCTION

    !------------------------------------------------------------------------------
    ! Read Electric Tips coordinates in a file for potential solver or for 
    ! electric power in heat solver
    !------------------------------------------------------------------------------
    SUBROUTINE GaussianTipCheckCalculation(Model)
    !------------------------------------------------------------------------------ 
        USE DefUtils
        IMPLICIT None
    !------------------------------------------------------------------------------   
        TYPE(Model_t) :: model
    !------------------------------------------------------------------------------   
        INTEGER :: n
        !TYPE(ValueList_t), POINTER :: Material
        TYPE(Solver_t), POINTER :: Solver
        TYPE(Variable_t), POINTER :: ElectricDistributionVar
        INTEGER :: k, dim,i, nbtips, ios, istat,NbTipsLocationTimes,TipsLocationTimesIndex=1, &
            OldTipsLocationTimesIndex, LocalNodes
        REAL(KIND=dp) :: x, y, z, pi32, sigma, coeff, Source_endtips, Source_middletips, &
            Temp, TempCutOff, CutOffCoefficient, TempCutOffGradient, Source !, Density
        REAL(KIND=dp),ALLOCATABLE :: xTip(:), yTip(:), zTip(:), coeffTip(:)
        REAL(KIND=dp),ALLOCATABLE :: TipsLocationTimesArray(:,:)
        CHARACTER :: line*100, str1*10, str2*10, TipsFile*100,Char_TipsLocationTimesIndex*100
        LOGICAL :: FirstTime = .TRUE., Found, MultiPower = .FALSE., &
            InterpolatedLineSource = .FALSE., MultiTipsLocation = .FALSE., CheckRecalculationRequired = .FALSE.
        TYPE(PointSet_t), POINTER :: ps
        INTEGER, POINTER :: Perm(:)

        SAVE FirstTime,TipsLocationTimesIndex, coeffTip !, Density

        Solver => GetSolver()
        Perm => Solver % Variable % Perm

    !------------------------------------------------------------------------------ 
    ! Check if we use an interpolated line source:
    !------------------------------------------------------------------------------
        InterpolatedLineSource = GetLogical( Model % Simulation, &
                'Interpolated Electric Power',Found )
        IF (.NOT. Found) InterpolatedLineSource = .FALSE.
        IF (InterpolatedLineSource) THEN
            Source = 0.0D0
            RETURN
        END IF

        dim = CoordinateSystemDimension()

        CALL ReadElectricTips(Model, Solver)

        !------------------------------------------------------------------------------ 
        ! Define gaussian parameters and flow:
        !------------------------------------------------------------------------------ 
        sigma = GetConstReal( Model % Simulation,'Gaussian Sigma Electric',Found )
        IF (.NOT. Found) sigma = 0.0030
        pi32 = exp(3.0*log(2.0*Pi)/2.0) 
        coeff = GetConstReal( Model % Simulation,'Gaussian Mult Coefficient Electric',Found )
        IF (.NOT. Found) coeff = 1!NumaReadElectricPower(model, n, time)
        !------------------------------------------------------------------------------ 
        ! Check if the electric power is constant over the tips
        !------------------------------------------------------------------------------
        NonUniformElectricPower = GetLogical( Model % Simulation, &
            'Non Uniform Electric Power',Found )
        IF (.NOT. Found) NonUniformElectricPower = .FALSE.

        ElectricDistributionVar => VariableGet(Model % Mesh % Variables, "electric distribution")

        Gaussian => ElectricDistributionVar % Values
        Gaussian = 0.0_dp
        DO n = 1, Model % NumberOfNodes
            !Material => GetMaterial(Element)
            !Density = ListGetRealAtNode(Material, "Density", n, Found)

            !IF (.NOT. Found) THEN
            !   CALL Fatal( 'NumaReadElectricTips', 'Unable to find Density')
            !END IF

            IF (Perm(n) <= 0) CYCLE

            x = Model % Nodes % x(n)
            y = Model % Nodes % y(n)
            IF (dim<3) THEN
                z = 0.0
            ELSE
                z = Model % Nodes % z(n)
            END IF

            Source = 0.0

            DO k=1, NSUFFICES
                ps => pointsets(k)
                DO i=1, ps % nbtips
                  IF (ps % controlledByThermocouple(i) > 0 .AND. .NOT. ThermocoupleStates(ps % controlledByThermocouple(i))) CYCLE
                  !------------------------------------------------------------------------------ 
                  ! ONLY IMPLEMENTED FOR SPECIAL CASE OF 9 ELECTRIC TIPS!!!!
                  !------------------------------------------------------------------------------ 
                  IF(DIM==3) THEN
                  !------------------------------------------------------------------------------
                      IF (NonUniformElectricPower) THEN
                          !Source = Source + coeff*3.5/(57.0*sigma*sigma*sigma*pi32) * &
                          !    coefftip(i)*&
                          !    exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i))+(z-ztip(i))*(z-ztip(i)))/(2*sigma*sigma))
                          Source = Source + (ps % ratio / coeffTotal) * coeff*(sigma*SQRT(2*Pi))**(-3.0) * &
                              ps % coeffTip(i)*exp(-((x-ps % xTip(i))*(x-ps % xTip(i))+ &
                              (y-ps % yTip(i))*(y-ps % yTip(i))+(z-ps % zTip(i))*(z-ps % zTip(i)))/(2*sigma*sigma))
                      ELSE
                          !Source = Source + coeff*2.0/(57.0*sigma*sigma*sigma*pi32) * &
                          !    exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i))+(z-ztip(i))*(z-ztip(i)))/(2*sigma*sigma))
                          Source = Source + (ps % ratio / coeffTotal) * coeff*(sigma*SQRT(2*Pi))**(-3.0) * &
                              exp(-((x-ps % xTip(i))*(x-ps % xTip(i))+(y-ps % yTip(i))*(y-ps % yTip(i))+&
                              (z-ps % zTip(i))*(z-ps % zTip(i)))/(2*sigma*sigma))
                      END IF
                  !------------------------------------------------------------------------------
                  ELSE
                  !------------------------------------------------------------------------------
                      Source = Source + coeff*(ps % ratio)/(6.5*sigma*sigma*2*Pi) * &
                          exp(-((x-ps % xTip(i))*(x-ps % xTip(i))+(y-ps % yTip(i))*(y-ps % yTip(i)))/(2*sigma*sigma))

                      !Source = Source + coeff*1.5/(7.0*sigma*sigma*2*Pi) * &
                      !   exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i)))/(2*sigma*sigma))
                  !------------------------------------------------------------------------------
                  END IF
                  !------------------------------------------------------------------------------
                END DO
            END DO

            Gaussian(Perm(n)) = Source ! / Density  ! Per unit mass, rather than volume

        END DO

        GaussianCalculated = .TRUE.
    !------------------------------------------------------------------------------
    END SUBROUTINE GaussianTipCheckCalculation
END MODULE NumaElectricTips
