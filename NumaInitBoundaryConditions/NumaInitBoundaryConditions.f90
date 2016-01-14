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
!------------------------------------------------------------------------------
!
!   Definition of time/space dependent Boundary Conditions, 
!   Initial Conditions, and source terms
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! Read Perfusion Map in a file for temperature solver 
!------------------------------------------------------------------------------
FUNCTION NumaReadPerfusionMap(model, n, time) RESULT( Perfusion )
!------------------------------------------------------------------------------ 
    USE DefUtils
    IMPLICIT None
!------------------------------------------------------------------------------   
    TYPE(Model_t) :: model
    INTEGER :: n
    REAL(KIND=dp) :: Perfusion,time
!------------------------------------------------------------------------------   
    INTEGER :: dim,i, nbPerf, ios, istat
    REAL(KIND=dp) :: x, y, z, pi32
    REAL(KIND=dp),ALLOCATABLE :: xPerf(:), yPerf(:), zPerf(:),sigmaPerf(:),coeffPerf(:)
    CHARACTER :: line*100, str1*10, str2*10, PerfusionMapFile*100
    LOGICAL :: FirstTime = .TRUE., Found

    SAVE FirstTime,xPerf,yPerf,zPerf,nbPerf,sigmaPerf,coeffPerf
    !------------------------------------------------------------------------------
    ! Get the spatial coordinates:
    !------------------------------------------------------------------------------
    dim = CoordinateSystemDimension()
    x = model % Nodes % x(n)
    y = model % Nodes % y(n)
    z = 0.0
    IF (dim==3) z = model % Nodes % z(n)
    !------------------------------------------------------------------------------ 
    ! Define gaussian parameters:
    !------------------------------------------------------------------------------ 
    pi32 = exp(3.0*log(2.0*Pi)/2.0) 
    !------------------------------------------------------------------------------ 
    ! If first time, read the file and fill the arrays of sources:
    !------------------------------------------------------------------------------
    IF(FirstTime) THEN
    !------------------------------------------------------------------------------     
        ! Open the Tips file:
        !------------------------------------------------------------------------------  
        PerfusionMapFile = GetString( Model % Simulation,'Perfusion Map Filename',Found )   
        !------------------------------------------------------------------------------ 
        IF (Found) THEN
        !------------------------------------------------------------------------------  
            PerfusionMapFile = TRIM(PerfusionMapFile) // ".txt"
            OPEN(UNIT=10,FILE=PerfusionMapFile,STATUS='OLD',ACTION='READ',IOSTAT=ios)
            !------------------------------------------------------------------------------ 
            IF(ios/=0) THEN
                PRINT*,'Could not open file ',PerfusionMapFile
                PRINT*,'I/O Fortran Error number ',ios
                CALL Fatal( 'NumaReadPerfusionMap', 'Unable to load perfusion map file' )
            ELSE
                !------------------------------------------------------------------------------ 
                !   Read the number of points and allocate the arrays:
                !------------------------------------------------------------------------------ 
                READ(10,*,END=1) line
                READ(10,*,END=1) str1, nbPerf, str2
                IF ( .NOT. ALLOCATED( xPerf ) ) THEN
                    ALLOCATE(xPerf(nbPerf),yPerf(nbPerf),zPerf(nbPerf), &
                        sigmaPerf(nbPerf),coeffPerf(nbPerf),STAT=istat)
                END IF
                !------------------------------------------------------------------------------
                DO i=1,nbPerf   
                    !------------------------------------------------------------------------------
                    ! Read the coordinates:
                    !------------------------------------------------------------------------------
                    READ(10,*,END=1) xPerf(i), yPerf(i), zPerf(i), sigmaPerf(i), coeffPerf(i)
                !------------------------------------------------------------------------------
                END DO
                !------------------------------------------------------------------------------ 
                !   Close the Perfusion Map file:
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
            CALL Info('NumaReadPerfusionMap', &
                'Please specify perfusion map file name root in input file.', Level=1 )
            CALL Fatal( 'NumaReadPerfusionMap', 'Unable to load perfusion map file' )
        !------------------------------------------------------------------------------
        END IF ! name of the perfusion map file found
        !------------------------------------------------------------------------------
        FirstTime = .FALSE.
        !------------------------------------------------------------------------------ 
    END IF ! FirstTime
    !------------------------------------------------------------------------------ 
    ! Compute the contribution of all points:
    !------------------------------------------------------------------------------ 
    Perfusion = GetConstReal( Model % Simulation,'Average Tissue Perfusion Coefficient',Found )
    IF (.NOT. Found) Perfusion = 0.0
    !------------------------------------------------------------------------------
    DO i=1,nbPerf   
    !------------------------------------------------------------------------------ 
        IF(DIM==3) THEN
            !------------------------------------------------------------------------------ 
            IF (sigmaPerf(i)>0.0) THEN !Gaussian variation
            !------------------------------------------------------------------------------ 
                Perfusion = Perfusion + &
                    coeffPerf(i) * exp(-((x-xPerf(i))*(x-xPerf(i))+ &
                    (y-yPerf(i))*(y-yPerf(i))+ (z-zPerf(i))*(z-zPerf(i))) &
                    /(2*sigmaPerf(i)*sigmaPerf(i)))
            !------------------------------------------------------------------------------ 
            ELSE !Sharp variation
            !------------------------------------------------------------------------------ 
                IF (sqrt((x-xPerf(i))*(x-xPerf(i))+(y-yPerf(i))*(y-yPerf(i))+(z-zPerf(i))*(z-zPerf(i)))<-sigmaPerf(i)) THEN
                    Perfusion = Perfusion + coeffPerf(i) 
                END IF
            !------------------------------------------------------------------------------ 
            END IF
            !------------------------------------------------------------------------------ 
        ELSE
            !------------------------------------------------------------------------------ 
            IF (sigmaPerf(i)>0.0) THEN !Gaussian variation
            !------------------------------------------------------------------------------
                Perfusion = Perfusion + &
                    coeffPerf(i) * exp(-((x-xPerf(i))*(x-xPerf(i))+ (y-yPerf(i))*(y-yPerf(i))) &
                    /(2*sigmaPerf(i)*sigmaPerf(i)))
            !------------------------------------------------------------------------------ 
            ELSE !Sharp variation
            !------------------------------------------------------------------------------ 
                IF (sqrt((x-xPerf(i))*(x-xPerf(i))+(y-yPerf(i))*(y-yPerf(i)))<-sigmaPerf(i)) THEN
                    Perfusion = Perfusion + coeffPerf(i) 
                END IF
            !------------------------------------------------------------------------------ 
            END IF
            !------------------------------------------------------------------------------ 
        END IF
    !------------------------------------------------------------------------------
    END DO
!------------------------------------------------------------------------------ 
END FUNCTION NumaReadPerfusionMap
!------------------------------------------------------------------------------
















!------------------------------------------------------------------------------
! Read the values of electric power over time in a text file (the first time 
! only), save these values and interpolate at each timestep 
!------------------------------------------------------------------------------
FUNCTION NumaReadElectricPower(model, n, time) RESULT( Power )
!------------------------------------------------------------------------------ 
    USE DefUtils
  USE GeneralUtils
    IMPLICIT None
!------------------------------------------------------------------------------   
    TYPE(Model_t) :: model
    INTEGER :: n
    REAL(KIND=dp) :: Power,time
!------------------------------------------------------------------------------   
    INTEGER NbValues,i, ios, istat
    REAL(KIND=dp),ALLOCATABLE :: TimeArray(:), PowerArray(:)
    CHARACTER :: line*100, PowerFile*100
    LOGICAL :: FirstTime = .TRUE., Found, Echo = .FALSE.

    !TODO:NUMA:If my reading of >=F90 is right, since NbValues is not
    !initialized, it won't be saved unless included below and was only retaining
    !its original value by coincidence - is that true? My tests suggest this is
    !the case, as it seemed to occasionally modify depending on when and where
    !NREP was called
    SAVE NbValues,FirstTime,TimeArray,PowerArray
    !------------------------------------------------------------------------------ 
    ! If first time, read the file and fill the arrays of time/power:
    !------------------------------------------------------------------------------
    IF(FirstTime) THEN
    !------------------------------------------------------------------------------     
        !   Open the Power file:
        !------------------------------------------------------------------------------  
        PowerFile = GetString( Model % Simulation,'Electric Power Filename',Found ) 
        !------------------------------------------------------------------------------ 
        IF (Found) THEN
        !------------------------------------------------------------------------------ 
            PowerFile = TRIM(PowerFile) // ".txt" 

            OPEN(UNIT=10,FILE=PowerFile,STATUS='OLD',ACTION='READ',IOSTAT=ios)
            !------------------------------------------------------------------------------ 
            IF(ios/=0) THEN
                PRINT*,'Could not open file ',PowerFile
                PRINT*,'I/O Fortran Error number ',ios
                CALL Fatal( 'NumaReadElectricPower', 'Unable to load electric power file' )
            ELSE
                !------------------------------------------------------------------------------ 
                !   Get the number of values and allocate the arrays:
                !------------------------------------------------------------------------------ 
                NbValues = -1
                
                DO WHILE( ReadAndTrim( 10,line,Echo ) )
                    IF (line=='begin') THEN
                        NbValues = 0
                        DO WHILE( ReadAndTrim( 10,line,Echo ) )
                            IF (Line==' ')  CYCLE
                            IF (Line=='end')  EXIT
                            NbValues = NbValues + 1
                        END DO
                    END IF
                END DO
                IF (NbValues==-1) THEN
                    CALL Fatal( 'NumaReadElectricPower', 'Please check the format of file '// &
                            PowerFile // 'Must be [Begin//Time Power//..//Time Power//End].' )
                ELSE IF (NbValues==0) THEN
                    CALL Fatal('NumaReadElectricPower','No values found in file '//PowerFile )
                END IF
                !------------------------------------------------------------------------------ 
                !   Restart reading at the beginning of the file:
                !------------------------------------------------------------------------------
                REWIND (UNIT=10,IOSTAT=ios)
                !------------------------------------------------------------------------------ 
                !   Allocate the arrays:
                !------------------------------------------------------------------------------
                IF ( .NOT. ALLOCATED( TimeArray ) ) THEN
                    ALLOCATE(TimeArray(NbValues),PowerArray(NbValues),STAT=istat)
                END IF

                READ(10,*) line
                !------------------------------------------------------------------------------
                DO i=1,NbValues 
                    !------------------------------------------------------------------------------
                    !       Read the values of time amd power:
                    !------------------------------------------------------------------------------
                    READ(10,*,END=1) TimeArray(i), PowerArray(i)
                !------------------------------------------------------------------------------
                END DO
                !------------------------------------------------------------------------------ 
                !   Close the Power file:
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
            CALL Info('NumaReadElectricPower', &
                'Please specify electric power file name root in input file.', Level=1 )
            CALL Info( 'NumaReadElectricPower', 'Unable to load electric power file' )
            Power = 1.0
            RETURN
        !------------------------------------------------------------------------------
        END IF ! name of the power file found
        !------------------------------------------------------------------------------
        FirstTime = .FALSE.
        !------------------------------------------------------------------------------ 
    END IF ! FirstTime
    !------------------------------------------------------------------------------ 
    ! Compute the power by interpolation:
    !------------------------------------------------------------------------------ 
    Power = 0.0
    IF (NbValues>0) Power = InterpolateCurve( TimeArray,PowerArray,time )
    !WRITE( Message, * ) 'Invalid value of Beta ', Power, time !RMV                                                                                                                                                                                                                                                                                                                         
    !CALL Info('NumaHeatSolve',Message,Level=4 )
!------------------------------------------------------------------------------ 
END FUNCTION NumaReadElectricPower
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
! Read Electric Tips coordinates in a file for potential solver or for 
! electric power in heat solver
!------------------------------------------------------------------------------
FUNCTION NumaReadElectricTipsVTK(model, n, time ) RESULT( Source )
!------------------------------------------------------------------------------ 
    USE DefUtils
    IMPLICIT None
!------------------------------------------------------------------------------   
    TYPE(Model_t) :: model
    INTEGER :: n
    REAL(KIND=dp) :: time,Source
!------------------------------------------------------------------------------   
    INTEGER :: dim,i, nbtips, ios, istat,NbTipsLocationTimes,TipsLocationTimesIndex=1, &
        OldTipsLocationTimesIndex
    REAL(KIND=dp) :: x, y, z, pi32, sigma, coeff, Source_endtips, Source_middletips
    REAL(KIND=dp),ALLOCATABLE :: xTip(:), yTip(:), zTip(:)
    REAL(KIND=dp),ALLOCATABLE :: TipsLocationTimesArray(:,:)
    CHARACTER :: line*100, str1*10, str2*10, TipsFile*100,Char_TipsLocationTimesIndex*100
    LOGICAL :: FirstTime = .TRUE., Found, MultiPower = .FALSE., &
        InterpolatedLineSource = .FALSE., MultiTipsLocation = .FALSE.

    SAVE FirstTime,xTip,yTip,zTip,nbtips,TipsLocationTimesIndex

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
!------------------------------------------------------------------------------ 
! Check if the electric power is constant over the probe
!------------------------------------------------------------------------------
    MultiPower = GetLogical( Model % Simulation, &
            'Multi Power Electric Tips',Found )
    IF (.NOT. Found) MultiPower = .TRUE.
!------------------------------------------------------------------------------
!   If electric power different at tips ends and middle
!------------------------------------------------------------------------------
    IF (MultiPower) THEN
!------------------------------------------------------------------------------
        Source_endtips = NumaReadEndElectricTipsVTK(model, n, time)
        Source_middletips = NumaReadMiddleElectricTipsVTK(model, n, time)
        Source = Source_endtips + Source_middletips
!------------------------------------------------------------------------------
! If electric power equal at tips ends and middle
!------------------------------------------------------------------------------
    ELSE
!------------------------------------------------------------------------------
    ! Get the spatial coordinates:
    !------------------------------------------------------------------------------
        dim = CoordinateSystemDimension()
        
        x = model % Nodes % x(n)
        y = model % Nodes % y(n)
        IF (dim<3) THEN
            z = 0.0
        ELSE
            z = model % Nodes % z(n)
        END IF
    !------------------------------------------------------------------------------ 
    ! Check if the electric tips location change over time:
    !------------------------------------------------------------------------------
    MultiTipsLocation = GetLogical( Model % Simulation, &
        'Multi Electric Tips Location',Found )
    !------------------------------------------------------------------------------
    IF(MultiTipsLocation) THEN
    !------------------------------------------------------------------------------
        ! Get the number of different locations:
        !------------------------------------------------------------------------------
        NbTipsLocationTimes = GetInteger( Model % Simulation, &
            'Electric Tips Location Times Nb', Found )
        !------------------------------------------------------------------------------
        ! Get the times at which locations change:
        !------------------------------------------------------------------------------
        ALLOCATE(TipsLocationTimesArray(NbTipsLocationTimes,1),STAT=istat)
        TipsLocationTimesArray = ListGetConstRealArray( Model % Simulation, &
            'Multi Electric Tips Location Times', Found )
        !------------------------------------------------------------------------------
        ! Check if a new location has to be considered:
        !------------------------------------------------------------------------------
        OldTipsLocationTimesIndex = TipsLocationTimesIndex
        DO WHILE(Time>TipsLocationTimesArray(TipsLocationTimesIndex,1) .AND. TipsLocationTimesIndex<=NbTipsLocationTimes)
            TipsLocationTimesIndex = TipsLocationTimesIndex + 1
        END DO
        IF(OldTipsLocationTimesIndex /= TipsLocationTimesIndex) THEN
            FirstTime=.TRUE.
        END IF
        !------------------------------------------------------------------------------
        DEALLOCATE(TipsLocationTimesArray)
    !------------------------------------------------------------------------------
    END IF
    !------------------------------------------------------------------------------ 
    ! Define gaussian parameters and flow:
    !------------------------------------------------------------------------------ 
        sigma = GetConstReal( Model % Simulation,'Gaussian Sigma Electric',Found )
        IF (.NOT. Found) sigma = 0.003
        pi32 = exp(3.0*log(2.0*Pi)/2.0) 
        coeff = GetConstReal( Model % Simulation,'Gaussian Mult Coefficient Electric',Found )
        IF (.NOT. Found) coeff = 1.0
    !------------------------------------------------------------------------------ 
    ! If first time, read the file and fill the arrays of sources:
    !------------------------------------------------------------------------------
        IF(FirstTime) THEN
    !------------------------------------------------------------------------------     
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
            !------------------------------------------------------------------------------ 
            END IF
            !------------------------------------------------------------------------------  
            IF (Found) THEN
            !------------------------------------------------------------------------------ 
                TipsFile = TRIM(TipsFile) // ".vtk" 
                OPEN(UNIT=10,FILE=TipsFile,STATUS='OLD',ACTION='READ',IOSTAT=ios)
                !------------------------------------------------------------------------------ 
                IF(ios/=0) THEN
                    PRINT*,'Could not open file ',TipsFile
                    PRINT*,'I/O Fortran Error number ',ios
                    CALL Fatal( 'NumaReadElectricTips', 'Unable to load electric tips file!' )
                ELSE
                    !------------------------------------------------------------------------------ 
                    !   Read the number of points and allocate the arrays:
                    !------------------------------------------------------------------------------ 
                    READ(10,*,END=1) line
                    READ(10,*,END=1) line
                    READ(10,*,END=1) line
                    READ(10,*,END=1) line
                    READ(10,*,END=1) str1, nbtips, str2
                    IF ( .NOT. ALLOCATED( xtip ) ) THEN
                        ALLOCATE(xtip(nbtips),ytip(nbtips),ztip(nbtips),STAT=istat)
                    END IF
                    !------------------------------------------------------------------------------
                    DO i=1,nbtips   
                        !------------------------------------------------------------------------------
                        !       Read the coordinates:
                        !------------------------------------------------------------------------------
                        READ(10,*,END=1) xtip(i), ytip(i), ztip(i)
                    !------------------------------------------------------------------------------
                    END DO
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
                CALL Fatal( 'NumaReadElectricTips', 'Unable to load electric tips file!' )
            !------------------------------------------------------------------------------
            END IF ! name of the tips file found
            !------------------------------------------------------------------------------
            FirstTime = .FALSE.
            !------------------------------------------------------------------------------ 
        END IF ! FirstTime
        !------------------------------------------------------------------------------ 
        ! Compute the contribution of all tips:
        !------------------------------------------------------------------------------ 
        Source = 0.0
        !------------------------------------------------------------------------------
        DO i=1,nbtips   
        !------------------------------------------------------------------------------ 
        ! Smoothed source with gaussian
        !------------------------------------------------------------------------------ 
            IF(DIM==3) THEN
                Source = Source + coeff/(nbtips*sigma*sigma*sigma*pi32) * &
                    exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i))+(z-ztip(i))*(z-ztip(i)))/(2*sigma*sigma))
            ELSE
                Source = Source + coeff/(nbtips*sigma*sigma*2.0*Pi) * &
                    exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i)))/(2*sigma*sigma))
            END IF
        !------------------------------------------------------------------------------ 
        ! Sharp source (constant value on a sphere)
        !------------------------------------------------------------------------------         
            !IF (sqrt((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i))+(z-ztip(i))*(z-ztip(i)))<sigma) THEN
            !   Source = Source + coeff/(nbtips*sigma*sigma*sigma*Pi*4/3) 
            !END IF
        !------------------------------------------------------------------------------
        END DO
    !------------------------------------------------------------------------------
    END IF
    !------------------------------------------------------------------------------ 

    CONTAINS

    !------------------------------------------------------------------------------
    ! Read End Electric Tips coordinates in a file for potential solver or for 
    ! electric power in heat solver
    !------------------------------------------------------------------------------
    FUNCTION NumaReadEndElectricTipsVTK(model, n, time) RESULT( Source )
    !------------------------------------------------------------------------------ 
        USE DefUtils
        IMPLICIT None
    !------------------------------------------------------------------------------   
        TYPE(Model_t) :: model
        INTEGER :: n
        REAL(KIND=dp) :: Source, time
    !------------------------------------------------------------------------------   
        INTEGER :: dim,i, nbtips, ios, istat,NbTipsLocationTimes,TipsLocationTimesIndex=1, &
        OldTipsLocationTimesIndex
        REAL(KIND=dp) :: x, y, z, pi32, sigma, coeff
        REAL(KIND=dp),ALLOCATABLE :: xTip(:), yTip(:), zTip(:)
        REAL(KIND=dp),ALLOCATABLE :: TipsLocationTimesArray(:,:)
        CHARACTER :: line*100, str1*10, str2*10, TipsFile*100,Char_TipsLocationTimesIndex*100
        LOGICAL :: FirstTime = .TRUE., Found, MultiTipsLocation = .FALSE.

        SAVE FirstTime,xTip,yTip,zTip,nbtips,TipsLocationTimesIndex
        !------------------------------------------------------------------------------
        ! Get the spatial coordinates:
        !------------------------------------------------------------------------------
        dim = CoordinateSystemDimension()

        x = model % Nodes % x(n)
        y = model % Nodes % y(n)
        IF (dim<3) THEN
            z = 0.0
        ELSE
            z = model % Nodes % z(n)
        END IF
        !------------------------------------------------------------------------------ 
        ! Check if the electric tips location change over time:
        !------------------------------------------------------------------------------
        MultiTipsLocation = GetLogical( Model % Simulation, &
            'Multi Electric Tips Location',Found )
        !------------------------------------------------------------------------------
        IF(MultiTipsLocation) THEN
        !------------------------------------------------------------------------------
            ! Get the number of different locations:
            !------------------------------------------------------------------------------
            NbTipsLocationTimes = GetInteger( Model % Simulation, &
                'Electric Tips Location Times Nb', Found )
            !------------------------------------------------------------------------------
            ! Get the times at which locations change:
            !------------------------------------------------------------------------------
            ALLOCATE(TipsLocationTimesArray(NbTipsLocationTimes,1),STAT=istat)
            TipsLocationTimesArray = ListGetConstRealArray( Model % Simulation, &
                'Multi Electric Tips Location Times', Found )
            !------------------------------------------------------------------------------
            ! Check if a new location has to be considered:
            !------------------------------------------------------------------------------
            OldTipsLocationTimesIndex = TipsLocationTimesIndex
            DO WHILE(Time>TipsLocationTimesArray(TipsLocationTimesIndex,1) .AND. TipsLocationTimesIndex<=NbTipsLocationTimes)
                TipsLocationTimesIndex = TipsLocationTimesIndex + 1
            END DO
            IF(OldTipsLocationTimesIndex /= TipsLocationTimesIndex) THEN
                FirstTime=.TRUE.
            END IF
            !------------------------------------------------------------------------------
            DEALLOCATE(TipsLocationTimesArray)
        !------------------------------------------------------------------------------
        END IF
        !------------------------------------------------------------------------------ 
        ! Define gaussian parameters and flow:
        !------------------------------------------------------------------------------ 
        sigma = GetConstReal( Model % Simulation,'Gaussian Sigma Electric',Found )
        IF (.NOT. Found) sigma = .0030
        pi32 = exp(3.0*log(2.0*Pi)/2.0) 
        coeff = GetConstReal( Model % Simulation,'Gaussian Mult Coefficient Electric',Found )
        IF (.NOT. Found) coeff = 1.0
        !------------------------------------------------------------------------------ 
        ! If first time, read the file and fill the arrays of sources:
        !------------------------------------------------------------------------------
        IF(FirstTime) THEN
        !------------------------------------------------------------------------------     
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
            !------------------------------------------------------------------------------ 
            END IF
            !------------------------------------------------------------------------------ 
            IF (Found) THEN
            !------------------------------------------------------------------------------ 
                TipsFile = TRIM(TipsFile) // "-ends.vtk" 
                OPEN(UNIT=10,FILE=TipsFile,STATUS='OLD',ACTION='READ',IOSTAT=ios)
                !------------------------------------------------------------------------------ 
                IF(ios/=0) THEN
                    PRINT*,'Could not open file ',TipsFile
                    PRINT*,'I/O Fortran Error number ',ios
                    CALL Fatal( 'NumaReadElectricTips', 'Unable to load electric end tips file' )
                ELSE
                    !------------------------------------------------------------------------------ 
                    !   Read the number of points and allocate the arrays:
                    !------------------------------------------------------------------------------ 
                    READ(10,*,END=1) line
                    READ(10,*,END=1) line
                    READ(10,*,END=1) line
                    READ(10,*,END=1) line
                    READ(10,*,END=1) str1, nbtips, str2
                    IF ( .NOT. ALLOCATED( xtip ) ) THEN
                        ALLOCATE(xtip(nbtips),ytip(nbtips),ztip(nbtips),STAT=istat)
                    END IF
                    !------------------------------------------------------------------------------
                    DO i=1,nbtips   
                        !------------------------------------------------------------------------------
                        !       Read the coordinates:
                        !------------------------------------------------------------------------------
                        READ(10,*,END=1) xtip(i), ytip(i), ztip(i)
                    !------------------------------------------------------------------------------
                    END DO
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
                CALL Info('NumaReadEndElectricTips', &
                    'Please specify electric tips file name root in input file.', Level=1 )
                CALL Fatal( 'NumaReadEndElectricTips', 'Unable to load electric end tips file' )
            !------------------------------------------------------------------------------
            END IF ! name of the tips file found
            !------------------------------------------------------------------------------
            FirstTime = .FALSE.
            !------------------------------------------------------------------------------ 
        END IF ! FirstTime
        !------------------------------------------------------------------------------ 
        ! Compute the contribution of all tips:
        !------------------------------------------------------------------------------ 
        Source = 0.0
        !------------------------------------------------------------------------------
        DO i=1,nbtips   
        !------------------------------------------------------------------------------ 
        ! ONLY IMPLEMENTED FOR SPECIAL CASE OF 9 ELECTRIC TIPS!!!!
        !------------------------------------------------------------------------------ 
            IF(DIM==3) THEN
                Source = Source + coeff*3.5/(57.0*sigma*sigma*sigma*pi32) * &
                    exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i))+(z-ztip(i))*(z-ztip(i)))/(2*sigma*sigma))
            ELSE
                Source = Source + coeff*3.5/(57.0*sigma*sigma*2*Pi) * &
                    exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i)))/(2*sigma*sigma))
            END IF
        !------------------------------------------------------------------------------
        END DO
    !------------------------------------------------------------------------------ 
    END FUNCTION NumaReadEndElectricTipsVTK
    !------------------------------------------------------------------------------

    !------------------------------------------------------------------------------
    ! Read Middles of Electric Tips coordinates in a file for potential solver or for 
    ! electric power in heat solver
    !------------------------------------------------------------------------------
    FUNCTION NumaReadMiddleElectricTipsVTK(model, n, time) RESULT( Source )
    !------------------------------------------------------------------------------ 
        USE DefUtils
        IMPLICIT None
    !------------------------------------------------------------------------------   
        TYPE(Model_t) :: model
        INTEGER :: n
        REAL(KIND=dp) :: Source, time
    !------------------------------------------------------------------------------   
        INTEGER :: dim,i, nbtips, ios, istat,NbTipsLocationTimes,TipsLocationTimesIndex=1, &
        OldTipsLocationTimesIndex
        REAL(KIND=dp) :: x, y, z, pi32, sigma, coeff
        REAL(KIND=dp),ALLOCATABLE :: xTip(:), yTip(:), zTip(:)
        REAL(KIND=dp),ALLOCATABLE :: TipsLocationTimesArray(:,:)
        CHARACTER :: line*100, str1*10, str2*10, TipsFile*100,Char_TipsLocationTimesIndex*100
        LOGICAL :: FirstTime = .TRUE., Found, MultiTipsLocation = .FALSE.

        SAVE FirstTime,xTip,yTip,zTip,nbtips,TipsLocationTimesIndex
        !------------------------------------------------------------------------------
        ! Get the spatial coordinates:
        !------------------------------------------------------------------------------
        dim = CoordinateSystemDimension()

        x = model % Nodes % x(n)
        y = model % Nodes % y(n)
        IF (dim<3) THEN
            z = 0.0
        ELSE
            z = model % Nodes % z(n)
        END IF
        !------------------------------------------------------------------------------ 
        ! Check if the electric tips location change over time:
        !------------------------------------------------------------------------------
        MultiTipsLocation = GetLogical( Model % Simulation, &
            'Multi Electric Tips Location',Found )
        !------------------------------------------------------------------------------
        IF(MultiTipsLocation) THEN
        !------------------------------------------------------------------------------
            ! Get the number of different locations:
            !------------------------------------------------------------------------------
            NbTipsLocationTimes = GetInteger( Model % Simulation, &
                'Electric Tips Location Times Nb', Found )
            !------------------------------------------------------------------------------
            ! Get the times at which locations change:
            !------------------------------------------------------------------------------
            ALLOCATE(TipsLocationTimesArray(NbTipsLocationTimes,1),STAT=istat)
            TipsLocationTimesArray = ListGetConstRealArray( Model % Simulation, &
                'Multi Electric Tips Location Times', Found )
            !------------------------------------------------------------------------------
            ! Check if a new location has to be considered:
            !------------------------------------------------------------------------------
            OldTipsLocationTimesIndex = TipsLocationTimesIndex
            DO WHILE(Time>TipsLocationTimesArray(TipsLocationTimesIndex,1) .AND. TipsLocationTimesIndex<=NbTipsLocationTimes)
                TipsLocationTimesIndex = TipsLocationTimesIndex + 1
            END DO
            IF(OldTipsLocationTimesIndex /= TipsLocationTimesIndex) THEN
                FirstTime=.TRUE.
            END IF
            !------------------------------------------------------------------------------
            DEALLOCATE(TipsLocationTimesArray)
        !------------------------------------------------------------------------------
        END IF
        !------------------------------------------------------------------------------ 
        ! Define gaussian parameters and flow:
        !------------------------------------------------------------------------------ 
        sigma = GetConstReal( Model % Simulation,'Gaussian Sigma Electric',Found )
        IF (.NOT. Found) sigma = .0030
        pi32 = exp(3.0*log(2.0*Pi)/2.0) 
        coeff = GetConstReal( Model % Simulation,'Gaussian Mult Coefficient Electric',Found )
        IF (.NOT. Found) coeff = 1.0
        !------------------------------------------------------------------------------ 
        ! If first time, read the file and fill the arrays of sources:
        !------------------------------------------------------------------------------
        IF(FirstTime) THEN
        !------------------------------------------------------------------------------     
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
            !------------------------------------------------------------------------------ 
            END IF
            !------------------------------------------------------------------------------ 
            IF (Found) THEN
            !------------------------------------------------------------------------------ 
                TipsFile = TRIM(TipsFile) // "-middles.vtk" 
                OPEN(UNIT=10,FILE=TipsFile,STATUS='OLD',ACTION='READ',IOSTAT=ios)
                !------------------------------------------------------------------------------ 
                IF(ios/=0) THEN
                    PRINT*,'Could not open file ',TipsFile
                    PRINT*,'I/O Fortran Error number ',ios
                    CALL Fatal( 'NumaReadElectricTips', 'Unable to load electric middle tips file!' )
                ELSE
                    !------------------------------------------------------------------------------ 
                    !   Read the number of points and allocate the arrays:
                    !------------------------------------------------------------------------------ 
                    READ(10,*,END=1) line
                    READ(10,*,END=1) line
                    READ(10,*,END=1) line
                    READ(10,*,END=1) line
                    READ(10,*,END=1) str1, nbtips, str2
                    IF ( .NOT. ALLOCATED( xtip ) ) THEN
                        ALLOCATE(xtip(nbtips),ytip(nbtips),ztip(nbtips),STAT=istat)
                    END IF
                    !------------------------------------------------------------------------------
                    DO i=1,nbtips   
                        !------------------------------------------------------------------------------
                        !       Read the coordinates:
                        !------------------------------------------------------------------------------
                        READ(10,*,END=1) xtip(i), ytip(i), ztip(i)
                    !------------------------------------------------------------------------------
                    END DO
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
                CALL Info('NumaReadMiddleElectricTips', &
                    'Please specify electric tips file name root in input file.', Level=1 )
                CALL Fatal( 'NumaReadMiddleElectricTips', 'Unable to load electric middle tips file!' )
            !------------------------------------------------------------------------------ 
            END IF ! name of the tips file found
            !------------------------------------------------------------------------------
            FirstTime = .FALSE.
            !------------------------------------------------------------------------------ 
        END IF ! FirstTime
        !------------------------------------------------------------------------------ 
        ! Compute the contribution of all tips:
        !------------------------------------------------------------------------------ 
        Source = 0.0
        !------------------------------------------------------------------------------
        DO i=1,nbtips   
        !------------------------------------------------------------------------------ 
        ! ONLY IMPLEMENTED FOR SPECIAL CASE OF 9 ELECTRIC TIPS!!!!
        !------------------------------------------------------------------------------ 
            IF(DIM==3) THEN
                Source = Source + coeff*2.0/(57.0*sigma*sigma*sigma*pi32) * &
                    exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i))+(z-ztip(i))*(z-ztip(i)))/(2*sigma*sigma))
            ELSE
                Source = Source + coeff*2.0/(57.0*sigma*sigma*2*Pi) * &
                    exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i)))/(2*sigma*sigma))
            END IF
        !------------------------------------------------------------------------------
        END DO
    !------------------------------------------------------------------------------ 
    END FUNCTION NumaReadMiddleElectricTipsVTK
    !------------------------------------------------------------------------------
END FUNCTION NumaReadElectricTipsVTK
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
! Read Electric Tips coordinates in a file for potential solver or for 
! electric power in heat solver
!------------------------------------------------------------------------------
FUNCTION NumaReadElectricTips(model, n, time ) RESULT( Source )
!------------------------------------------------------------------------------ 
    USE DefUtils
    IMPLICIT None
!------------------------------------------------------------------------------   
    TYPE(Model_t) :: model
    INTEGER :: n
    REAL(KIND=dp) :: time,Source
!------------------------------------------------------------------------------   
    INTEGER :: dim,i, nbtips, ios, istat,NbTipsLocationTimes,TipsLocationTimesIndex=1, &
        OldTipsLocationTimesIndex
    REAL(KIND=dp) :: x, y, z, pi32, sigma, coeff, Source_endtips, Source_middletips
    REAL(KIND=dp),ALLOCATABLE :: xTip(:), yTip(:), zTip(:), coeffTip(:), cache(:)
    REAL(KIND=dp),ALLOCATABLE :: TipsLocationTimesArray(:,:)
    CHARACTER :: line*100, str1*10, str2*10, TipsFile*100,Char_TipsLocationTimesIndex*100
    LOGICAL :: FirstTime = .TRUE., Found, MultiPower = .FALSE., &
        InterpolatedLineSource = .FALSE., MultiTipsLocation = .FALSE., &
        NonUniformElectricPower = .FALSE.

    SAVE FirstTime,xTip,yTip,zTip,nbtips,TipsLocationTimesIndex, coeffTip

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
!------------------------------------------------------------------------------ 
! Check if the electric power is constant over the tips
!------------------------------------------------------------------------------
    NonUniformElectricPower = GetLogical( Model % Simulation, &
            'Non Uniform Electric Power',Found )
    IF (.NOT. Found) NonUniformElectricPower = .FALSE.
!------------------------------------------------------------------------------ 
! Check if the electric power is constant over the probe
!------------------------------------------------------------------------------
    MultiPower = GetLogical( Model % Simulation, &
            'Multi Power Electric Tips',Found )
    IF (.NOT. Found) MultiPower = .TRUE.
!------------------------------------------------------------------------------
!   If electric power different at tips ends and middle
!------------------------------------------------------------------------------
    IF (MultiPower) THEN
!------------------------------------------------------------------------------
        Source_endtips = NumaReadEndElectricTips(model, n, time)
        Source_middletips = NumaReadMiddleElectricTips(model, n, time)
        Source = Source_endtips + Source_middletips
!------------------------------------------------------------------------------
! If electric power equal at tips ends and middle
!------------------------------------------------------------------------------
    ELSE
!------------------------------------------------------------------------------
    ! Get the spatial coordinates:
    !------------------------------------------------------------------------------
        !dim = CoordinateSystemDimension()
        !
        !x = model % Nodes % x(n)
        !y = model % Nodes % y(n)
        !IF (dim<3) THEN
        !    z = 0.0
        !ELSE
        !    z = model % Nodes % z(n)
        !END IF
        !!------------------------------------------------------------------------------ 
        !! Check if the electric tips location change over time:
        !!------------------------------------------------------------------------------
        !MultiTipsLocation = GetLogical( Model % Simulation, &
        !    'Multi Electric Tips Location',Found )
        !!------------------------------------------------------------------------------
        !IF(MultiTipsLocation) THEN
        !!------------------------------------------------------------------------------
        !    ! Get the number of different locations:
        !    !------------------------------------------------------------------------------
        !    NbTipsLocationTimes = GetInteger( Model % Simulation, &
        !        'Electric Tips Location Times Nb', Found )
        !    !------------------------------------------------------------------------------
        !    ! Get the times at which locations change:
        !    !------------------------------------------------------------------------------
        !    ALLOCATE(TipsLocationTimesArray(NbTipsLocationTimes,1),STAT=istat)
        !    TipsLocationTimesArray = ListGetConstRealArray( Model % Simulation, &
        !        'Multi Electric Tips Location Times', Found )
        !    !------------------------------------------------------------------------------
        !    ! Check if a new location has to be considered:
        !    !------------------------------------------------------------------------------
        !    OldTipsLocationTimesIndex = TipsLocationTimesIndex
        !    DO WHILE(Time>TipsLocationTimesArray(TipsLocationTimesIndex,1) .AND. TipsLocationTimesIndex<=NbTipsLocationTimes)
        !        TipsLocationTimesIndex = TipsLocationTimesIndex + 1
        !    END DO
        !    IF(OldTipsLocationTimesIndex /= TipsLocationTimesIndex) THEN
        !        FirstTime=.TRUE.
        !    END IF
        !    !------------------------------------------------------------------------------
        !    DEALLOCATE(TipsLocationTimesArray)
        !!------------------------------------------------------------------------------
        !END IF
        !!------------------------------------------------------------------------------ 
        !! Define gaussian parameters and flow:
        !!------------------------------------------------------------------------------ 
        !sigma = GetConstReal( Model % Simulation,'Gaussian Sigma Electric',Found )
        !IF (.NOT. Found) sigma = 3.0
        !pi32 = exp(3.0*log(2.0*Pi)/2.0) 
        !coeff = GetConstReal( Model % Simulation,'Gaussian Mult Coefficient Electric',Found )
        !IF (.NOT. Found) coeff = 1.0
        !!------------------------------------------------------------------------------ 
        !! If first time, read the file and fill the arrays of sources:
        !!------------------------------------------------------------------------------
        !IF(FirstTime) THEN
    !---!---------------------------------------------------------------------------     
        !    !   Open the Tips file:
        !    !------------------------------------------------------------------------------  
        !    TipsFile = GetString( Model % Simulation,'Electric Tips Filename Root',Found )  
        !    !------------------------------------------------------------------------------ 
        !    ! If variable location, add the right extension to file name:
        !    !------------------------------------------------------------------------------ 
        !    IF(MultiTipsLocation) THEN 
        !        WRITE(Char_TipsLocationTimesIndex,*) MAX(TipsLocationTimesIndex-1,1)
        !        Char_TipsLocationTimesIndex = ADJUSTL(Char_TipsLocationTimesIndex)
        !        TipsFile = TRIM(TipsFile) // "_"
        !        TipsFile = TRIM(TipsFile) // Char_TipsLocationTimesIndex            
        !    !------------------------------------------------------------------------------ 
        !    END IF
        !    !------------------------------------------------------------------------------ 
        !    IF (Found) THEN
        !    !------------------------------------------------------------------------------ 
        !        TipsFile = TRIM(TipsFile) // ".txt" 
        !        OPEN(UNIT=10,FILE=TipsFile,STATUS='OLD',ACTION='READ',IOSTAT=ios)
        !        !------------------------------------------------------------------------------ 
        !        IF(ios/=0) THEN
        !            PRINT*,'Could not open file ',TipsFile
        !            PRINT*,'I/O Fortran Error number ',ios
        !            CALL Fatal( 'NumaReadElectricTips', 'Unable to load electric tips file!' )
        !        ELSE
        !            !------------------------------------------------------------------------------ 
        !            !   Read the number of points and allocate the arrays:
        !            !------------------------------------------------------------------------------ 
        !            READ(10,*,END=1) line
        !            READ(10,*,END=1) str1, nbtips, str2
        !            IF ( .NOT. ALLOCATED( xtip ) ) THEN
        !                ALLOCATE(xtip(nbtips),ytip(nbtips),ztip(nbtips),STAT=istat) 
        !                IF (NonUniformElectricPower) ALLOCATE(coefftip(nbtips))
        !            END IF
        !            !------------------------------------------------------------------------------
        !            DO i=1,nbtips   
        !                !------------------------------------------------------------------------------
        !                !       Read the coordinates:
        !                !------------------------------------------------------------------------------
        !                IF (NonUniformElectricPower) THEN
        !                    READ(10,*,END=1) xtip(i), ytip(i), ztip(i), coefftip(i)
        !                ELSE
        !                    READ(10,*,END=1) xtip(i), ytip(i), ztip(i)
        !                END IF
        !            !------------------------------------------------------------------------------
        !            END DO
        !            !------------------------------------------------------------------------------
        !            ! Check that the non uniform electric power is set correctly:
        !            !------------------------------------------------------------------------------
        !            IF (NonUniformElectricPower) THEN
        !                IF (SUM(coefftip)/=21.0) THEN
        !                    PRINT*,'Check coefficients for non uniform electric power! Value 1 set everywhere.'
        !                    coefftip = 1.0
        !                ELSE
        !                    PRINT*,'Non uniform electric power:'
        !                    DO i=1,nbtips
        !                        PRINT*,'Point ',i,': coeff=',coefftip(i)
        !                    END DO
        !                END IF
        !            END IF
        !            !------------------------------------------------------------------------------ 
        !            !   Close the Tips file:
        !            !------------------------------------------------------------------------------ 
        !            1 CONTINUE
        !            CLOSE(10)
        !        !------------------------------------------------------------------------------ 
        !        END IF
        !        !------------------------------------------------------------------------------ 
        !    ELSE
        !    !------------------------------------------------------------------------------
        !    !   If the file can't be found, print an error message and stop the simulation: 
        !    !------------------------------------------------------------------------------
        !        CALL Info('NumaReadElectricTips', &
        !            'Please specify electric tips file name root in input file.', Level=1 )
        !        CALL Fatal( 'NumaReadElectricTips', 'Unable to load electric tips file!' )
        !    !------------------------------------------------------------------------------
        !    END IF ! name of the tips file found
        !    !------------------------------------------------------------------------------
        !    FirstTime = .FALSE.
        !    !------------------------------------------------------------------------------ 
        !END IF ! FirstTime
        !!------------------------------------------------------------------------------

        !!------------------------------------------------------------------------------ 
        !! Compute the contribution of all tips:
        !!------------------------------------------------------------------------------ 
        !Source = 0.0
        !!------------------------------------------------------------------------------
        !DO i=1,nbtips   
        !!------------------------------------------------------------------------------ 
        !! Smoothed source with gaussian
        !!------------------------------------------------------------------------------ 
        !    IF(DIM==3) THEN
        !!------------------------------------------------------------------------------ 
        !        IF (NonUniformElectricPower) THEN
        !            Source = Source + coeff/(nbtips*sigma*sigma*sigma*pi32) * &
        !                coefftip(i)*&
        !                exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i))+(z-ztip(i))*(z-ztip(i)))/(2*sigma*sigma))
        !        ELSE
        !            Source = Source + coeff/(nbtips*sigma*sigma*sigma*pi32) * &
        !                exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i))+(z-ztip(i))*(z-ztip(i)))/(2*sigma*sigma))
        !        END IF
        !!------------------------------------------------------------------------------ 
        !    ELSE
        !!------------------------------------------------------------------------------ 
        !        Source = Source + coeff/(nbtips*sigma*sigma*2.0*Pi) * &
        !            exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i)))/(2*sigma*sigma))
        !!------------------------------------------------------------------------------ 
        !    END IF
        !!------------------------------------------------------------------------------ 
        !! Sharp source (constant value on a sphere)
        !!------------------------------------------------------------------------------         
        !    !IF (sqrt((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i))+(z-ztip(i))*(z-ztip(i)))<sigma) THEN
        !    !   Source = Source + coeff/(nbtips*sigma*sigma*sigma*Pi*4/3) 
        !    !END IF
        !!------------------------------------------------------------------------------
        !END DO
    !------------------------------------------------------------------------------
    END IF
    !------------------------------------------------------------------------------ 




    CONTAINS

    !------------------------------------------------------------------------------
    ! Read End Electric Tips coordinates in a file for potential solver or for 
    ! electric power in heat solver
    !------------------------------------------------------------------------------
    FUNCTION NumaReadEndElectricTips(model, n, time) RESULT( Source )
    !------------------------------------------------------------------------------ 
        USE DefUtils
        IMPLICIT None
    !------------------------------------------------------------------------------   
        TYPE(Model_t) :: model
        INTEGER :: n
        REAL(KIND=dp) :: Source,time
    !------------------------------------------------------------------------------   
        INTEGER :: dim,i, nbtips, ios, istat,NbTipsLocationTimes,TipsLocationTimesIndex=1, &
        OldTipsLocationTimesIndex
        REAL(KIND=dp) :: x, y, z, pi32, sigma, coeff
        REAL(KIND=dp),ALLOCATABLE :: xTip(:), yTip(:), zTip(:), coeffTip(:)
        REAL(KIND=dp),ALLOCATABLE :: TipsLocationTimesArray(:,:)
        CHARACTER :: line*100, str1*10, str2*10, TipsFile*100,Char_TipsLocationTimesIndex*100
        LOGICAL :: FirstTime = .TRUE., Found, MultiTipsLocation = .FALSE., &
        NonUniformElectricPower = .FALSE.

        SAVE FirstTime,xTip,yTip,zTip,nbtips,TipsLocationTimesIndex, coeffTip
        !------------------------------------------------------------------------------
        ! Get the spatial coordinates:
        !------------------------------------------------------------------------------
        dim = CoordinateSystemDimension()

        x = model % Nodes % x(n)
        y = model % Nodes % y(n)
        IF (dim<3) THEN
            z = 0.0
        ELSE
            z = model % Nodes % z(n)
        END IF
        !------------------------------------------------------------------------------ 
        ! Check if the electric tips location change over time:
        !------------------------------------------------------------------------------
        MultiTipsLocation = GetLogical( Model % Simulation, &
            'Multi Electric Tips Location',Found )
        !------------------------------------------------------------------------------
        IF(MultiTipsLocation) THEN
        !------------------------------------------------------------------------------
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
            TipsLocationTimesArray = ListGetConstRealArray( Model % Simulation, &
                'Multi Electric Tips Location Times', Found )
            !------------------------------------------------------------------------------
            ! Check if a new location has to be considered:
            !------------------------------------------------------------------------------
            OldTipsLocationTimesIndex = TipsLocationTimesIndex
            DO WHILE(Time>TipsLocationTimesArray(TipsLocationTimesIndex,1) .AND. TipsLocationTimesIndex<=NbTipsLocationTimes)
                TipsLocationTimesIndex = TipsLocationTimesIndex + 1
            END DO
            IF(OldTipsLocationTimesIndex /= TipsLocationTimesIndex) THEN
                FirstTime=.TRUE.
            END IF
            !------------------------------------------------------------------------------
            DEALLOCATE(TipsLocationTimesArray)
        !------------------------------------------------------------------------------
        END IF
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
        !------------------------------------------------------------------------------ 
        ! If first time, read the file and fill the arrays of sources:
        !------------------------------------------------------------------------------
        IF(FirstTime) THEN
        !------------------------------------------------------------------------------     
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
            !------------------------------------------------------------------------------ 
            END IF
            !------------------------------------------------------------------------------ 
            IF (Found) THEN
            !------------------------------------------------------------------------------ 
                TipsFile = TRIM(TipsFile) // "-ends.txt" 
                OPEN(UNIT=10,FILE=TipsFile,STATUS='OLD',ACTION='READ',IOSTAT=ios)
                !------------------------------------------------------------------------------ 
                IF(ios/=0) THEN
                    PRINT*,'Could not open file ',TipsFile
                    PRINT*,'I/O Fortran Error number ',ios
                    CALL Fatal( 'NumaReadElectricTips', 'Unable to load electric end tips file' )
                ELSE
                    !------------------------------------------------------------------------------ 
                    !   Read the number of points and allocate the arrays:
                    !------------------------------------------------------------------------------ 
                    READ(10,*,END=1) line
                    READ(10,*,END=1) str1, nbtips, str2
                    IF ( .NOT. ALLOCATED( xtip ) ) THEN
                        ALLOCATE(xtip(nbtips),ytip(nbtips),ztip(nbtips),STAT=istat)
                        IF (NonUniformElectricPower) ALLOCATE(coefftip(nbtips))
                    END IF
                    !------------------------------------------------------------------------------
                    DO i=1,nbtips   
                        !------------------------------------------------------------------------------
                        !       Read the coordinates:
                        !------------------------------------------------------------------------------
                        IF (NonUniformElectricPower) THEN
                            READ(10,*,END=1) xtip(i), ytip(i), ztip(i), coefftip(i)
                        ELSE
                            READ(10,*,END=1) xtip(i), ytip(i), ztip(i)
                        END IF
                    !------------------------------------------------------------------------------
                    END DO
                    !------------------------------------------------------------------------------
                    ! Check that the non uniform electric power is set correctly:
                    !------------------------------------------------------------------------------
                    IF (NonUniformElectricPower) THEN
                        IF (SUM(coefftip)/=10.0) THEN
                            PRINT*,'Check coefficients for non uniform electric power for end points! Value 1 set everywhere.'
                            coefftip = 1.0
                        ELSE
                            PRINT*,'Non uniform electric power at end points:'
                            DO i=1,nbtips
                                PRINT*,'Point ',i,': coeff=',coefftip(i)
                            END DO
                        END IF
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
                CALL Info('NumaReadEndElectricTips', &
                    'Please specify electric tips file name root in input file.', Level=1 )
                CALL Fatal( 'NumaReadEndElectricTips', 'Unable to load electric end tips file' )
            !------------------------------------------------------------------------------
            END IF ! name of the tips file found
            !------------------------------------------------------------------------------
            FirstTime = .FALSE.
            !------------------------------------------------------------------------------ 
        END IF ! FirstTime
        !------------------------------------------------------------------------------ 
        ! Compute the contribution of all tips:
        !------------------------------------------------------------------------------ 
        Source = 0.0
        !------------------------------------------------------------------------------
        DO i=1,nbtips   
        !------------------------------------------------------------------------------ 
        ! ONLY IMPLEMENTED FOR SPECIAL CASE OF 9 ELECTRIC TIPS!!!!
        !------------------------------------------------------------------------------ 
            IF(DIM==3) THEN
        !------------------------------------------------------------------------------
                IF (NonUniformElectricPower) THEN
                    !Source = Source + coeff*3.5/(57.0*sigma*sigma*sigma*pi32) * &
                    !    coefftip(i)*&
                    !    exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i))+(z-ztip(i))*(z-ztip(i)))/(2*sigma*sigma))
                    Source = Source + (1.5 / 26) * coeff*(sigma*SQRT(2*Pi))**(-3.0) * &
                        coefftip(i)*exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i))+(z-ztip(i))*(z-ztip(i)))/(2*sigma*sigma))
                ELSE
                    !Source = Source + coeff*2.0/(57.0*sigma*sigma*sigma*pi32) * &
                    !    exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i))+(z-ztip(i))*(z-ztip(i)))/(2*sigma*sigma))
                    Source = Source + (1.5 / 26) * coeff*(sigma*SQRT(2*Pi))**(-3.0) * &
                        exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i))+(z-ztip(i))*(z-ztip(i)))/(2*sigma*sigma))
                END IF
        !------------------------------------------------------------------------------
            ELSE
        !------------------------------------------------------------------------------
                Source = Source + coeff*1.5/(6.5*sigma*sigma*2*Pi) * &
                    exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i)))/(2*sigma*sigma))

                !Source = Source + coeff*1.5/(7.0*sigma*sigma*2*Pi) * &
                !   exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i)))/(2*sigma*sigma))
        !------------------------------------------------------------------------------
            END IF
        !------------------------------------------------------------------------------
        END DO
    !------------------------------------------------------------------------------ 
    END FUNCTION NumaReadEndElectricTips
    !------------------------------------------------------------------------------

    !------------------------------------------------------------------------------
    ! Read Middles of Electric Tips coordinates in a file for potential solver or for 
    ! electric power in heat solver
    !------------------------------------------------------------------------------
    FUNCTION NumaReadMiddleElectricTips(model, n, time ) RESULT( Source )
    !------------------------------------------------------------------------------ 
        USE DefUtils
        IMPLICIT None
    !------------------------------------------------------------------------------   
        TYPE(Model_t) :: model
        INTEGER :: n
        REAL(KIND=dp) :: Source,time
    !------------------------------------------------------------------------------   
        INTEGER :: dim,i, nbtips, ios, istat,NbTipsLocationTimes,TipsLocationTimesIndex=1, &
        OldTipsLocationTimesIndex
        REAL(KIND=dp) :: x, y, z, pi32, sigma, coeff
        REAL(KIND=dp),ALLOCATABLE :: xTip(:), yTip(:), zTip(:), coeffTip(:)
        REAL(KIND=dp),ALLOCATABLE :: TipsLocationTimesArray(:,:)
        CHARACTER :: line*100, str1*10, str2*10, TipsFile*100,Char_TipsLocationTimesIndex*100
        LOGICAL :: FirstTime = .TRUE., Found, MultiTipsLocation = .FALSE., &
        NonUniformElectricPower = .FALSE.

        SAVE FirstTime,xTip,yTip,zTip,nbtips,TipsLocationTimesIndex, coeffTip
        !------------------------------------------------------------------------------
        ! Get the spatial coordinates:
        !------------------------------------------------------------------------------
        dim = CoordinateSystemDimension()

        x = model % Nodes % x(n)
        y = model % Nodes % y(n)
        IF (dim<3) THEN
            z = 0.0
        ELSE
            z = model % Nodes % z(n)
        END IF
        !------------------------------------------------------------------------------ 
        ! Check if the electric tips location change over time:
        !------------------------------------------------------------------------------
        MultiTipsLocation = GetLogical( Model % Simulation, &
            'Multi Electric Tips Location',Found )
        !------------------------------------------------------------------------------
        IF(MultiTipsLocation) THEN
        !------------------------------------------------------------------------------
            ! Get the number of different locations:
            !------------------------------------------------------------------------------
            NbTipsLocationTimes = GetInteger( Model % Simulation, &
                'Electric Tips Location Times Nb', Found )
            !TODO check that this has fixed the unallocated read valgrind shows on 1442
            IF (.NOT. Found) THEN
                NbTipsLocationTimes = 1
            END IF
            !------------------------------------------------------------------------------
            ! Get the times at which locations change:
            !------------------------------------------------------------------------------
            ALLOCATE(TipsLocationTimesArray(NbTipsLocationTimes,1),STAT=istat)
            TipsLocationTimesArray = ListGetConstRealArray( Model % Simulation, &
                'Multi Electric Tips Location Times', Found )
            !------------------------------------------------------------------------------
            ! Check if a new location has to be considered:
            !------------------------------------------------------------------------------
            OldTipsLocationTimesIndex = TipsLocationTimesIndex
            DO WHILE(Time>TipsLocationTimesArray(TipsLocationTimesIndex,1) .AND. TipsLocationTimesIndex<=NbTipsLocationTimes)
                TipsLocationTimesIndex = TipsLocationTimesIndex + 1
            END DO
            IF(OldTipsLocationTimesIndex /= TipsLocationTimesIndex) THEN
                FirstTime=.TRUE.
            END IF
            !------------------------------------------------------------------------------
            DEALLOCATE(TipsLocationTimesArray)
        !------------------------------------------------------------------------------
        END IF
        !------------------------------------------------------------------------------ 
        ! Define gaussian parameters and flow:
        !------------------------------------------------------------------------------ 
        sigma = GetConstReal( Model % Simulation,'Gaussian Sigma Electric',Found )
        IF (.NOT. Found) sigma = 0.003
        pi32 = exp(3.0*log(2.0*Pi)/2.0) 
        coeff = GetConstReal( Model % Simulation,'Gaussian Mult Coefficient Electric',Found )
        IF (.NOT. Found) coeff = 1!NumaReadElectricPower(model, n, time)
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
            !------------------------------------------------------------------------------ 
            END IF
            !------------------------------------------------------------------------------ 
            IF (Found) THEN
            !------------------------------------------------------------------------------ 
                TipsFile = TRIM(TipsFile) // "-middles.txt" 
                OPEN(UNIT=10,FILE=TipsFile,STATUS='OLD',ACTION='READ',IOSTAT=ios)
                !------------------------------------------------------------------------------ 
                IF(ios/=0) THEN
                    PRINT*,'Could not open file ',TipsFile
                    PRINT*,'I/O Fortran Error number ',ios
                    CALL Fatal( 'NumaReadElectricTips', 'Unable to load electric middle tips file!' )
                ELSE
                    !------------------------------------------------------------------------------ 
                    !   Read the number of points and allocate the arrays:
                    !------------------------------------------------------------------------------ 
                    READ(10,*,END=1) line
                    READ(10,*,END=1) str1, nbtips, str2
                    IF ( .NOT. ALLOCATED( xtip ) ) THEN
                        ALLOCATE(xtip(nbtips),ytip(nbtips),ztip(nbtips),STAT=istat)
                        IF (NonUniformElectricPower) ALLOCATE(coefftip(nbtips))
                    END IF
                    !------------------------------------------------------------------------------
                    DO i=1,nbtips   
                        !------------------------------------------------------------------------------
                        !       Read the coordinates:
                        !------------------------------------------------------------------------------
                        IF (NonUniformElectricPower) THEN
                            READ(10,*,END=1) xtip(i), ytip(i), ztip(i), coefftip(i)
                        ELSE
                            READ(10,*,END=1) xtip(i), ytip(i), ztip(i)
                        END IF
                    !------------------------------------------------------------------------------
                    END DO
                    !------------------------------------------------------------------------------ 
                    !   Close the Tips file:
                    !------------------------------------------------------------------------------ 
                    1 CONTINUE
                    CLOSE(10)
                    !------------------------------------------------------------------------------
                    ! Check that the non uniform electric power is set correctly:
                    !------------------------------------------------------------------------------
                    IF (NonUniformElectricPower) THEN
                        IF (SUM(coefftip)/=11.0) THEN
                            PRINT*,'Check coefficients for non uniform electric power for middle points! Value 1 set everywhere.'
                            coefftip = 1.0
                        ELSE
                            PRINT*,'Non uniform electric power at middle points:'
                            DO i=1,nbtips
                                PRINT*,'Point ',i,': coeff=',coefftip(i)
                            END DO
                        END IF
                    END IF
                !------------------------------------------------------------------------------ 
                END IF
            !------------------------------------------------------------------------------ 
            ELSE
            !------------------------------------------------------------------------------
            !   If the file can't be found, print an error message and stop the simulation: 
            !------------------------------------------------------------------------------
                CALL Info('NumaReadMiddleElectricTips', &
                    'Please specify electric tips file name root in input file.', Level=1 )
                CALL Fatal( 'NumaReadMiddleElectricTips', 'Unable to load electric middle tips file!' )
            !------------------------------------------------------------------------------ 
            END IF ! name of the tips file found
            !------------------------------------------------------------------------------
            FirstTime = .FALSE.
            !------------------------------------------------------------------------------ 
        END IF ! FirstTime
        !------------------------------------------------------------------------------ 
        ! Compute the contribution of all tips:
        !------------------------------------------------------------------------------ 
        Source = 0.0
        !------------------------------------------------------------------------------
        DO i=1,nbtips   
        !------------------------------------------------------------------------------ 
        ! ONLY IMPLEMENTED FOR SPECIAL CASE OF 9 ELECTRIC TIPS!!!!
        !------------------------------------------------------------------------------ 
            IF(DIM==3) THEN
        !------------------------------------------------------------------------------
                IF (NonUniformElectricPower) THEN
                    !Source = Source + coeff*2.0/(57.0*sigma*sigma*sigma*pi32) * &
                    !    coefftip(i)*&
                    !    exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i))+(z-ztip(i))*(z-ztip(i)))/(2*sigma*sigma))
                    Source = Source + (1.0 / 26) * coeff*(sigma*SQRT(2*Pi))**(-3.0) * &
                        coefftip(i)*exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i))+(z-ztip(i))*(z-ztip(i)))/(2*sigma*sigma))
                ELSE
                    !Source = Source + coeff*2.0/(57.0*sigma*sigma*sigma*pi32) * &
                    !    exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i))+(z-ztip(i))*(z-ztip(i)))/(2*sigma*sigma))
                    Source = Source + (1.0 / 26) * coeff*(sigma*SQRT(2*Pi))**(-3.0) * &
                        exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i))+(z-ztip(i))*(z-ztip(i)))/(2*sigma*sigma))
                END IF
        !------------------------------------------------------------------------------
            ELSE
        !------------------------------------------------------------------------------
                Source = Source + coeff*1.0/(6.5*sigma*sigma*2*Pi) * &
                    exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i)))/(2*sigma*sigma))

                !Source = Source + coeff*2.5/(7.0*sigma*sigma*2*Pi) * &
                !   exp(-((x-xtip(i))*(x-xtip(i))+(y-ytip(i))*(y-ytip(i)))/(2*sigma*sigma))
        !------------------------------------------------------------------------------
            END IF
        !------------------------------------------------------------------------------
        END DO
    !------------------------------------------------------------------------------ 
    END FUNCTION NumaReadMiddleElectricTips

!------------------------------------------------------------------------------
! Read the values of electric power over time in a text file (the first time 
! only), save these values and interpolate at each timestep 
!------------------------------------------------------------------------------
FUNCTION NumaReadElectricPower(model, n, time) RESULT( Power )
!------------------------------------------------------------------------------ 
    USE DefUtils
  USE GeneralUtils
    IMPLICIT None
!------------------------------------------------------------------------------   
    TYPE(Model_t) :: model
    INTEGER :: n
    REAL(KIND=dp) :: Power,time
!------------------------------------------------------------------------------   
    INTEGER NbValues,i, ios, istat
    REAL(KIND=dp),ALLOCATABLE :: TimeArray(:), PowerArray(:)
    CHARACTER :: line*100, PowerFile*100
    LOGICAL :: FirstTime = .TRUE., Found, Echo = .FALSE.

    !TODO:NUMA:If my reading of >=F90 is right, since NbValues is not
    !initialized, it won't be saved unless included below and was only retaining
    !its original value by coincidence - is that true? My tests suggest this is
    !the case, as it seemed to occasionally modify depending on when and where
    !NREP was called
    SAVE NbValues,FirstTime,TimeArray,PowerArray
    !------------------------------------------------------------------------------ 
    ! If first time, read the file and fill the arrays of time/power:
    !------------------------------------------------------------------------------
    IF(FirstTime) THEN
    !------------------------------------------------------------------------------     
        !   Open the Power file:
        !------------------------------------------------------------------------------  
        PowerFile = GetString( Model % Simulation,'Electric Power Filename',Found ) 
        !------------------------------------------------------------------------------ 
        IF (Found) THEN
        !------------------------------------------------------------------------------ 
            PowerFile = TRIM(PowerFile) // ".txt" 

            OPEN(UNIT=10,FILE=PowerFile,STATUS='OLD',ACTION='READ',IOSTAT=ios)
            !------------------------------------------------------------------------------ 
            IF(ios/=0) THEN
                PRINT*,'Could not open file ',PowerFile
                PRINT*,'I/O Fortran Error number ',ios
                CALL Fatal( 'NumaReadElectricPower', 'Unable to load electric power file' )
            ELSE
                !------------------------------------------------------------------------------ 
                !   Get the number of values and allocate the arrays:
                !------------------------------------------------------------------------------ 
                NbValues = -1
                
                DO WHILE( ReadAndTrim( 10,line,Echo ) )
                    IF (line=='begin') THEN
                        NbValues = 0
                        DO WHILE( ReadAndTrim( 10,line,Echo ) )
                            IF (Line==' ')  CYCLE
                            IF (Line=='end')  EXIT
                            NbValues = NbValues + 1
                        END DO
                    END IF
                END DO
                IF (NbValues==-1) THEN
                    CALL Fatal( 'NumaReadElectricPower', 'Please check the format of file '// &
                            PowerFile // 'Must be [Begin//Time Power//..//Time Power//End].' )
                ELSE IF (NbValues==0) THEN
                    CALL Fatal('NumaReadElectricPower','No values found in file '//PowerFile )
                END IF
                !------------------------------------------------------------------------------ 
                !   Restart reading at the beginning of the file:
                !------------------------------------------------------------------------------
                REWIND (UNIT=10,IOSTAT=ios)
                !------------------------------------------------------------------------------ 
                !   Allocate the arrays:
                !------------------------------------------------------------------------------
                IF ( .NOT. ALLOCATED( TimeArray ) ) THEN
                    ALLOCATE(TimeArray(NbValues),PowerArray(NbValues),STAT=istat)
                END IF

                READ(10,*) line
                !------------------------------------------------------------------------------
                DO i=1,NbValues 
                    !------------------------------------------------------------------------------
                    !       Read the values of time amd power:
                    !------------------------------------------------------------------------------
                    READ(10,*,END=1) TimeArray(i), PowerArray(i)
                !------------------------------------------------------------------------------
                END DO
                !------------------------------------------------------------------------------ 
                !   Close the Power file:
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
            CALL Info('NumaReadElectricPower', &
                'Please specify electric power file name root in input file.', Level=1 )
            CALL Info( 'NumaReadElectricPower', 'Unable to load electric power file' )
            Power = 1.0
            FirstTime = .FALSE.
            RETURN
        !------------------------------------------------------------------------------
        END IF ! name of the power file found
        !------------------------------------------------------------------------------
        FirstTime = .FALSE.
        !------------------------------------------------------------------------------ 
    END IF ! FirstTime
    !------------------------------------------------------------------------------ 
    ! Compute the power by interpolation:
    !------------------------------------------------------------------------------ 
    Power = 0.0
    IF (NbValues>0) Power = InterpolateCurve( TimeArray,PowerArray,time )
    !WRITE( Message, * ) 'Invalid value of Beta ', Power, time !RMV                                                                                                                                                                                                                                                                                                                         
    !CALL Info('NumaHeatSolve',Message,Level=4 )
!------------------------------------------------------------------------------ 
END FUNCTION NumaReadElectricPower
!------------------------------------------------------------------------------
    !------------------------------------------------------------------------------
END FUNCTION NumaReadElectricTips
!------------------------------------------------------------------------------




!------------------------------------------------------------------------------
! Read Sources and Sinks coordinates in a file for pressure solver
!------------------------------------------------------------------------------
FUNCTION NumaReadSourceSink(model, n, dummyArgument ) RESULT( SourceSink )
!------------------------------------------------------------------------------ 
    USE DefUtils
    IMPLICIT None
!------------------------------------------------------------------------------   
    TYPE(Model_t) :: model
    INTEGER :: n
    REAL(KIND=dp) :: dummyArgument,SourceSink
!------------------------------------------------------------------------------   
    INTEGER :: dim,i, nbsources, nbsinks, ios, istat
  REAL(KIND=dp) :: x, y, z, pi32, flow
    REAL(KIND=dp) :: sigma
    REAL(KIND=dp),ALLOCATABLE :: xSource(:), ySource(:), zSource(:), &
        xSink(:), ySink(:), zSink(:)
    CHARACTER :: line*100, str1*10, str2*10
    LOGICAL :: FirstTime = .TRUE., Found
    CHARACTER :: SourcesFile*100, SinksFile*100

    SAVE FirstTime,xSource,ySource,zSource,xSink,ySink,zSink,nbsources,nbsinks
!------------------------------------------------------------------------------ 
! Get the spatial coordinates:
!------------------------------------------------------------------------------
    dim = CoordinateSystemDimension()

    x = model % Nodes % x(n)
    y = model % Nodes % y(n)
    IF (dim<3) THEN
        z = 0.0
    ELSE
        z = model % Nodes % z(n)
    END IF
!------------------------------------------------------------------------------ 
! Define gaussian parameters and flow:
!------------------------------------------------------------------------------ 
    sigma = GetConstReal( Model % Simulation,'Gaussian Sigma Pressure',Found )
    IF (.NOT. Found) sigma = 0.0030
  pi32 = exp(3.0*log(2.0*Pi)/2.0)   
    flow = GetConstReal( Model % Simulation,'Pressure SourcesSinks Flow',Found )
    IF (.NOT. Found) flow = 1.0
!------------------------------------------------------------------------------ 
! If first time, read the files and fill the arrays of sources and sinks:
!------------------------------------------------------------------------------
    IF(FirstTime) THEN
!------------------------------------------------------------------------------     
        !   Sources
        !------------------------------------------------------------------------------ 
        !   Open the Sources file:
        !------------------------------------------------------------------------------  
        SourcesFile = GetString( Model % Simulation,'Pressure Sources Filename',Found ) 
        !------------------------------------------------------------------------------ 
        IF (Found) THEN
        !------------------------------------------------------------------------------ 
            OPEN(UNIT=10,FILE=SourcesFile,STATUS='OLD',ACTION='READ',IOSTAT=ios)
            !------------------------------------------------------------------------------ 
            IF(ios/=0) THEN
                PRINT*,'Could not open file ',SourcesFile
                PRINT*,'I/O Fortran Error number ',ios
                CALL Fatal( 'NumaReadSourceSink', 'Unable to load pressure sources file!' )
            ELSE
                !------------------------------------------------------------------------------ 
                !   Read the number of points and allocate the arrays:
                !------------------------------------------------------------------------------ 
                READ(10,*,END=1) line
                READ(10,*,END=1) str1, nbsources, str2
                ALLOCATE(xsource(nbsources),ysource(nbsources),zsource(nbsources),STAT=istat)
                !------------------------------------------------------------------------------
                DO i=1,nbsources    
                    !------------------------------------------------------------------------------
                    !       Read the coordinates:
                    !------------------------------------------------------------------------------
                    READ(10,*,END=1) xsource(i), ysource(i), zsource(i)
                !------------------------------------------------------------------------------
                END DO
                !------------------------------------------------------------------------------ 
                !   Close the Sources file:
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
            CALL Info('NumaReadSourceSink', &
                    'Please specify pressure sources file name in input file.', Level=1 )
            CALL Fatal( 'NumaReadSourceSink', 'Unable to load pressure sources file!' )
        !------------------------------------------------------------------------------ 
        END IF ! name of the sources file found
        !------------------------------------------------------------------------------
        !   Sinks
        !------------------------------------------------------------------------------ 
        !   Open the Sinks file:
        !------------------------------------------------------------------------------ 
        SinksFile = GetString( Model % Simulation,'Pressure Sinks Filename',Found ) 
        !------------------------------------------------------------------------------ 
        IF (Found) THEN
        !------------------------------------------------------------------------------
            OPEN(UNIT=10,FILE=SinksFile,STATUS='OLD',ACTION='READ',IOSTAT=ios)
            IF(ios/=0) THEN
                PRINT*,'Could not open file ',SinksFile
                PRINT*,'I/O Fortran Error number: ',ios
                CALL Fatal( 'NumaReadSourceSink', 'Unable to load pressure sinks file!' )
            ELSE
                !------------------------------------------------------------------------------ 
                !   Read the number of points and allocate the arrays:
                !------------------------------------------------------------------------------ 
                READ(10,*,END=2) line
                READ(10,*,END=2) str1, nbsinks, str2
                ALLOCATE(xsink(nbsinks),ysink(nbsinks),zsink(nbsinks),STAT=istat)   
                !------------------------------------------------------------------------------ 
                DO i=1,nbsinks  
                    !------------------------------------------------------------------------------
                    !       Read the coordinates:
                    !------------------------------------------------------------------------------
                    READ(10,*,END=2) xsink(i), ysink(i), zsink(i)
                !------------------------------------------------------------------------------
                END DO
                !------------------------------------------------------------------------------ 
                !   Close the Sinks file:
                !------------------------------------------------------------------------------ 
                2 CONTINUE                  
                CLOSE(10)
                !------------------------------------------------------------------------------
            END IF 
        !------------------------------------------------------------------------------
        ELSE
        !------------------------------------------------------------------------------
        !   If the file can't be found, print an error message and stop the simulation: 
        !------------------------------------------------------------------------------
            CALL Info('NumaReadSourceSink', &
                    'Please specify pressure sinks file name in input file.', Level=1 )
            CALL Fatal( 'NumaReadSourceSink', 'Unable to load pressure sinks file' )
        !------------------------------------------------------------------------------
        END IF ! name of the sinks file found
        !------------------------------------------------------------------------------
        FirstTime = .FALSE.
        !------------------------------------------------------------------------------ 
    END IF ! FirstTime
    !------------------------------------------------------------------------------ 

    !------------------------------------------------------------------------------ 
    ! Compute the contribution of all sources and sinks:
    !------------------------------------------------------------------------------ 
    SourceSink = 0.0
    !------------------------------------------------------------------------------ 
    DO i=1,nbsources    
    !------------------------------------------------------------------------------ 
        IF(DIM==3) THEN
            SourceSink = SourceSink - flow/(nbsources*sigma*sigma*sigma*pi32) * exp(-((x-xsource(i))*(x-xsource(i))+ &
                (y-ysource(i))*(y-ysource(i))+(z-zsource(i))*(z-zsource(i)))/(2*sigma*sigma))
        ELSE
            SourceSink = SourceSink - flow/(nbsources*sigma*sigma*2*Pi) * exp(-((x-xsource(i))*(x-xsource(i))+ &
                (y-ysource(i))*(y-ysource(i)))/(2*sigma*sigma))
        END IF
    !------------------------------------------------------------------------------
    END DO
    !------------------------------------------------------------------------------
    DO i=1,nbsinks  
    !------------------------------------------------------------------------------ 
        IF(DIM==3) THEN
            SourceSink = SourceSink + flow/(nbsinks*sigma*sigma*sigma*pi32) * exp(-((x-xsink(i))*(x-xsink(i))+ &
                (y-ysink(i))*(y-ysink(i))+(z-zsink(i))*(z-zsink(i)))/(2*sigma*sigma))
        ELSE
            SourceSink = SourceSink + flow/(nbsinks*sigma*sigma*2*Pi) * exp(-((x-xsink(i))*(x-xsink(i))+ &
                (y-ysink(i))*(y-ysink(i)))/(2*sigma*sigma))
        END IF
    !------------------------------------------------------------------------------
    END DO
    !------------------------------------------------------------------------------

!------------------------------------------------------------------------------ 
END FUNCTION NumaReadSourceSink
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! Initialize temperature to a non constant value
!------------------------------------------------------------------------------
FUNCTION NumaInitTemperature(model, n, dummyArgument ) RESULT( InitTemp )
!------------------------------------------------------------------------------ 
    USE DefUtils
    IMPLICIT None
!------------------------------------------------------------------------------   
    TYPE(Model_t) :: model
    INTEGER :: n
    REAL(KIND=dp) :: dummyArgument,InitTemp
!------------------------------------------------------------------------------   
    INTEGER :: dim
  REAL(KIND=dp) :: x, y, z
!------------------------------------------------------------------------------ 
! Get the spatial coordinates:
!------------------------------------------------------------------------------
    dim = CoordinateSystemDimension()
    
    x = model % Nodes % x(n)
    y = model % Nodes % y(n)
    IF (dim<3) THEN
        z = 0.0
    ELSE
        z = model % Nodes % z(n)
    END IF

    !------------------------------------------------------------------------------ 
    InitTemp = 0.0
    !-----------------------------------------------------------------------------  
    !InitTemp = 50.0-(x-25)

    !IF (x<=45) THEN 
    !   InitTemp = 50.0
    !ELSE IF (x<=55) THEN 
    !   InitTemp = 275.0-5*x
    !ELSE 
    !   InitTemp = 0.0
    !END IF

    !InitTemp = 50.0 * exp(-((x-50.0)**2+(y-70.0)**2+(z-110.0)**2)/(100))
    InitTemp = 310.0 + 50.0 * exp(-((x-25.0)**2+(y-12.5)**2)/(50))
    !InitTemp = 310.0 + 50.0 * exp(-((x-25.0)**2)/(50))

    !------------------------------------------------------------------------------

!------------------------------------------------------------------------------ 
END FUNCTION NumaInitTemperature
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! Initialize temperature to a non constant value
!------------------------------------------------------------------------------
FUNCTION NumaForceTerm(model, n, dummyArgument ) RESULT( Force )
!------------------------------------------------------------------------------ 
    USE DefUtils
    IMPLICIT None
!------------------------------------------------------------------------------   
    TYPE(Model_t) :: model
    INTEGER :: n
    REAL(KIND=dp) :: dummyArgument,Force
!------------------------------------------------------------------------------   
    INTEGER :: dim
  REAL(KIND=dp) :: x, y, z
!------------------------------------------------------------------------------ 
! Get the spatial coordinates:
!------------------------------------------------------------------------------
    dim = CoordinateSystemDimension()
    
    x = model % Nodes % x(n)
    y = model % Nodes % y(n)
    IF (dim<3) THEN
        z = 0.0
    ELSE
        z = model % Nodes % z(n)
    END IF

    !------------------------------------------------------------------------------ 
    Force = 0.0
    !-----------------------------------------------------------------------------  
    IF (x<=150) THEN 
        Force = 1-0.01*x
    ELSE IF (x<=200) THEN 
        Force = -2+0.01*x
    ELSE 
        Force = 0.0
    END IF
!------------------------------------------------------------------------------ 
END FUNCTION NumaForceTerm
!------------------------------------------------------------------------------


