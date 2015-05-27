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

!------------------------------------------------------------------------------
! Read the values of electric power over time in a text file (the first time 
! only), save these values and interpolate at each timestep 
!------------------------------------------------------------------------------
FUNCTION NumaReadElectricPower(model, time) RESULT( Power )
!------------------------------------------------------------------------------ 
    USE DefUtils
  USE GeneralUtils
    IMPLICIT None
!------------------------------------------------------------------------------   
    TYPE(Model_t) :: model
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

