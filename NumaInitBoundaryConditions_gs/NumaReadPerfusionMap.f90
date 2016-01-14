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

