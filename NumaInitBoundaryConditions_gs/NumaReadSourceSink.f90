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

