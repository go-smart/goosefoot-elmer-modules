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
    FUNCTION NumaDistanceToElectricTip (x, y, z, xtip, ytip, ztip, nbpoints, xproj, yproj, zproj ) RESULT( Distance )
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

    USE MaterialModels
    USE DefUtils
    USE NumaAdaptive
    IMPLICIT NONE

        REAL(KIND=dp) :: Distance, x, y, z, xtip(nbpoints), ytip(nbpoints), ztip(nbpoints), &
            xproj, yproj, zproj
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
            xproj =xtip(1)
            yproj =ytip(1)
            zproj =ztip(1)
        !------------------------------------------------------------------------------ 
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
                        xproj =x_s
                        yproj =y_s
                        zproj =z_s
                    END IF
                    s = j*1.0/(S_MAX-1)
                !------------------------------------------------------------------------------
                END DO
            !------------------------------------------------------------------------------
            END DO
        !------------------------------------------------------------------------------
        END IF
!------------------------------------------------------------------------------ 
    END FUNCTION NumaDistanceToElectricTip
!------------------------------------------------------------------------------
