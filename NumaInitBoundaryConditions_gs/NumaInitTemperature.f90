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

