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

