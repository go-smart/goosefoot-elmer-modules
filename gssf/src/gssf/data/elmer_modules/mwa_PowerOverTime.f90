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
FUNCTION PowerOverTimeNL( Model, n, t ) RESULT(PowerOverTime)
      USE DefUtils
      IMPLICIT None
      TYPE(Model_t) :: Model
      INTEGER :: n
      REAL(KIND=dp) :: t, PowerOverTime

      IF (t < 180) THEN
              PowerOverTime = 100;
      
      ELSEIF (t < 540 ) THEN 
              PowerOverTime = 100;
              
    !  ELSEIF (t < 1000 ) THEN
	!		  PowerOverTime = 96;
			  
	  !ELSEIF (t < 1020 ) THEN
       !   PowerOverTime
      ENDIF
END FUNCTION PowerOverTimeNL
