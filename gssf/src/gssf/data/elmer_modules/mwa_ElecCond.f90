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
FUNCTION ElecCondNL( Model, n, T ) RESULT(ElecCond)
      USE DefUtils
      IMPLICIT None
      TYPE(Model_t) :: Model
      INTEGER :: n
      REAL(KIND=dp) :: T, C, ElecCond

      C = MAX(T - 273, 37.0)
      !C = 37 ! RMV
      ElecCond = 2.173 * (1 - 1 / (1 + exp(0.0697 * (85.375 - C))))
END FUNCTION ElecCondNL
