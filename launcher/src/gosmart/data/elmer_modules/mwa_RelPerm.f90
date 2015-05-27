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
FUNCTION RelPermNL( Model, n, T ) RESULT(RelPerm)
      USE DefUtils
      IMPLICIT None
      TYPE(Model_t) :: Model
      INTEGER :: n
      REAL(KIND=dp) :: T, C, RelPerm

      C = MAX(T - 273, 37.0)
      RelPerm = 48.391 * (1 - 1 / (1 + exp(0.0764 * (82.271 - C)))) + 1
END FUNCTION RelPermNL
