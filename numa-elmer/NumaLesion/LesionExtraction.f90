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
FUNCTION LesionExtract( Model, n, T ) RESULT(Lesion)
      USE DefUtils
      IMPLICIT None
      TYPE(Model_t) :: Model
      TYPE(Variable_t),POINTER :: LesionPar
      INTEGER :: n
      INTEGER,POINTER :: LesionPerm(:)      
      REAL(KIND=dp) ::  Lesion,T(2),LesionMin
!      REAL(KIND=dp), POINTER:: LesionMin(:)

      LesionPar=>VariableGet(Model%Variables,"LesionPrevious")
      LesionPerm=>LesionPar%Perm
      IF(T(2)>2) THEN
        LesionMin=LesionPar%Values(LesionPerm(n))      
        Lesion=MIN(LesionMin,T(1))
      ELSE
        Lesion=T(1)
      ENDIF
      LesionPar%Values(LesionPerm(n))=Lesion
END FUNCTION LesionExtract
