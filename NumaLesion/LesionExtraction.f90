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
! Author: Panchatcharam Mariappan
!
FUNCTION LesionExtract( Model, n, T ) RESULT(Lesion)
      USE DefUtils
      IMPLICIT None
      TYPE(Model_t) :: Model
      TYPE(Variable_t),POINTER :: LesionPar
      INTEGER :: n
      INTEGER,POINTER :: LesionPerm(:)
      LOGICAL :: Found
      TYPE(ValueList_t), POINTER :: Material
      REAL(KIND=dp) ::  Lesion,T(2),LesionMin
      LOGICAL :: Freezable

      ! Stores previous temp
      LesionPar=>VariableGet(Model%Variables,"LesionPrevious")
      Material => GetMaterial(Model % CurrentElement, Found)

      IF (.NOT. Found) THEN
          Freezable = .TRUE.
      ELSE
          Freezable = GetLogical(Material, "Freezable", Found)
          IF (.NOT. Found .OR. Model % CurrentElement % TYPE % dimension < 3) Freezable = .TRUE.
      END IF

      LesionPerm=>LesionPar%Perm

      ! If a material is specifically marked as unfreezable,
      ! we set it to 0C or above
      IF (Freezable) THEN
          ! T(2) is time
          IF(T(2)>2) THEN
            LesionMin=LesionPar%Values(LesionPerm(n))      
            ! T(1) is temperature
            Lesion=MIN(LesionMin,T(1))
          ELSE
            Lesion=T(1)
          ENDIF
      ELSE
          Lesion = MAX(273.15, T(1))
      END IF

      LesionPar%Values(LesionPerm(n))=Lesion
END FUNCTION LesionExtract
