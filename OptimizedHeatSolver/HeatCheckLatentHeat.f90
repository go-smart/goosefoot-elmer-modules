! *****************************************************************************/
! *  This file is based on the file "HeatSolve.f90" of Elmer (Finite Element 
! *  Software for Multiphysical Problems)
! *  Authors: Juha Ruokolainen
! *  Copyright 1st April 1995 - , CSC - Scientific Computing Ltd., Finland
! *  The original file is relicensed, as permitted by the authors, as GPLv3
! *****************************************************************************/
! /**
!  * Modifications to this file are part of the Go-Smart Simulation Architecture (GSSA).
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
!  *
!  * These alterations may be re-licensed under the terms of the GNU General
!  * Public License version 2 (or later version) ONLY for inclusion in the
!  * publicly available Elmer project by CSC, except by express consent of
!  * NUMA Engineering Services Ltd.
!  */
! 
MODULE HeatCheckLatentHeat
 CONTAINS
    FUNCTION CheckLatentHeat(Solver, Model, TempPerm, Temperature, PrevSolution) RESULT(Failure)
      USE MaterialModels
      USE DefUtils
      USE NumaAdaptive
      IMPLICIT NONE

!------------------------------------------------------------------------------
      TYPE(Solver_t) :: Solver
      TYPE(Model_t) :: Model
      INTEGER :: TempPerm(:)
      REAL(KIND=dp), POINTER :: Temperature(:), PrevSolution(:)
      LOGICAL :: Failure
!------------------------------------------------------------------------------
      TYPE(Element_t), POINTER :: Element
      TYPE(ValueList_t), POINTER :: Material
      LOGICAL :: PhaseChange, CheckLatentHeatRelease, Found
      INTEGER, POINTER :: NodeIndexes(:)
      REAL(KIND=dp), POINTER :: PhaseChangeIntervals(:,:)
      INTEGER :: t, eq_id, body_id, n, i, j, k
      CHARACTER(LEN=MAX_NAME_LEN) :: PhaseModel
!------------------------------------------------------------------------------

      Failure = .FALSE.
!------------------------------------------------------------------------------
      DO t=1,Solver % Mesh % NumberOfBulkElements
!------------------------------------------------------------------------------
!       Check if this element belongs to a body where temperature 
!       has been calculated
!------------------------------------------------------------------------------
        Element => Solver % Mesh % Elements(t)

        NodeIndexes => Element % NodeIndexes
        IF ( ANY( TempPerm( NodeIndexes ) <= 0 ) ) CYCLE

        body_id = Element % Bodyid
        eq_id = ListGetInteger( Model % Bodies(body_id) % Values, &
            'Equation', minv=1, maxv=Model % NumberOfEquations )

        PhaseModel = ListGetString( Model % Equations(eq_id) % Values, &
                          'Phase Change Model',Found )

        PhaseChange = Found .AND. (PhaseModel(1:4) /= 'none')

        IF ( PhaseChange ) THEN
          CheckLatentHeatRelease = ListGetLogical(Model % Equations(eq_id) % &
                    Values, 'Check Latent Heat Release',Found )
        END IF
        IF ( .NOT. ( PhaseChange .AND. CheckLatentHeatRelease ) ) CYCLE

        n = Element % TYPE % NumberOfNodes
!------------------------------------------------------------------------------
!       Set the current element pointer in the model structure to
!       reflect the element being processed
!------------------------------------------------------------------------------
        Model % CurrentElement => Element
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!       Get element material parameters
!------------------------------------------------------------------------------
        k = ListGetInteger( Model % Bodies(body_id) % Values,'Material', &
                minv=1, maxv=Model % NumberOfMaterials )
        Material => Model % Materials(k) % Values

        PhaseChangeIntervals => ListGetConstRealArray( Material, &
                        'Phase Change Intervals' )

        DO k=1,n
          i = TempPerm( NodeIndexes(k) )
          DO j=1,SIZE(PhaseChangeIntervals,2)
            IF ( ( Temperature(i)  < PhaseChangeIntervals(1,j) .AND. &
                   PrevSolution(i) > PhaseChangeIntervals(2,j) ).OR. &
                 ( Temperature(i)  > PhaseChangeIntervals(2,j) .AND. &
                   PrevSolution(i) < PhaseChangeIntervals(1,j) )  ) THEN
              Failure = .TRUE.
              EXIT
            END IF
          END DO
          IF ( Failure ) EXIT
        END DO
        IF ( Failure ) EXIT
      END DO
!------------------------------------------------------------------------------
    END FUNCTION CheckLatentHeat
END MODULE HeatCheckLatentHeat
