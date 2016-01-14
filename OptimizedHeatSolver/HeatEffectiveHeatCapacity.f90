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
MODULE HeatEffectiveHeatCapacity
 CONTAINS
  SUBROUTINE EffectiveHeatCapacity(n, Temperature, PrevTemperature, TempPerm, Element, LocalTemperature, &
              HeatCapacity, Density, Enthalpy, Material, PhaseModel, PhaseSpatial, TransientSimulation, &
              MeshChanged)
        USE DefUtils
        USE MaterialModels
        USE HeatPhaseDefs
        IMPLICIT NONE

        TYPE(ValueList_t), POINTER :: Material
        REAL(KIND=dp), POINTER :: Temperature(:), PrevTemperature(:)
        INTEGER :: n
        CHARACTER(LEN=MAX_NAME_LEN) :: PhaseModel
        LOGICAL :: PhaseSpatial
        INTEGER, POINTER :: TempPerm(:)
        TYPE(Element_t), POINTER :: Element
        REAL(KIND=dp), ALLOCATABLE :: HeatCapacity(:), Enthalpy(:), LocalTemperature(:), &
            Density(:)
        LOGICAL :: TransientSimulation, MeshChanged

        LOGICAL :: Found, Specific, AllocationsDone = .FALSE.
        INTEGER :: PhaseChangeModel
        REAL(KIND=dp), ALLOCATABLE :: Work(:), dT(:)
        REAL(KIND=dp) :: s

        SAVE :: AllocationsDone, Work, dT

  !------------------------------------------------------------------------------
  !     See if temperature gradient indside the element is large enough
  !     to use  the c_p = SQRT( (dH/dx)^2 / (dT/dx)^2 ), otherwise
  !     use c_p = dH/dT, or if in time dependent simulation, use
  !     c_p = (dH/dt) / (dT/dt), if requested.
  !------------------------------------------------------------------------------

        IF ( .NOT. AllocationsDone .OR. MeshChanged ) THEN
            CALL Allocations()
        END IF

        SELECT CASE(PhaseModel)
  !------------------------------------------------------------------------------
          CASE( 'spatial 1' )
            PhaseChangeModel = PHASE_SPATIAL_1
  !------------------------------------------------------------------------------

          CASE( 'spatial 2' )
  !------------------------------------------------------------------------------
  ! Check if local variation of temperature is large enough to actually use the
  ! Spatial 2 model. Should perhaps be scaled to element size (or actually
  ! compute the gradient, but this will do for now...).
  !------------------------------------------------------------------------------
            s = MAXVAL(LocalTemperature(1:n))-MINVAL(LocalTemperature(1:n))
            IF ( s < AEPS ) THEN
              PhaseChangeModel = PHASE_SPATIAL_1
            ELSE
              PhaseChangeModel = PHASE_SPATIAL_2
            END IF

  !------------------------------------------------------------------------------
  ! Note that here HeatCapacity is miused for saving dT.
  !------------------------------------------------------------------------------
          CASE('temporal')
            IF ( TransientSimulation )  THEN
              dT(1:n) = Temperature(TempPerm(Element % NodeIndexes)) - &
                       PrevTemperature(TempPerm(Element % NodeIndexes))

              IF ( ANY(ABS(dT(1:n)) < AEPS) ) THEN
                PhaseChangeModel = PHASE_SPATIAL_1
              ELSE
                PhaseChangeModel = PHASE_TEMPORAL
              END IF
            ELSE
               PhaseChangeModel = PHASE_SPATIAL_1
            END IF

  !------------------------------------------------------------------------------
          CASE DEFAULT
            PhaseChangeModel = PHASE_SPATIAL_1

        END SELECT
  !------------------------------------------------------------------------------

        PhaseSpatial = ( PhaseChangeModel == PHASE_SPATIAL_2 )
        Specific = ListCheckPresent( Material,'Specific Enthalpy')

  !-----------------------------------------------------------------------------
        SELECT CASE( PhaseChangeModel )

  !------------------------------------------------------------------------------
  ! This phase change model is available only for some type of real entries
  ! that have an implemented analytical derivation rule.
  !-----------------------------------------------------------------------------
        CASE( PHASE_SPATIAL_1 )
          HeatCapacity(1:n) = ListGetReal( Material, &
               'Effective Heat Capacity', n,Element % NodeIndexes, Found )
          IF ( .NOT. Found ) THEN
            IF( Specific ) THEN
              HeatCapacity(1:n) = ListGetDerivValue( Material, &
                  'Specific Enthalpy', n,Element % NodeIndexes )
              HeatCapacity(1:n) = Density(1:n) * HeatCapacity(1:n)
            ELSE
              HeatCapacity(1:n) = ListGetDerivValue( Material, &
                  'Enthalpy', n,Element % NodeIndexes )
            END IF
          END IF

  !---------------------------------------------------------------------------------------
  ! Note that for the 'spatial 2' model the evaluation of c_p is done in each integration
  ! point and thus Enthalphy and PhaseSpatial flag are used instead of HeatCapacity directly.
  !-----------------------------------------------------------------------------------------
        CASE( PHASE_SPATIAL_2 )
          IF( Specific ) THEN
            Enthalpy(1:n) = ListGetReal(Material,'Specific Enthalpy',n,Element % NodeIndexes)
            Enthalpy(1:n) = Density(1:n) * Enthalpy(1:n)
          ELSE
            Enthalpy(1:n) = ListGetReal(Material,'Enthalpy',n,Element % NodeIndexes)
          END IF

  !------------------------------------------------------------------------------
        CASE( PHASE_TEMPORAL )
          ! When retrieving the value of enthalphy on the previous timestep
          ! the relevant entries of the Temperature solution in the global vector
          ! are tampered in order to make the ListGetReal command work as wanted.
          ! 1) Values at current temperature
          !------------------------------------------------------------------------
          IF( Specific ) THEN
            Work(1:n) = ListGetReal( Material,'Specific Enthalpy',n,Element % NodeIndexes )
          ELSE
            Work(1:n) = ListGetReal( Material,'Enthalpy',n,Element % NodeIndexes )
          END IF

          ! 2) Values at previous temperature
          Temperature(TempPerm(Element % NodeIndexes)) = &
              PrevTemperature(TempPerm(Element % NodeIndexes))

          IF( Specific ) THEN
            Work(1:n) = Work(1:n) - ListGetReal( Material,'Specific Enthalpy', &
                n,Element % NodeIndexes )
            HeatCapacity(1:n) = Density(1:n) * Work(1:n) / dT(1:n)
         ELSE
            Work(1:n) = Work(1:n) - ListGetReal( Material,'Enthalpy', &
                n,Element % NodeIndexes )
            HeatCapacity(1:n) = Work(1:n) / dT(1:n)
          END IF

          ! Revert to current temperature
          Temperature(TempPerm(Element % NodeIndexes)) = &
              PrevTemperature(TempPerm(Element % NodeIndexes)) + dT(1:n)

  !------------------------------------------------------------------------------
        END SELECT
  !------------------------------------------------------------------------------
   CONTAINS
      SUBROUTINE Allocations
          INTEGER :: istat

          IF (AllocationsDone) THEN
              DEALLOCATE( &
                  Work, &
                  dT &
              )
          END IF

          ALLOCATE( &
              Work(N), &
              dT(N), &
              STAT=istat &
          )

          IF ( istat /= 0 ) THEN
            CALL Fatal( 'HeatSolve', 'Memory allocation error' )
          END IF

          AllocationsDone = .TRUE.
      END SUBROUTINE Allocations
  END SUBROUTINE EffectiveHeatCapacity
END MODULE
