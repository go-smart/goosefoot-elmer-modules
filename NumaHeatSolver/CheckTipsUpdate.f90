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

FUNCTION CheckTipsUpdate(Model, Time) RESULT(RecalculateElectricTips)
    USE MaterialModels
    USE DefUtils
    USE NumaAdaptive
    IMPLICIT NONE

    TYPE(Model_t)  :: Model
    REAL(KIND=dp) :: Time
    LOGICAL :: RecalculateElectricTips

    INTEGER :: nbtips, istat, NbTipsLocationTimes,TipsLocationTimesIndex=2, &
        OldTipsLocationTimesIndex
    REAL(KIND=dp),ALLOCATABLE :: TipsLocationTimesArray(:,:)
    LOGICAL :: MultiTipsLocation = .FALSE., Found = .FALSE.

    SAVE TipsLocationTimesIndex

    !------------------------------------------------------------------------------ 
    ! Check if the electric tips location change over time:
    !------------------------------------------------------------------------------
    RecalculateElectricTips = .FALSE.
    MultiTipsLocation = GetLogical( Model % Simulation, &
        'Multi Electric Tips Location',Found )
    !------------------------------------------------------------------------------
    IF(MultiTipsLocation) THEN
    !------------------------------------------------------------------------------
        ! Get the number of different locations:
        !------------------------------------------------------------------------------
        NbTipsLocationTimes = GetInteger( Model % Simulation, &
            'Electric Tips Location Times Nb', Found )
        !TODO check that this has fixed the unallocated read valgrind shows on 1233
        IF (.NOT. Found) THEN
            NbTipsLocationTimes = 1
        END IF
        PRINT *, NbTipsLocationTimes, TipsLocationTimesIndex, "<="
        !------------------------------------------------------------------------------
        ! Get the times at which locations change:
        !------------------------------------------------------------------------------
        ALLOCATE(TipsLocationTimesArray(NbTipsLocationTimes,1),STAT=istat)
        TipsLocationTimesArray = ListGetConstRealArray( Model % Simulation, &
            'Multi Electric Tips Location Times', Found )
        !------------------------------------------------------------------------------
        ! Check if a new location has to be considered:
        !------------------------------------------------------------------------------
        OldTipsLocationTimesIndex = TipsLocationTimesIndex
        DO WHILE(Time>TipsLocationTimesArray(TipsLocationTimesIndex,1) .AND. TipsLocationTimesIndex<=NbTipsLocationTimes)
            TipsLocationTimesIndex = TipsLocationTimesIndex + 1
        END DO
        IF(OldTipsLocationTimesIndex /= TipsLocationTimesIndex) THEN
            RecalculateElectricTips =.TRUE.
        END IF
        !------------------------------------------------------------------------------
        DEALLOCATE(TipsLocationTimesArray)
    !------------------------------------------------------------------------------
    END IF
END FUNCTION CheckTipsUpdate
