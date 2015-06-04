!/*****************************************************************************/
! *
! *  Elmer, A Finite Element Software for Multiphysical Problems
! *
! *  Copyright 1st April 1995 - , CSC - IT Center for Science Ltd., Finland
! *
! *  (NUMA: altered from original GPLv2-or-later)
! *  This program is free software: you can redistribute it and/or modify
! *  it under the terms of the GNU General Public License as published by
! *  the Free Software Foundation, either version 3 of the License, or
! *  (at your option) any later version.
! *
! *  This program is distributed in the hope that it will be useful,
! *  but WITHOUT ANY WARRANTY; without even the implied warranty of
! *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
! *  GNU General Public License for more details.
! *
! *  You should have received a copy of the GNU General Public License
! *  along with this program. If not, see <http://www.gnu.org/licenses/>.
! *****************************************************************************/
!
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
SUBROUTINE Transform( Model,Solver,dt,TransientSimulation )
  USE DefUtils
  USE LinearAlgebra

  IMPLICIT NONE

  TYPE(Solver_t) :: Solver
  TYPE(Model_t) :: Model

  REAL(KIND=dp) :: dt, minval
  INTEGER :: t
  LOGICAL :: TransientSimulation, Backward, Found, DoExtrapolate

  TYPE(Mesh_t), POINTER :: Mesh
  TYPE(Variable_t), POINTER :: Var

  REAL(KIND=dp), DIMENSION(4,4), TARGET :: AffineTransformation
  REAL(KIND=dp), POINTER :: AffineBackTransformation(:,:)
  REAL(KIND=dp), DIMENSION(9,1) :: TransformationVector
  REAL(KIND=dp), DIMENSION(3,1) :: Translation

  Mesh => GetMesh()

  AffineTransformation = 0._dp
  AffineTransformation(4,4) = 1._dp

  Backward = GetLogical(GetSolverParams(), 'Backward', Found)
  IF (.NOT. Found) THEN
      Backward = .FALSE.
  END IF

  TransformationVector = ListGetConstRealArray(GetSolverParams(), 'Transformation Matrix', Found)
  !ERROR IF NOT
  AffineTransformation(1:3,1:3) = RESHAPE(TransformationVector, (/3,3/))

  Translation = ListGetConstRealArray(GetSolverParams(), 'Translation', Found)
  !ERROR IF NOT
  AffineTransformation(4,1:3) = Translation(1:3,1)

  IF (Backward) THEN
      CALL InvertMatrix(AffineTransformation, 4)
  END IF

  AffineBackTransformation => AffineTransformation

  CALL InvalidateVariable( CurrentModel % Meshes, Solver % Mesh, &
        GetVarName(Solver % Variable))

  Var => VariableGet( Mesh % Variables, GetString(GetSolverParams(),'Interpolant'), &
      ThisOnly=.TRUE., AffineBackTransformation=AffineBackTransformation)

  IF (ASSOCIATED(Var)) THEN
    Var % Valid = .FALSE.
    Var % ValuesChanged = .TRUE.
  END IF
  Var => VariableGet( Mesh % Variables, GetString(GetSolverParams(),'Interpolant'), &
      AffineBackTransformation=AffineBackTransformation)
  minval = GetCReal(GetSolverParams(), 'Minimum Value', Found)

  IF (Found) THEN
      DO t=1,SIZE(Var % Values) ! RMV
        Var % Values (t) = MAX(minval, Var % Values(t))
      END DO
  END IF

  Solver % Variable % Values = Var % Values
  Solver % Variable % Perm = Var % Perm
END SUBROUTINE Transform

