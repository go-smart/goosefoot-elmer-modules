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
SUBROUTINE HeatSolver_init( Model,Solver,Timestep,TransientSimulation )
     USE DefUtils

     IMPLICIT NONE
     TYPE(Solver_t) :: Solver  
     TYPE(Model_t) :: Model    
     REAL(KIND=dp) :: Timestep
     LOGICAL :: TransientSimulation 
!------------------------------------------------------------------------------
!    Local variables
!------------------------------------------------------------------------------
     TYPE(ValueList_t),POINTER :: SolverParams
     LOGICAL :: Found, VisualizePerfusion

     SolverParams => GetSolverParams()

     VisualizePerfusion = GetLogical( SolverParams,'Perfusion Visualization',Found )
     IF ( Found .AND. VisualizePerfusion ) THEN
         CALL ListAddString( SolverParams,&
              NextFreeKeyword('Exported Variable',SolverParams),'Perfusion')
     END IF
     CALL ListAddString( SolverParams,&
          NextFreeKeyword('Exported Variable',SolverParams),'Deposition')
END SUBROUTINE HeatSolver_init

SUBROUTINE HeatSolver(Model, Solver, Timestep, TransientSimulation)
      USE HeatSolve

     IMPLICIT NONE
!------------------------------------------------------------------------------
     TYPE(Model_t) :: Model
     TYPE(Solver_t) :: Solver
     REAL(KIND=dp) :: Timestep
     LOGICAL :: TransientSimulation

      CALL SolverMain(Model, Solver, Timestep, TransientSimulation)
END SUBROUTINE HeatSolver
