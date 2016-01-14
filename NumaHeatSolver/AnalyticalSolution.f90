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


!------------------------------------------------------------------------------
    FUNCTION AnalyticalSolution( x, y, z ) RESULT(T)
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Return the analytical temperature for some simple problems (used in error computation) 
!
!  ARGUMENTS:
!
!  REAL(KIND=dp) :: x, y, z
!     INPUT: coordinates
!
!******************************************************************************
    USE MaterialModels
    USE DefUtils
    USE NumaAdaptive
!------------------------------------------------------------------------------
    IMPLICIT NONE
        REAL(KIND=dp) :: x, y, z, T
!------------------------------------------------------------------------------
        ! Diffusion on a square with dirichlet conditions only:
        !T = sinh(pi*y)*sin(pi*x)/sinh(pi)
        
        ! Diffusion on a square with dirichlet and Neumann conditions:
        T = -2*cosh(pi)*cos(2*pi*(x-0.5))*sinh(2*pi*(y-0.5))/sinh(2*pi)
        
        ! Advection-diffusion on a square with dirichlet and Neumann conditions:
        !T = (exp(20.0)-2+exp(20.0*y))/(exp(20.0)-1)
!------------------------------------------------------------------------------
    END FUNCTION AnalyticalSolution
!------------------------------------------------------------------------------
