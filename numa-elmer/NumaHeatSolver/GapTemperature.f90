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
    FUNCTION GapTemperature( Solver, Element, ElementNodes, Temperature, TempPerm, n ) RESULT(T)
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Return element's neighbourhood temperature 
!
!  ARGUMENTS:
!
!  TYPE(Solver_t) :: Solver
!     INPUT: Linear equation solver options
!
!  TYPE(Element_t) :: Element
!   INPUT: Structure describing the element (dimension,nof nodes,
!               interpolation degree, etc...)
!
!  REAL(KIND=dp) :: Temperature(:)
!     INPUT: Nodal values of temperature from previous iteration
!
!   INTEGER :: TempPerm(:)
!       INPUT: Element local numbering
!
!  INTEGER :: n
!   INPUT: Number of element nodes
!
!******************************************************************************
    USE MaterialModels
    USE DefUtils
    USE NumaAdaptive
!------------------------------------------------------------------------------
    IMPLICIT NONE
        TYPE(Solver_t) :: Solver
        TYPE(Element_t), POINTER :: Element
        INTEGER :: TempPerm(:), n
        REAL(KIND=dp) :: Temperature(:)
        TYPE(Nodes_t)   :: ElementNodes

!------------------------------------------------------------------------------
!     Local variables
!------------------------------------------------------------------------------
        TYPE(Element_t), POINTER :: Parent, Left, Right
        INTEGER :: i,j,k
        REAL(KIND=dp) :: x0,y0,z0,x,y,z,T(n)
!------------------------------------------------------------------------------
!       Initialization
!------------------------------------------------------------------------------
        T = 0.0d0
!------------------------------------------------------------------------------
!       Get the neighbourood information of the boundary element
!------------------------------------------------------------------------------     
        Left  => Element % BoundaryInfo % Left
        Right => Element % BoundaryInfo % Right

        IF ( .NOT. ASSOCIATED(Left) .OR. .NOT.ASSOCIATED(Right) ) RETURN
!------------------------------------------------------------------------------
!       Go through the nodes of the boundary element
!------------------------------------------------------------------------------     
        DO i=1,n
!------------------------------------------------------------------------------
!           Get the neighbour of the boundary element which is in the other side of
!           the boundary (may not be defined)
!------------------------------------------------------------------------------     
            Parent => Left
            k = Element % NodeIndexes(i)
            IF ( ANY( Parent % NodeIndexes == k ) ) Parent => Right
!------------------------------------------------------------------------------
!           Get the coordinates of the nodes of the boundary element
!------------------------------------------------------------------------------
            x0 = ElementNodes % x(i)
            y0 = ElementNodes % y(i)
            z0 = ElementNodes % z(i)
            
            DO j=1,Parent % TYPE % NumberOfNodes
!------------------------------------------------------------------------------
!               Go through the nodes of the Parent element
!------------------------------------------------------------------------------             
                k = Parent % NodeIndexes(j)
!------------------------------------------------------------------------------
!               Compute the distance between the boundary element node and the parent 
!               element node, and check this distance is lower than a fixed threshold
!------------------------------------------------------------------------------                     
                x = Solver % Mesh % Nodes % x(k) - x0
                y = Solver % Mesh % Nodes % y(k) - y0
                z = Solver % Mesh % Nodes % z(k) - z0
                IF ( x**2 + y**2 + z**2 < AEPS ) EXIT
!------------------------------------------------------------------------------                 
            END DO
!------------------------------------------------------------------------------
!           Get the parent element node temperature 
!------------------------------------------------------------------------------ 
            T(i) = Temperature( TempPerm( k ) )
!------------------------------------------------------------------------------             
        END DO
!------------------------------------------------------------------------------
    END FUNCTION GapTemperature
!------------------------------------------------------------------------------
