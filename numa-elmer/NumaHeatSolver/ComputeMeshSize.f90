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
FUNCTION ComputeMeshSize( Solver, Model ) RESULT(MeshSize)
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Return mesh average size, defined as the average distance between two nodes
!
!  ARGUMENTS:
!
!  TYPE(Solver_t) :: Solver
!     INPUT:
!
!******************************************************************************
        USE MaterialModels
        USE DefUtils
        USE NumaAdaptive
        IMPLICIT NONE

        TYPE(Solver_t) :: Solver
        TYPE(Model_t) :: Model
        REAL(KIND=dp) :: MeshSize
!------------------------------------------------------------------------------
        INTEGER :: t,i,cpt, N
        INTEGER, POINTER :: NodeIndexes(:)  
        REAL(KIND=dp) :: tmp
        TYPE(Nodes_t) :: Nodes 
        TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
!       Allocations and initializations
!------------------------------------------------------------------------------
        ALLOCATE( Nodes % x( Model % MaxElementNodes ) )
        ALLOCATE( Nodes % y( Model % MaxElementNodes ) )
        ALLOCATE( Nodes % z( Model % MaxElementNodes ) )

        MeshSize = 0.0
!------------------------------------------------------------------------------
!       Two times the number of edges
!------------------------------------------------------------------------------         
        cpt = 0
!------------------------------------------------------------------------------                         
!       Go through the elements of the model (bulk and boundary ones)       
!------------------------------------------------------------------------------                 
        DO t=1, Solver % Mesh % NumberOfBulkElements + Solver % Mesh % NumberOfBoundaryElements
!------------------------------------------------------------------------------                     
            Element =>  Solver % Mesh % Elements(t)
            !------------------------------------------------------------------------------
            ! Get the indexes of the element nodes:
            !------------------------------------------------------------------------------
            NodeIndexes => Element % NodeIndexes
            !------------------------------------------------------------------------------
            ! Get the number of element nodes:
            !------------------------------------------------------------------------------
            n = Element % TYPE % NumberOfNodes
            !------------------------------------------------------------------------------
            ! Get the coordinates of the element nodes:
            !------------------------------------------------------------------------------
            Nodes % x(1:n) = Model % Nodes % x( NodeIndexes )
            Nodes % y(1:n) = Model % Nodes % y( NodeIndexes )
            Nodes % z(1:n) = Model % Nodes % z( NodeIndexes )
            !------------------------------------------------------------------------------
            ! All elements (no condition on n)
            !------------------------------------------------------------------------------
            DO i =1,n-1
                !------------------------------------------------------------------------------
                ! Square of the distance between nodes
                !------------------------------------------------------------------------------
                tmp =  &
                    (Nodes % x(i+1)-Nodes % x(i))*(Nodes % x(i+1)-Nodes % x(i)) + &
                    (Nodes % y(i+1)-Nodes % y(i))*(Nodes % y(i+1)-Nodes % y(i)) + &
                    (Nodes % z(i+1)-Nodes % z(i))*(Nodes % z(i+1)-Nodes % z(i)) 
                !------------------------------------------------------------------------------
                ! Update sum of distances
                !------------------------------------------------------------------------------ 
                MeshSize = MeshSize +sqrt(tmp)
                !------------------------------------------------------------------------------
                ! Increment the number of edges
                !------------------------------------------------------------------------------ 
                cpt = cpt + 1
            END DO
            !------------------------------------------------------------------------------
            ! Trianglular and tetrahedral elements
            !------------------------------------------------------------------------------
            IF (n>=3) THEN
                !------------------------------------------------------------------------------
                ! Square of the distance between nodes
                !------------------------------------------------------------------------------
                tmp =  &
                    (Nodes % x(1)-Nodes % x(n))*(Nodes % x(1)-Nodes % x(n)) + &
                    (Nodes % y(1)-Nodes % y(n))*(Nodes % y(1)-Nodes % y(n)) + &
                    (Nodes % z(1)-Nodes % z(n))*(Nodes % z(1)-Nodes % z(n)) 
                !------------------------------------------------------------------------------
                ! Update sum of distances
                !------------------------------------------------------------------------------ 
                MeshSize = MeshSize +sqrt(tmp)
                !------------------------------------------------------------------------------
                ! Increment the number of edges
                !------------------------------------------------------------------------------ 
                cpt = cpt + 1
            END IF
            !------------------------------------------------------------------------------
            ! Tetrahedral elements
            !------------------------------------------------------------------------------
            IF (n==4) THEN
                !------------------------------------------------------------------------------
                ! Square of the distance between nodes
                !------------------------------------------------------------------------------
                DO i =1,n-2
                    tmp =  &
                        (Nodes % x(i+2)-Nodes % x(i))*(Nodes % x(i+2)-Nodes % x(i)) + &
                        (Nodes % y(i+2)-Nodes % y(i))*(Nodes % y(i+2)-Nodes % y(i)) + &
                        (Nodes % z(i+2)-Nodes % z(i))*(Nodes % z(i+2)-Nodes % z(i)) 
                    !------------------------------------------------------------------------------
                    ! Update sum of distances
                    !------------------------------------------------------------------------------ 
                    MeshSize = MeshSize +sqrt(tmp)
                    !------------------------------------------------------------------------------
                    ! Increment the number of edges
                    !------------------------------------------------------------------------------ 
                    cpt = cpt + 1
                END DO
            END IF
!------------------------------------------------------------------------------                     
        END DO ! t
!------------------------------------------------------------------------------
!       Compute the average distance between nodes
!------------------------------------------------------------------------------             
        MeshSize =MeshSize / cpt
        !PRINT *,'meshsize= ',MeshSize,'cpt= ',cpt
!------------------------------------------------------------------------------                     
        DEALLOCATE( Nodes % x, Nodes % y, Nodes % z)
!------------------------------------------------------------------------------
    END FUNCTION ComputeMeshSize
!------------------------------------------------------------------------------
