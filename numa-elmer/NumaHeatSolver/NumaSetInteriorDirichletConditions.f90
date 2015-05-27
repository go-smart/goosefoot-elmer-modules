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
    SUBROUTINE NumaSetInteriorDirichletConditions( Model, A, b, Name, DOF, NDOFs, Perm )
!------------------------------------------------------------------------------
!******************************************************************************
!
! Set dirichlet boundary condition for non boundary elements
!
! TYPE(Model_t) :: Model
!   INPUT: the current model structure
!
! TYPE(Matrix_t), POINTER :: A
!   INOUT: The global stiff matrix
!
! REAL(KIND=dp) :: b
!   INOUT: The global RHS vector
! 
! CHARACTER(LEN=*) :: Name
!   INPUT: name of the dof to be set
!
! INTEGER :: DOF, NDOFs
!   INPUT: The order number of the dof and the total number of DOFs for
!          this equation
!
! INTEGER :: Perm(:)
!   INPUT: The node reordering info, this has been generated at the
!          beginning of the simulation for bandwidth optimization
!******************************************************************************
!------------------------------------------------------------------------------
        USE MaterialModels
        USE DefUtils
        USE NumaAdaptive
        IMPLICIT NONE

        TYPE(Model_t) :: Model
        TYPE(Matrix_t), POINTER :: A
        REAL(KIND=dp) :: b(:)
        CHARACTER(LEN=*) :: Name 
        INTEGER :: DOF, NDOFs, Perm(:)
!------------------------------------------------------------------------------
!       Local variables
!------------------------------------------------------------------------------
        TYPE(ValueList_t), POINTER :: ValueList
        
        INTEGER, POINTER :: NodeIndexes(:), IndNodes(:)
        INTEGER :: BC,i,j,n, NoNodes, NOFNodesFound, dim, Nb_Target_Spherical
        
        LOGICAL :: GotIt, NodesFound, Interior, Target_spherical, Interior1, Interior2
        REAL(KIND=dp) ::  min_x, min_y, max_x, max_y,min_z,max_z, dist
        REAL(KIND=dp), POINTER :: c_x_Array(:,:), c_y_Array(:,:), c_z_Array(:,:), &
            radius_Array(:,:)
!------------------------------------------------------------------------------
!       Dimension of the model
!------------------------------------------------------------------------------     
        dim=CoordinateSystemDimension()
!------------------------------------------------------------------------------
!     Go through the BCs 
!------------------------------------------------------------------------------
        DO BC=1,Model % NumberOfBCs
!------------------------------------------------------------------------------ 
!           Check if the variable is concerned by this BC
!------------------------------------------------------------------------------      
            IF( .NOT. ListCheckPresent( Model % BCs(BC) % Values,Name )) CYCLE
!------------------------------------------------------------------------------         
            NodesFound = .FALSE.
!------------------------------------------------------------------------------
!           The areas in which Dirichlet conditions have to be applied are defined either 
!           in terms of coordinates or in terms of nodes.
!           At the first calling the list of coordinates is transformed to a list of nodes 
!------------------------------------------------------------------------------
            IF(.NOT. NodesFound) THEN
!------------------------------------------------------------------------------            
                ALLOCATE( IndNodes(Model % NumberOfNodes) )
                IndNodes = -1
                NoNodes=0
                GotIt = .FALSE.
!------------------------------------------------------------------------------ 
!               Check if the target zone is spherical 
!------------------------------------------------------------------------------                 
                Target_spherical = GetLogical( Model % BCs(BC) % Values,'Target Spherical',GotIt )      
                IF ( .NOT. GotIt ) Target_spherical = .FALSE.
                IF(Target_spherical) THEN
!------------------------------------------------------------------------------ 
!                   Check if several spherical zones are treated:
!------------------------------------------------------------------------------ 
                    Nb_Target_Spherical = GetInteger( Model % BCs(BC) % Values,'Nb Target Spherical',GotIt )        
                    IF ( .NOT. GotIt ) THEN 
                        Nb_Target_Spherical = 1
                    ELSE
                        IF ( Nb_Target_Spherical < 1 ) THEN 
                            PRINT*,'Nb Target Spherical = ',Nb_Target_Spherical
                            CALL Fatal( 'NumaSetInteriorDirichletConditions', 'Check Nb Target Spherical!' )
                        END IF
                    END IF
                    ALLOCATE( c_x_Array(Nb_Target_Spherical,1), c_y_Array(Nb_Target_Spherical,1), &
                        c_z_Array(Nb_Target_Spherical,1), radius_Array(Nb_Target_Spherical,1) )
!------------------------------------------------------------------------------ 
!                   Read the centre coordinates and radius for spherical areas in which a 
!                   Dirichlet BC has to be set
!                   These areas are then defined by
!                   (x-x_c)^2 + (y-y_c)^2 + (z-z_c)^2 < radius^2
!------------------------------------------------------------------------------                     
                    min_x=1
                    max_x=0
                    min_y=1
                    max_y=0
                    min_z=1
                    max_z=0
!------------------------------------------------------------------------------                                 
                    c_x_Array => ListGetConstRealArray( Model % BCs(BC) % Values, 'Target CentreX', GotIt )
                    IF (.NOT. GotIt) CYCLE
                    
                    c_y_Array => ListGetConstRealArray( Model % BCs(BC) % Values, 'Target CentreY', GotIt )
                    IF (.NOT. GotIt) CYCLE

                    radius_Array => ListGetConstRealArray( Model % BCs(BC) % Values, 'Target Radius', GotIt )
                    IF (.NOT. GotIt) CYCLE

                    IF (dim>2) THEN
                        c_z_Array => ListGetConstRealArray( Model % BCs(BC) % Values, 'Target CentreZ', GotIt )
                        IF (.NOT. GotIt) CYCLE
                    END IF
!------------------------------------------------------------------------------                     
                ELSE
!------------------------------------------------------------------------------ 
!                   Read the upper/lower bounds of coordinates for areas in which a 
!                   Dirichlet BC has to be set
!                   These areas are then defined by
!                   min_x < x < max_x, min_y < y < max_y, min_z < z < max_z
!------------------------------------------------------------------------------                                             
                    min_x =  ListGetConstReal(  Model % BCs(BC) % Values, 'Target min x',GotIt)
                    IF (.NOT. GotIt) CYCLE
                    min_y =  ListGetConstReal(  Model % BCs(BC) % Values, 'Target min y',GotIt)
                    IF (.NOT. GotIt) CYCLE
                    max_x =  ListGetConstReal(  Model % BCs(BC) % Values, 'Target max x',GotIt)
                    IF (.NOT. GotIt) CYCLE
                    max_y =  ListGetConstReal(  Model % BCs(BC) % Values, 'Target max y',GotIt)
                    IF (.NOT. GotIt) CYCLE
                    IF (dim>2) THEN
                        min_z =  ListGetConstReal(  Model % BCs(BC) % Values, 'Target min z',GotIt)
                        IF (.NOT. GotIt) CYCLE
                        max_z =  ListGetConstReal(  Model % BCs(BC) % Values, 'Target max z',GotIt)
                        IF (.NOT. GotIt) CYCLE
                    END IF
!------------------------------------------------------------------------------
                END IF
!------------------------------------------------------------------------------ 
!               Go through the nodes of the model and check for each if it belongs 
!               to an area with Dirichlet BC
!------------------------------------------------------------------------------             
                DO i=1,Model % NumberOfNodes
!------------------------------------------------------------------------------ 
!                       Check if the element is active 
!------------------------------------------------------------------------------    
                    IF( Perm(i) == 0) CYCLE
                    Interior = .FALSE.
!------------------------------------------------------------------------------ 
!                   Comparisons between node coordinates and area bounds in spherical case
!------------------------------------------------------------------------------     
                    Interior1 = .FALSE.
!------------------------------------------------------------------------------
                    IF(Target_spherical) THEN
!------------------------------------------------------------------------------
                        DO j=1,Nb_Target_Spherical
                            dist = (Model % Mesh % Nodes % x(i)-c_x_Array(j,1))**2 + &
                                (Model % Mesh % Nodes % y(i)-c_y_Array(j,1))**2 
                            IF (dim>2) THEN 
                                dist = dist + (Model % Mesh % Nodes % z(i)-c_z_Array(j,1))**2 
                            END IF
                            Interior1= Interior1 .OR. (dist<=radius_Array(j,1)**2)
                        END DO
!------------------------------------------------------------------------------
                    END IF
!------------------------------------------------------------------------------ 
!                   Comparisons between node coordinates and area bounds in non-spherical case
!------------------------------------------------------------------------------    
                    Interior2= (Model % Mesh % Nodes % x(i)>min_x) .AND. &
                        (Model % Mesh % Nodes % x(i)<max_x)
                    Interior2= Interior2 .AND. (Model % Mesh % Nodes % y(i)>min_y) .AND. &
                        (Model % Mesh % Nodes % y(i)<max_y)
                    IF (dim>2) THEN
                        Interior2= Interior2 .AND. (Model % Mesh % Nodes % z(i)>min_z) .AND. &
                        (Model % Mesh % Nodes % z(i)<max_z)
                    END IF
!------------------------------------------------------------------------------ 
                    Interior = Interior1 .OR. Interior2                 
!------------------------------------------------------------------------------ 
!                   Update the number of nodes for the BC, and the vector of these nodes
!------------------------------------------------------------------------------
                    IF( Interior ) THEN
                        NoNodes = NoNodes+1
                        IndNodes(NoNodes) = i
                    END IF
!------------------------------------------------------------------------------
                END DO ! Model % NumberOfNodes
!------------------------------------------------------------------------------         
!               Deallocate local arrays:
!------------------------------------------------------------------------------
                IF(Target_spherical) DEALLOCATE( c_x_Array, c_y_Array, c_z_Array, radius_Array )
!--------------------------------------------------------------------------     
!               Check if all the selected nodes are active
!------------------------------------------------------------------------------         
                NOFNodesFound = 0
                DO j=1,NoNodes
                    IF ( IndNodes(j)>0 ) THEN
                        NOFNodesFound=NOFNodesFound+1
                        IndNodes(NOFNodesFound) = IndNodes(j)
                    END IF
                END DO
!------------------------------------------------------------------------------ 
!               In the first time add the found nodes to the list structure
!------------------------------------------------------------------------------ 
                IF ( NOFNodesFound > 0 ) THEN
                    CALL ListAddIntegerArray( Model % BCs(BC) % Values,'Target Nodes', &
                        NOFNodesFound, IndNodes) 
                    DEALLOCATE(IndNodes)
                    NodesFound = .TRUE.               
                END IF  
!------------------------------------------------------------------------------
            END IF ! NOT NodesFound
!------------------------------------------------------------------------------
!           If the nodes are specified in the input file, or if the coordinates 
!        have been transformed in nodes, we apply the conditions:           
!------------------------------------------------------------------------------
            IF(NodesFound) THEN          
!------------------------------------------------------------------------------     
!               Read the nodes in the input file
!------------------------------------------------------------------------------             
                NodeIndexes => ListGetIntegerArray( Model % BCs(BC) % Values,'Target Nodes')
!------------------------------------------------------------------------------     
!               Get the number of nodes concerned be Dirichlet BC
!------------------------------------------------------------------------------                     
                n = SIZE(NodeIndexes)
!------------------------------------------------------------------------------     
!               Get the fixed values of the Dirichlet BC
!------------------------------------------------------------------------------     
                ValueList => Model % BCs(BC) % Values
!------------------------------------------------------------------------------     
!               Modify the system with the Dirichlet BC
!------------------------------------------------------------------------------     
                CALL NumaSetPointValues(A, b,Name, DOF,NDOFs,ValueList,n,NodeIndexes,Perm)
!------------------------------------------------------------------------------
            END IF ! NodesFound
!------------------------------------------------------------------------------
        END DO ! BC
!------------------------------------------------------------------------------
  END SUBROUTINE NumaSetInteriorDirichletConditions
!------------------------------------------------------------------------------
