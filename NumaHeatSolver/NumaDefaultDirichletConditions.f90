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
    SUBROUTINE NumaDefaultDirichletConditions( Solver, Time )
        USE MaterialModels
        USE DefUtils
        USE NumaAdaptive
        IMPLICIT NONE
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Set dirichlet condition on boundary and interior nodes 
!
!   ARGUMENT:
!
!  TYPE(Solver_t) :: Solver
!     INPUT: Linear equation solver options
!
!******************************************************************************
        TYPE(Solver_t), TARGET :: Solver
!------------------------------------------------------------------------------
        TYPE(Matrix_t), POINTER   :: A
        TYPE(Variable_t), POINTER :: x
        TYPE(ValueList_t), POINTER :: BC
        TYPE(Element_t), POINTER :: Element, Parent, Edge, Face, SaveElement
        
        REAL(KIND=dp), POINTER    :: b(:)
        REAL(KIND=dp), ALLOCATABLE :: Work(:), STIFF(:,:)
        REAL(KIND=dp) :: Time
        
        INTEGER, ALLOCATABLE :: lInd(:), gInd(:)
        INTEGER :: i,j, k, kk, l, n,nb, mb, DOF, numEdgeDofs,istat

        LOGICAL :: Found
        
        CHARACTER(LEN=MAX_NAME_LEN) :: name
!------------------------------------------------------------------------------
        SAVE gInd, lInd, STIFF, Work

        INTERFACE
            SUBROUTINE NumaSetDirichletBoundaries( Solver, Model, Time, A, b, Name, DOF, NDOFs, Perm )
                USE MaterialModels
                USE DefUtils
                USE NumaAdaptive
                IMPLICIT NONE

                TYPE(Solver_t) :: Solver
                TYPE(Model_t) :: Model
                TYPE(Matrix_t), POINTER :: A

                REAL(KIND=dp) :: b(:), Time

                CHARACTER(LEN=*) :: Name 
                INTEGER :: DOF, NDOFs, Perm(:)
            END SUBROUTINE
        END INTERFACE
!------------------------------------------------------------------------------
!       Get the linear system components
!------------------------------------------------------------------------------
        A => Solver % Matrix
        x => Solver % Variable
        b => A % RHS
!------------------------------------------------------------------------------
!       Get the maximal number of DOF 
!------------------------------------------------------------------------------
        n = Solver % Mesh % MaxElementDOFs
!------------------------------------------------------------------------------
!       Allocations
!------------------------------------------------------------------------------     
        IF ( .NOT. ALLOCATED( gInd ) ) THEN
            ALLOCATE( gInd(n), lInd(n), STIFF(n,n), Work(n), stat=istat )
            IF ( istat /= 0 ) &
                CALL Fatal('DefUtils::DefaultDirichletBCs','Memory allocation failed.' )
        ELSE IF ( SIZE(gInd) < n ) THEN
            DEALLOCATE( gInd, lInd, STIFF, Work )
            ALLOCATE( gInd(n), lInd(n), STIFF(n,n), Work(n), stat=istat )
            IF ( istat /= 0 ) &
                CALL Fatal('DefUtils::DefaultDirichletBCs','Memory allocation failed.' )
        END IF
!------------------------------------------------------------------------------
!       Special treatment if several DOFs ?
!------------------------------------------------------------------------------
        IF ( x % DOFs > 1 ) THEN
            !RMV : why does this comment say: TEMP!!!
            CALL NumaSetDirichletBoundaries( Solver, CurrentModel, Time, A, b, x % Name,-1,x % DOFs,x % Perm )
            CALL NumaSetInteriorDirichletConditions( CurrentModel, A, b, x % &
                Name, -1, x % DOFs, x % Perm )
        END IF
!------------------------------------------------------------------------------
!       Clear dirichlet BCs for face & edge DOFs:
!------------------------------------------------------------------------------
        DO DOF=1,x % DOFs
!------------------------------------------------------------------------------     
!           Get the name of the DOF:
!------------------------------------------------------------------------------     
            name = x % name
            IF ( x % DOFs > 1 ) name = ComponentName(name,DOF)
!------------------------------------------------------------------------------
!           Go through the boundary elements:
!------------------------------------------------------------------------------
            DO i=1,Solver % Mesh % NumberOfBoundaryElements
!------------------------------------------------------------------------------
                Element => GetBoundaryElement(i)
!------------------------------------------------------------------------------
!               Check if the element is active:
!------------------------------------------------------------------------------
                IF ( .NOT. ActiveBoundaryElement() ) CYCLE
!------------------------------------------------------------------------------
!               Get the BC associated to this element
!               Check if the BC exists
!------------------------------------------------------------------------------
                BC => GetBC()
                IF ( .NOT. ASSOCIATED( BC ) ) CYCLE
                IF ( .NOT. ListCheckPresent(BC, Name) ) CYCLE 
!------------------------------------------------------------------------------
!               Get the parent element: bulk element with common face or edge.
!               Left or right
!------------------------------------------------------------------------------
                Parent => Element % BoundaryInfo % Left
                IF ( .NOT. ASSOCIATED( Parent ) ) THEN
                    Parent => Element % BoundaryInfo % Right
                END IF
                IF ( .NOT. ASSOCIATED( Parent ) ) CYCLE
!------------------------------------------------------------------------------
!               Clear dofs associated with element edges:
!------------------------------------------------------------------------------
                IF ( ASSOCIATED( Solver % Mesh % Edges ) ) THEN
!------------------------------------------------------------------------------             
                    DO j=1,Parent % TYPE % NumberOfEdges
!------------------------------------------------------------------------------                 
                        Edge => Solver % Mesh % Edges( Parent % EdgeIndexes(j) )
                        IF ( Edge % BDOFs == 0 ) CYCLE

                        n = 0
                        DO k=1,Element % TYPE % NumberOfNodes
                            DO l=1,Edge % TYPE % NumberOfNodes
                                IF ( Edge % NodeIndexes(l) == Element % NodeIndexes(k) ) n=n+1
                            END DO
                        END DO

                        IF ( n ==  Edge % Type % NumberOfNodes ) THEN
                            DO k=1,Edge % BDOFs
                                n = Solver % Mesh % NumberofNodes + &
                                    (Parent % EdgeIndexes(j)-1)*Solver % Mesh % MaxEdgeDOFs+k
                                n = x % Perm( n )
                                IF ( n <= 0 ) CYCLE
                                n = x % DOFs*(n-1) + DOF
                                CALL CRS_ZeroRow( A, n )
                                A % RHS(n) = 0.0d0
                            END DO
                        END IF
                    END DO
                END IF
!------------------------------------------------------------------------------
!              Clear dofs associated with element faces:
!------------------------------------------------------------------------------
                IF ( ASSOCIATED( Solver % Mesh % Faces ) ) THEN
!------------------------------------------------------------------------------             
                    DO j=1,Parent % Type % NumberOfFaces
!------------------------------------------------------------------------------
                        Face => Solver % Mesh % Faces( Parent % FaceIndexes(j) )
                        IF ( Face % BDOFs == 0 ) CYCLE

                        n = 0
                        DO k=1,Element % TYPE % NumberOfNodes
                            DO l=1,Face % TYPE % NumberOfNodes
                                IF ( Face % NodeIndexes(l) == Element % NodeIndexes(k) ) n=n+1
                            END DO
                        END DO
                        IF ( n /= Face % TYPE % NumberOfNodes ) CYCLE

                        DO k=1,Face % BDOFs
                            n = Solver % Mesh % NumberofNodes + &
                            Solver % Mesh % MaxEdgeDOFs * Solver % Mesh % NumberOfEdges + &
                            (Parent % FaceIndexes(j)-1) * Solver % Mesh % MaxFaceDOFs + k
                            n = x % Perm( n )
                            IF ( n <= 0 ) CYCLE
                            n = x % DOFs*(n-1) + DOF
                            CALL CRS_ZeroRow( A, n )
                            A % RHS(n) = 0.0d0
                        END DO
                    END DO
                END IF
!------------------------------------------------------------------------------          
            END DO ! i
!------------------------------------------------------------------------------
        END DO ! DOF
!------------------------------------------------------------------------------
        CALL Info('NumaHeatSolve: ', &
            'Setting Dirichlet boundary conditions', Level=5)
!------------------------------------------------------------------------------
!     Set Dirichlet dofs for edges and faces
!------------------------------------------------------------------------------
        DO DOF=1,x % DOFs
!------------------------------------------------------------------------------     
!           Get the name of the DOF:
!------------------------------------------------------------------------------     
            name = x % name
            IF ( x % DOFs > 1 ) name = ComponentName(name,DOF)
!------------------------------------------------------------------------------     
!           Set nodal loads:
!------------------------------------------------------------------------------     
            !CALL SetNodalLoads( CurrentModel,A, b, &
            !   Name,DOF,x % DOFs,x % Perm )
!------------------------------------------------------------------------------     
!           Set Dirichlet Conditions for Boundaries:
!------------------------------------------------------------------------------     
            CALL NumaSetDirichletBoundaries( Solver, CurrentModel, Time, A, b, &
                Name, DOF, x % DOFs, x % Perm )
!------------------------------------------------------------------------------
!       Set Dirichlet conditions for interior zones
!------------------------------------------------------------------------------ 
            CALL NumaSetInteriorDirichletConditions( CurrentModel, A, b, &
                Name, DOF, x % DOFs, x % Perm )
            SaveElement => CurrentModel % CurrentElement
!------------------------------------------------------------------------------
!        Dirichlet BCs for face & edge DOFs:
!-------------------------------------------------------------------------------
            DO i=1,Solver % Mesh % NumberOfBoundaryElements
!------------------------------------------------------------------------------         
                Element => GetBoundaryElement(i)
                IF ( .NOT. ActiveBoundaryElement() ) CYCLE
!------------------------------------------------------------------------------
!               Get the BC associated to this element
!               Check if the BC exists
!------------------------------------------------------------------------------
                BC => GetBC()
                IF ( .NOT. ASSOCIATED( BC ) ) CYCLE
                IF ( .NOT. ListCheckPresent(BC, Name) ) CYCLE
!------------------------------------------------------------------------------
!           Get parent element:
!------------------------------------------------------------------------------
                Parent => Element % BoundaryInfo % Left
                IF ( .NOT. ASSOCIATED( Parent ) ) THEN
                    Parent => Element % BoundaryInfo % Right
                END IF
                IF ( .NOT. ASSOCIATED( Parent ) )   CYCLE
                IF ( .NOT. ASSOCIATED( Parent % pDefs ) ) CYCLE
!------------------------------------------------------------------------------
!               
!------------------------------------------------------------------------------
                n = Element % Type % NumberOfNodes
                DO j=1,n
                    l = Element % NodeIndexes(j)
                    Work(j)  = ListGetConstReal( BC, Name, Found, &
                        CurrentModel % Mesh % Nodes % x(l), &
                        CurrentModel % Mesh % Nodes % y(l), &
                        CurrentModel % Mesh % Nodes % z(l) )
                END DO
!------------------------------------------------------------------------------
                SELECT CASE(Parent % Type % Dimension)
!------------------------------------------------------------------------------
                    CASE(2)
!------------------------------------------------------------------------------
                    ! If no edges do not try to set boundary conditions
                    IF ( .NOT. ASSOCIATED( Solver % Mesh % Edges ) ) CYCLE

                    ! If boundary edge has no dofs move on to next edge
                    IF (Element % BDOFs <= 0) CYCLE

                    ! Number of nodes for this element
                    n = Element % TYPE % NumberOfNodes

                    ! Get indexes for boundary and values for dofs associated to them
                    CALL getBoundaryIndexes( Solver % Mesh, Element, Parent, gInd, numEdgeDofs )
                    CALL LocalBcBDOFs( BC, Element, numEdgeDofs, Name, STIFF, Work )

                    ! Contribute this boundary to global system
                    ! (i.e solve global boundary problem)
                    DO k=n+1,numEdgeDofs
                        nb = x % Perm( gInd(k) )
                        IF ( nb <= 0 ) CYCLE
                        nb = x % DOFs * (nb-1) + DOF
                        A % RHS(nb) = A % RHS(nb) + Work(k)
                        DO l=1,numEdgeDofs
                            mb = x % Perm( gInd(l) )
                            IF ( mb <= 0 ) CYCLE
                            mb = x % DOFs * (mb-1) + DOF
                            DO kk=A % Rows(nb)+DOF-1,A % Rows(nb+1)-1,x % DOFs
                                IF ( A % Cols(kk) == mb ) THEN
                                    A % Values(kk) = A % Values(kk) + STIFF(k,l)
                                    EXIT
                                END IF
                            END DO
                        END DO
                    END DO
!------------------------------------------------------------------------------                 
                    CASE(3)
!------------------------------------------------------------------------------
                    ! If no faces present do not try to set boundary conditions
                    ! @todo This should be changed to EXIT
                    IF ( .NOT. ASSOCIATED( Solver % Mesh % Faces ) ) CYCLE

                    ! Parameters of element
                    n = Element % TYPE % NumberOfNodes

                    ! Get global boundary indexes and solve dofs associated to them
                    CALL getBoundaryIndexes( Solver % Mesh, Element,  &
                        Parent, gInd, numEdgeDofs )
                    ! If boundary face has no dofs skip to next boundary element
                    IF (numEdgeDOFs == n) CYCLE

                    ! Get local solution
                    CALL LocalBcBDofs( BC, Element, numEdgeDofs, Name, STIFF, Work )

                    ! Contribute this entry to global boundary problem
                    DO k=n+1, numEdgeDOFs
                        nb = x % Perm( gInd(k) )
                        IF ( nb <= 0 ) CYCLE
                        nb = x % DOFs * (nb-1) + DOF
                        A % RHS(nb) = A % RHS(nb) + Work(k)
                        DO l=1, numEdgeDOFs
                            mb = x % Perm( gInd(l) )
                            IF ( mb <= 0 ) CYCLE
                            mb = x % DOFs * (mb-1) + DOF
                            DO kk=A % Rows(nb)+DOF-1,A % Rows(nb+1)-1,x % DOFs
                                IF ( A % Cols(kk) == mb ) THEN
                                    A % Values(kk) = A % Values(kk) + STIFF(k,l)
                                    EXIT
                                END IF
                            END DO
                        END DO
                    END DO
!------------------------------------------------------------------------------
                END SELECT
!------------------------------------------------------------------------------
        END DO ! elements
!------------------------------------------------------------------------------
        CurrentModel % CurrentElement => SaveElement
!------------------------------------------------------------------------------        
        END DO ! DOFs
!------------------------------------------------------------------------------
        CALL Info('NumaHeatSolve: ', &
            'Dirichlet boundary conditions set', Level=5)
!------------------------------------------------------------------------------
    END SUBROUTINE NumaDefaultDirichletConditions
!------------------------------------------------------------------------------
