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
    SUBROUTINE NumaSetDirichletBoundaries( Solver, Model, Time, A, b, Name, DOF, NDOFs, Perm )
!------------------------------------------------------------------------------
!******************************************************************************
!
! Set dirichlet boundary condition for given dof
!
! TYPE(Model_t) :: Model
!   INPUT: the current model structure
!
! TYPE(Matrix_t), POINTER :: A
!   INOUT: The global matrix
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
    USE MaterialModels
    USE DefUtils
    USE NumaAdaptive
!------------------------------------------------------------------------------
    IMPLICIT NONE
!------------------------------------------------------------------------------
        TYPE(Solver_t) :: Solver
        TYPE(Model_t) :: Model
        TYPE(Matrix_t), POINTER :: A

        REAL(KIND=dp) :: b(:), Time

        CHARACTER(LEN=*) :: Name 
        INTEGER :: DOF, NDOFs, Perm(:)
!------------------------------------------------------------------------------
    TYPE(Element_t), POINTER :: Element
    INTEGER, POINTER :: NodeIndexes(:), IndNodes(:)
    INTEGER, ALLOCATABLE :: Indexes(:)
    INTEGER :: BC,i,j,n,t, ModifDBC_Index
    LOGICAL :: GotIt, OrderByBCNumbering
    REAL(KIND=dp) :: ModifDBC_DistMax, ModifDBC_xc, ModifDBC_yc, ModifDBC_zc, &
            ModifDBC_DiamMax, Diameter, ModifDBC_TimeStart

    LOGICAL :: Conditional, ModifDBC, ModifDBCToFluxBC, SelectElement
    LOGICAL, ALLOCATABLE :: DonePeriodic(:)
    CHARACTER(LEN=MAX_NAME_LEN) :: CondName, PassName

    INTEGER :: NoNodes,NoDims,bf_id,nlen, NOFNodesFound
    REAL(KIND=dp), POINTER :: CoordNodes(:,:)
    REAL(KIND=dp) :: MinDist,Dist, Eps
    LOGICAL, ALLOCATABLE :: ActivePart(:), ActiveCond(:), ActivePartAll(:)
    LOGICAL :: NodesFound, Passive
    TYPE(ValueList_t), POINTER :: ValueList, SolverParams

        TYPE(Nodes_t) :: Nodes      

    INTERFACE
        SUBROUTINE NumaSetElementValues (A,b,Name,DOF,NDOFs,ValueList,n,NodeIndexes,Perm)
            USE MaterialModels
            USE DefUtils
            USE NumaAdaptive
            IMPLICIT NONE

            TYPE(Matrix_t), POINTER :: A
            REAL(KIND=dp) :: b(:)
            CHARACTER(LEN=*) :: Name 
            INTEGER :: n,DOF,NDOFs, Perm(:),NodeIndexes(:)
            TYPE(ValueList_t), POINTER :: ValueList
        END SUBROUTINE
    END INTERFACE


!------------------------------------------------------------------------------
!   These logical vectors are used to minimize extra effort in setting up different BCs
!------------------------------------------------------------------------------
    nlen = LEN_TRIM(Name)

    n = MAX( Model % NumberOfBodyForces,Model % NumberOfBCs)
    ALLOCATE( ActivePart(n), ActivePartAll(n), ActiveCond(n))
    CondName = Name(1:nlen) // ' Condition'
    PassName = Name(1:nlen) // ' Passive'
 
    ALLOCATE( Indexes(Model % Mesh % MaxElementDOFs) )
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!   Check if the BC will have to be modified at some elements
!------------------------------------------------------------------------------
        ModifDBC = .FALSE.
        ModifDBCToFluxBC = .FALSE.

        SolverParams => GetSolverParams()
        ModifDBC = ListGetLogical( SolverParams, 'Dirichlet Modified BC ', GotIt )
!------------------------------------------------------------------------------
        IF (ModifDBC) THEN
!------------------------------------------------------------------------------
            ModifDBCToFluxBC = ListGetLogical( SolverParams, &
                'Dirichlet Modified BC To Flux BC', GotIt )
            !------------------------------------------------------------------------------
            IF (.NOT. ModifDBCToFluxBC) THEN
                CALL Info( 'NumaSetDirichletBoundaries', 'Setting Modified Dirichlet BC' )
            END IF
            ALLOCATE(Nodes % x(n), Nodes % y(n), Nodes % z(n))
            !------------------------------------------------------------------------------
            ! Index of the boundary condition where is specified the modification:
            !------------------------------------------------------------------------------
            ModifDBC_Index = GetInteger( SolverParams, 'Dirichlet Modified BC Index', GotIt )
            IF (ModifDBC_Index>Model % NumberOfBCs) THEN
                CALL Fatal( 'NumaSetDirichletBoundaries', 'Check Dirichlet Modified BC Index!' )
            END IF
            !------------------------------------------------------------------------------
            ! Distance from target under which is applied the modification:
            !------------------------------------------------------------------------------
            ModifDBC_DistMax = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                'Dirichlet Modified BC Distance Max', GotIt )
            IF (.NOT. GotIt) THEN
                CALL Error( 'NumaSetDirichletBoundaries', 'Check Dirichlet Modified BC Distance Max!' )
                ModifDBC_DistMax = 0.0 
            END IF
            !------------------------------------------------------------------------------
            ! x-coord of the target around which is applied the modification:
            !------------------------------------------------------------------------------
            ModifDBC_xc = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                'Dirichlet Modified BC xc', GotIt )
            IF (.NOT. GotIt) THEN
                CALL Error( 'NumaSetDirichletBoundaries', 'Check Dirichlet Modified BC xc!' )
            END IF
            !------------------------------------------------------------------------------
            ! y-coord of the target around which is applied the modification:
            !------------------------------------------------------------------------------
            ModifDBC_yc = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                'Dirichlet Modified BC yc', GotIt )
            IF (.NOT. GotIt) THEN
                CALL Error( 'NumaSetDirichletBoundaries', 'Check Dirichlet Modified BC yc!' )
            END IF
            !------------------------------------------------------------------------------
            ! z-coord of the target around which is applied the modification:
            !------------------------------------------------------------------------------
            ModifDBC_zc = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                'Dirichlet Modified BC zc', GotIt )
            IF (.NOT. GotIt) THEN
                CALL Error( 'NumaSetDirichletBoundaries', 'Check Dirichlet Modified BC zc!' )
            END IF
            !------------------------------------------------------------------------------
            ! Diameter max of the elements on which is applied the modification:
            !------------------------------------------------------------------------------
            ModifDBC_DiamMax = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                'Dirichlet Modified BC Diameter Max', GotIt )
            IF (.NOT. GotIt) THEN
                CALL Info( 'NumaSetDirichletBoundaries', 'No Dirichlet Modified BC Diameter Max specified' )
                ModifDBC_DiamMax = -1.0 
            END IF
            !------------------------------------------------------------------------------
            ! Start time at which is applied the modification:
            !------------------------------------------------------------------------------
            ModifDBC_TimeStart = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                'Dirichlet Modified BC Time Start', GotIt )
            IF (.NOT. GotIt) THEN
                ModifDBC_TimeStart = 0.0
            END IF
!------------------------------------------------------------------------------
        END IF
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!   Go through the periodic BCs and set the linear dependence
!------------------------------------------------------------------------------
        ActivePart = .FALSE.
!------------------------------------------------------------------------------
        DO BC=1,Model % NumberOfBCs
         IF ( .NOT. ListGetLogical( Model % BCs(BC) % Values, &
                 'Periodic BC ' // Name(1:nlen), GotIt ) ) ActivePart(BC) = .TRUE.
         IF ( .NOT. ListGetLogical( Model % BCs(BC) % Values, &
                 'Anti Periodic BC ' // Name(1:nlen), GotIt ) ) ActivePart(BC) = .TRUE.
        END DO
!------------------------------------------------------------------------------
        IF( ANY(ActivePart) ) THEN    
            ALLOCATE( DonePeriodic( Model % Mesh % NumberOFNodes ) )
            DonePeriodic = .FALSE.
            DO BC=1,Model % NumberOfBCs
                CALL NumaSetPeriodicBoundariesPass1( Model, A, b, Name, DOF, NDOFs, Perm, BC, DonePeriodic )
            END DO

            DonePeriodic = .FALSE.
            DO BC=1,Model % NumberOfBCs
                CALL NumaSetPeriodicBoundariesPass2( Model, A, b, Name, DOF, NDOFs, Perm, BC, DonePeriodic )
            END DO
            DEALLOCATE( DonePeriodic ) 
        END IF
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
!   Go through the normal Dirichlet BCs applied on the boundaries
!------------------------------------------------------------------------------
    ActivePart = .FALSE.
    ActiveCond = .FALSE.
    ActivePartAll = .FALSE.
!------------------------------------------------------------------------------
    DO BC=1,Model % NumberOfBCs
      ActivePartAll(BC) = ListCheckPresent( &
            Model % BCs(bc) % Values, Name(1:nlen) // ' DOFs' )
      ActivePart(BC) = ListCheckPresent( Model % BCs(bc) % Values, Name ) 
      ActiveCond(BC) = ListCheckPresent( Model % BCs(bc) % Values, CondName )      
    END DO
!------------------------------------------------------------------------------
    IF( ANY(ActivePart) .OR. ANY(ActivePartAll) ) THEN    
      OrderByBCNumbering = ListGetLogical( Model % Simulation, &
          'Set Dirichlet BCs by BC Numbering', gotIt)
!------------------------------------------------------------------------------
      IF ( OrderByBCNumbering ) THEN
!------------------------------------------------------------------------------
!               Go through Boundaries
!------------------------------------------------------------------------------
        DO BC=1,Model % NumberOfBCs
!------------------------------------------------------------------------------
          IF(.NOT. ActivePart(BC) .AND. .NOT. ActivePartAll(BC) ) CYCLE
          Conditional = ActiveCond(BC)
!------------------------------------------------------------------------------
!                   Go through Boundary elements
!------------------------------------------------------------------------------
          DO t = Model % NumberOfBulkElements + 1, &
              Model % NumberOfBulkElements + Model % NumberOfBoundaryElements
!------------------------------------------------------------------------------            
            Element => Model % Elements(t)
!------------------------------------------------------------------------------
!                       Check that this is the boundary associated to the element:
!------------------------------------------------------------------------------
            IF ( Element % BoundaryInfo % Constraint /= Model % BCs(BC) % Tag ) CYCLE
           
            Model % CurrentElement => Element
!------------------------------------------------------------------------------
!                       Get element description:
!------------------------------------------------------------------------------
            IF ( ActivePart(BC) ) THEN
              n = Element % Type % NumberOfNodes
              Indexes(1:n) = Element % NodeIndexes
            ELSE
              n = SgetElementDOFs( Indexes )
            END IF
!------------------------------------------------------------------------------
!                       Get the values to apply on the boundary:
!------------------------------------------------------------------------------
            ValueList => Model % BCs(BC) % Values

!------------------------------------------------------------------------------
!                       If locally modified boundary condition:
!------------------------------------------------------------------------------
                        IF (ModifDBC) THEN
!------------------------------------------------------------------------------
                            SelectElement = .FALSE.
!------------------------------------------------------------------------------
!                           Selection test for locally modified boundary condition:
!------------------------------------------------------------------------------
                            Nodes % x(1:n) = Solver % Mesh % Nodes % x(Element % NodeIndexes)
                            Nodes % y(1:n) = Solver % Mesh % Nodes % y(Element % NodeIndexes)
                            Nodes % z(1:n) = Solver % Mesh % Nodes % z(Element % NodeIndexes)
!------------------------------------------------------------------------------
!                           Compute the maximum distance from the nodes to the target:
!------------------------------------------------------------------------------
                            Dist = sqrt( (Nodes % x(1)-ModifDBC_xc)**2 + &
                                (Nodes % y(1)-ModifDBC_yc)**2 + &
                                (Nodes % z(1)-ModifDBC_zc)**2 )
                
                            DO i=2,n
                                Dist = Max (Dist, sqrt( (Nodes % x(i)-ModifDBC_xc)**2 + &
                                    (Nodes % y(i)-ModifDBC_yc)**2 + &
                                    (Nodes % z(i)-ModifDBC_zc)**2 ) )
                            END DO
!------------------------------------------------------------------------------
!                           Check the condition on distance from the target and time:
!------------------------------------------------------------------------------
                            IF ( ((Dist<ModifDBC_DistMax) .OR. (ModifDBC_DistMax<0.0)) .AND. (Time>=ModifDBC_TimeStart) ) THEN
!------------------------------------------------------------------------------
!                               Compute the diameter of the element:
!------------------------------------------------------------------------------                             
                                Diameter = ElementDiameter( Element, Nodes )
!------------------------------------------------------------------------------
!                               Check the condition on the diameter of the element:
!------------------------------------------------------------------------------
                                IF ( (Diameter<ModifDBC_DiamMax) .OR. (ModifDBC_DiamMax<0.0) ) THEN
!------------------------------------------------------------------------------
                                    SelectElement = .TRUE.
!------------------------------------------------------------------------------
                                END IF ! diameter
!------------------------------------------------------------------------------
                            END IF ! distance and time
!------------------------------------------------------------------------------
!                           Element is selected and Dirichlet BC is modified into Dirichlet BC:
!------------------------------------------------------------------------------
                            IF (SelectElement .AND. .NOT. ModifDBCToFluxBC) THEN
!------------------------------------------------------------------------------
!                               Get the modified value in input file:
!------------------------------------------------------------------------------
                                ValueList => Model % BCs(ModifDBC_Index) % Values
!------------------------------------------------------------------------------
                            END IF
!------------------------------------------------------------------------------
!                           Dirichlet BC is modified into Dirichlet BC, or DBC is modified into 
!                           NBC but element not selected:
!------------------------------------------------------------------------------
                            IF (.NOT. ModifDBCToFluxBC .OR. (ModifDBCToFluxBC .AND. .NOT.SelectElement)) THEN
!------------------------------------------------------------------------------
!                               Use the dirichlet boundary values (modified) to update the system
!------------------------------------------------------------------------------
                                CALL NumaSetElementValues(A, b,Name, DOF,NDOFs,ValueList,n,Indexes,Perm)
!------------------------------------------------------------------------------
                            END IF
!------------------------------------------------------------------------------
                        ELSE
!------------------------------------------------------------------------------
!                           Use the dirichlet boundary values (not modified) to update the system
!------------------------------------------------------------------------------
                CALL NumaSetElementValues(A, b,Name, DOF,NDOFs,ValueList,n,Indexes,Perm)
!------------------------------------------------------------------------------
                        END IF ! ModifDBC
!------------------------------------------------------------------------------
          END DO !Boundary elements
!------------------------------------------------------------------------------
        END DO !BC
!------------------------------------------------------------------------------
      ELSE
!------------------------------------------------------------------------------
!               Go through Boundary elements
!------------------------------------------------------------------------------
        DO t = Model % NumberOfBulkElements + 1, &
            Model % NumberOfBulkElements + Model % NumberOfBoundaryElements
!------------------------------------------------------------------------------
!                   Go through Boundaries
!------------------------------------------------------------------------------
          DO BC=1,Model % NumberOfBCs
!------------------------------------------------------------------------------
            IF(.NOT. ActivePart(BC) .AND. .NOT. ActivePartAll(BC) ) CYCLE
            Conditional = ActiveCond(BC)   
            Element => Model % Elements(t)
!------------------------------------------------------------------------------
!                       Check that this is the boundary associated to the element:
!------------------------------------------------------------------------------
            IF ( Element % BoundaryInfo % Constraint /= Model % BCs(BC) % Tag ) CYCLE
            
            Model % CurrentElement => Element
!------------------------------------------------------------------------------
!                       Get element description:
!------------------------------------------------------------------------------
            IF ( ActivePart(BC) ) THEN
              n = Element % Type % NumberOfNodes
              Indexes(1:n) = Element % NodeIndexes
            ELSE
              n = SgetElementDOFs( Indexes )
            END IF
!------------------------------------------------------------------------------
!                       Get the values to apply on the boundary:
!------------------------------------------------------------------------------
            ValueList => Model % BCs(BC) % Values

!------------------------------------------------------------------------------
!                       If locally modified boundary condition:
!------------------------------------------------------------------------------
                        IF (ModifDBC) THEN
!------------------------------------------------------------------------------
                            SelectElement = .FALSE.
!------------------------------------------------------------------------------
!                           Selection test for locally modified boundary condition:
!------------------------------------------------------------------------------
                            Nodes % x(1:n) = Solver % Mesh % Nodes % x(Element % NodeIndexes)
                            Nodes % y(1:n) = Solver % Mesh % Nodes % y(Element % NodeIndexes)
                            Nodes % z(1:n) = Solver % Mesh % Nodes % z(Element % NodeIndexes)
!------------------------------------------------------------------------------
!                           Compute the maximum distance from the nodes to the target:
!------------------------------------------------------------------------------
                            Dist = sqrt( (Nodes % x(1)-ModifDBC_xc)**2 + &
                                (Nodes % y(1)-ModifDBC_yc)**2 + &
                                (Nodes % z(1)-ModifDBC_zc)**2 )
                
                            DO i=2,n
                                Dist = Max (Dist, sqrt( (Nodes % x(i)-ModifDBC_xc)**2 + &
                                    (Nodes % y(i)-ModifDBC_yc)**2 + &
                                    (Nodes % z(i)-ModifDBC_zc)**2 ) )
                            END DO
!------------------------------------------------------------------------------
!                           Check the condition on distance from the target and time:
!------------------------------------------------------------------------------
                            IF ( ((Dist<ModifDBC_DistMax) .OR. (ModifDBC_DistMax<0.0)) .AND. (Time>=ModifDBC_TimeStart) ) THEN
!------------------------------------------------------------------------------
!                               Compute the diameter of the element:
!------------------------------------------------------------------------------                             
                                Diameter = ElementDiameter( Element, Nodes )
!------------------------------------------------------------------------------
!                               Check the condition on the diameter of the element:
!------------------------------------------------------------------------------
                                IF ( (Diameter<ModifDBC_DiamMax) .OR. (ModifDBC_DiamMax<0.0) ) THEN
!------------------------------------------------------------------------------
                                    SelectElement = .TRUE.
!------------------------------------------------------------------------------
                                END IF ! diameter
!------------------------------------------------------------------------------
                            END IF ! distance and time
!------------------------------------------------------------------------------
!                           Element is selected and Dirichlet BC is modified into Dirichlet BC:
!------------------------------------------------------------------------------
                            IF (SelectElement .AND. .NOT. ModifDBCToFluxBC) THEN
!------------------------------------------------------------------------------
!                               Get the modified value in input file:
!------------------------------------------------------------------------------
                                ValueList => Model % BCs(ModifDBC_Index) % Values
!------------------------------------------------------------------------------
                            END IF
!------------------------------------------------------------------------------
!                           Dirichlet BC is modified into Dirichlet BC, or DBC is modified into 
!                           NBC but element not selected:
!------------------------------------------------------------------------------
                            IF (.NOT. ModifDBCToFluxBC .OR. (ModifDBCToFluxBC .AND. .NOT.SelectElement)) THEN
!------------------------------------------------------------------------------
!                               Use the dirichlet boundary values (modified) to update the system
!------------------------------------------------------------------------------
                                CALL NumaSetElementValues(A, b,Name, DOF,NDOFs,ValueList,n,Indexes,Perm)
!------------------------------------------------------------------------------
                            END IF
!------------------------------------------------------------------------------
                        ELSE
!------------------------------------------------------------------------------
!                           Use the dirichlet boundary values (not modified) to update the system
!------------------------------------------------------------------------------
                CALL NumaSetElementValues(A, b,Name, DOF,NDOFs,ValueList,n,Indexes,Perm)
!------------------------------------------------------------------------------
                        END IF ! ModifDBC
!------------------------------------------------------------------------------
          END DO !BC
!------------------------------------------------------------------------------
        END DO !Boundary elements
!------------------------------------------------------------------------------
      END IF !OrderByBCNumbering
!------------------------------------------------------------------------------
    END IF !ActivePart or ActivePartAll
!------------------------------------------------------------------------------
        IF (ModifDBC) DEALLOCATE(Nodes % x, Nodes % y, Nodes % z)
!------------------------------------------------------------------------------




!------------------------------------------------------------------------------
!   Go through the Dirichlet conditions in the body force lists
!------------------------------------------------------------------------------ 
    ActivePart = .FALSE.
    ActiveCond = .FALSE.
    ActivePartAll = .FALSE.
    Passive = .FALSE.
!------------------------------------------------------------------------------
    DO bf_id=1,Model % NumberOFBodyForces
      ActivePart(bf_id) = ListCheckPresent( Model % BodyForces(bf_id) % Values, Name ) 
      ActivePartAll(bf_id) = ListCheckPresent( &
           Model % BodyForces(bf_id) % Values, Name(1:nlen) // ' DOFs' ) 
      ActiveCond(bf_id) = ListCheckPresent( Model % BodyForces(bf_id) % Values,CondName )      

      Passive = Passive .OR. ListCheckPresent( Model % BodyForces(bf_id) % Values, &
           PassName )
    END DO !bf_id 
!------------------------------------------------------------------------------  
    IF ( ANY( ActivePart ) .OR. ANY(ActivePartAll) ) THEN
!------------------------------------------------------------------------------
!           Go through Bulk elements
!------------------------------------------------------------------------------
      DO t = 1, Model % NumberOfBulkElements 
!------------------------------------------------------------------------------
        Element => Model % Elements(t)
        bf_id = ListGetInteger( Model % Bodies(Element % BodyId) % Values,'Body Force', GotIt)
        
        IF(.NOT. GotIt) CYCLE
        IF(.NOT. ActivePart(bf_id) .AND. .NOT. ActivePartAll(bf_id)) CYCLE
        Conditional = ActiveCond(bf_id)
        
        Model % CurrentElement => Element
        IF ( ActivePart(bf_id) ) THEN
          n = Element % Type % NumberOfNodes
          Indexes(1:n) = Element % NodeIndexes
        ELSE
          n = SgetElementDOFs( Indexes )
        END IF
        ValueList => Model % BodyForces(bf_id) % Values
!------------------------------------------------------------------------------
!               Use the boundary values to update the system
!------------------------------------------------------------------------------
        CALL NumaSetElementValues(A, b,Name, DOF,NDOFs,ValueList,n,Indexes,Perm)
!------------------------------------------------------------------------------
      END DO !Bulk Elements
!------------------------------------------------------------------------------
    END IF
!------------------------------------------------------------------------------
    DEALLOCATE(ActivePart, ActivePartAll, ActiveCond, Indexes)
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
!   Go through the pointwise Dirichlet BCs that are created on-the-fly
!   Note that it is best that the coordinates are transformed to nodes using 
!   the right variable. Otherwise it could point to nodes that are not active.
!------------------------------------------------------------------------------
    DO BC=1,Model % NumberOfBCs
!------------------------------------------------------------------------------      
      IF( .NOT. ListCheckPresent( Model % BCs(BC) % Values,Name )) CYCLE
      NodesFound = ListCheckPresent( Model % BCs(BC) % Values,'Target Nodes' )
!------------------------------------------------------------------------------      
!       The coordinates are only requested for a body that has no list of nodes. 
!       At the first calling the list of coorinates is transformed to list of nodes. 
!------------------------------------------------------------------------------
      IF(.NOT. NodesFound) THEN
!------------------------------------------------------------------------------
        CoordNodes => ListGetConstRealArray(Model % BCs(BC) % Values, &
                    'Target Coordinates',GotIt)
        IF(GotIt) THEN
          Eps = ListGetConstReal( Model % BCs(BC) % Values, &
                      'Target Coordinates Eps', Gotit )
          IF ( .NOT. GotIt ) THEN
            Eps = HUGE(Eps)
          ELSE
            Eps = Eps**2
          END IF

          NoNodes = SIZE(CoordNodes,1)
          NoDims = SIZE(CoordNodes,2)
          
          IF(NoNodes > 0) THEN               
            ALLOCATE( IndNodes(NoNodes) )
            IndNodes = -1
            DO j=1,NoNodes
              MinDist = HUGE(Dist)
              
              DO i=1,Model % NumberOfNodes
                IF( Perm(i) == 0) CYCLE
                
                Dist = (Model % Mesh % Nodes % x(i) - CoordNodes(j,1))**2.0 
                IF(NoDims >= 2) Dist = Dist + (Model % Mesh % Nodes % y(i) - CoordNodes(j,2))**2.0 
                IF(NoDims == 3) Dist = Dist + (Model % Mesh % Nodes % z(i) - CoordNodes(j,3))**2.0 
                Dist = SQRT(Dist)
                
                IF(Dist < MinDist .AND. Dist <= Eps ) THEN
                  MinDist = Dist
                  IndNodes(j) = i
                END IF
              END DO
            END DO

            NOFNodesFound = 0
            DO j=1,NoNodes
               IF ( IndNodes(j)>0 ) THEN
                 NOFNodesFound=NOFNodesFound+1
                 IndNodes(NOFNodesFound) = IndNodes(j)
               END IF
            END DO
            
            ! In the first time add the found nodes to the list structure
            IF ( NOFNodesFound > 0 ) THEN
              CALL ListAddIntegerArray( Model % BCs(BC) % Values,'Target Nodes', &
                  NOFNodesFound, IndNodes) 
              DEALLOCATE(IndNodes)
              NodesFound = .TRUE.               
            END IF
          END IF
        END IF
!------------------------------------------------------------------------------
      END IF !not NodesFound
!------------------------------------------------------------------------------      
      IF(NodesFound) THEN       
!------------------------------------------------------------------------------  
        Conditional = ListCheckPresent( Model % BCs(bc) % Values, CondName )      
        NodeIndexes => ListGetIntegerArray( Model % BCs(BC) % Values,'Target Nodes')
        n = SIZE(NodeIndexes)
        ValueList => Model % BCs(BC) % Values
!------------------------------------------------------------------------------
!               Use the point values to update the system
!------------------------------------------------------------------------------
        CALL NumaSetPointValues(A, b,Name, DOF,NDOFs,ValueList,n,NodeIndexes,Perm)
!------------------------------------------------------------------------------  
      END IF !NodesFound
!------------------------------------------------------------------------------
    END DO !BC
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
!   Take care of the matrix entries of passive elements
!------------------------------------------------------------------------------
    IF ( Passive ) THEN
       DO i = 1, A % NumberOfRows
          IF ( ABS(A % Values( A % Diag(i) ) ) < 1.0e-14 ) THEN
             A % Values( A % Diag(i) ) = 1.0d0
             b(i) = Model % Solver % Variable % Values(i)
          END IF
       END DO
    END IF !Passive
!------------------------------------------------------------------------------
  END SUBROUTINE NumaSetDirichletBoundaries
!------------------------------------------------------------------------------
