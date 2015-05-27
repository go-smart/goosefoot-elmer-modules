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

SUBROUTINE AssemblyNeumann(Solver, Model, StiffMatrix, ForceVector, &
        TempPerm, Temperature, &
        TransientSimulation, Time, dt)

    USE MaterialModels
    USE DefUtils
    USE NumaAdaptive
    IMPLICIT NONE

    TYPE(Matrix_t), POINTER :: StiffMatrix
    TYPE(Model_t)  :: Model
    TYPE(Solver_t), TARGET :: Solver
    REAL(KIND=dp) :: Time, dt
    REAL(KIND=dp), POINTER :: Temperature(:), ForceVector(:)
    LOGICAL :: TransientSimulation
    INTEGER, POINTER :: TempPerm(:)

    REAL(KIND=dp), EXTERNAL :: GapTemperature

    TYPE(Nodes_t)   :: ElementNodes
    TYPE(Element_t), POINTER :: Element
    TYPE(ValueList_t), POINTER :: BC
    LOGICAL :: Found, &
        ModifDBC, ModifDBCToFluxBC, ModifDBC_Select_Element, &
        AllocationsDone = .FALSE.
    INTEGER :: ModifDBC_Index, n, i, j, t
    REAL(KIND=dp) :: &
        ModifDBC_DistMax, ModifDBC_xc, ModifDBC_yc, ModifDBC_zc, ModifDBC_DiamMax, &
        Diameter, ModifDBC_TimeStart, ModifDBC_Dist
    REAL(KIND=dp), ALLOCATABLE :: LOAD(:), &
        LocalMASS(:,:), LocalSTIFF(:,:), LocalFORCE(:), &
        HeatTransferCoeff(:)
    
    REAL(KIND=dp), POINTER :: ATextArray(:,:), &
        HeatFluxArray(:,:), HeatTransferArray(:, :)
        
    REAL(KIND=dp), ALLOCATABLE ::  &
        Work(:), AText(:)

    SAVE LOAD, ElementNodes, HeatTransferCoeff, AllocationsDone, Work, AText, LocalMASS, &
        LocalSTIFF, LocalFORCE, ModifDBC_Index, ModifDBC, ModifDBCToFluxBC, &
        ModifDBC_DistMax, ModifDBC_xc, ModifDBC_yc, ModifDBC_zc, ModifDBC_DiamMax, &
        ModifDBC_TimeStart

    INTERFACE
        SUBROUTINE TemperatureBoundary( BoundaryMatrix,BoundaryVector, &
                   LoadVector,NodalAlpha,Element,n,Nodes )
            USE MaterialModels
            USE DefUtils
            USE NumaAdaptive
            IMPLICIT NONE

            REAL(KIND=dp) :: BoundaryMatrix(:,:),BoundaryVector(:), &
                LoadVector(:),NodalAlpha(:)
            INTEGER :: n
            TYPE(Nodes_t)   :: Nodes
            TYPE(Element_t) :: Element
        END SUBROUTINE TemperatureBoundary
    END INTERFACE
    IF ( .NOT. AllocationsDone .OR. Solver % Mesh % Changed ) THEN
        CALL Allocations()
    END IF

    !------------------------------------------------------------------------------
    ! Neumann boundary conditions
    !------------------------------------------------------------------------------

    !------------------------------------------------------------------------------
    ! Go through boundary elements
    !------------------------------------------------------------------------------
    DO t=1, Solver % Mesh % NumberOfBoundaryElements
    !------------------------------------------------------------------------------
        Element => GetBoundaryElement(t)
        !------------------------------------------------------------------------------
        ! Check if the element is active and of suitable type
        !------------------------------------------------------------------------------
        IF ( .NOT. ActiveBoundaryElement() ) CYCLE
        IF ( GetElementFamily() == 1 ) CYCLE
        !------------------------------------------------------------------------------
        ! Get the number of nodes 
        !------------------------------------------------------------------------------
        n = GetElementNOFNodes()
        !------------------------------------------------------------------------------
        ! Get the BC associated to the element
        !------------------------------------------------------------------------------
        BC => GetBC()
        !------------------------------------------------------------------------------
        ! Check that it is a Neumann condition
        !------------------------------------------------------------------------------
        IF ( GetLogical( BC, 'Heat Flux BC',Found) ) THEN
            !------------------------------------------------------------------------------
            ! Get the nodes characteristics
            !------------------------------------------------------------------------------
            CALL GetElementNodes( ElementNodes )
            !------------------------------------------------------------------------------
            ! Get physical parameters from input file
            !------------------------------------------------------------------------------
            HeatTransferCoeff = 0.0D0
            LOAD  = 0.0D0
            Work = 0.0D0
            AText = 0.0D0
            !------------------------------------------------------------------------------
            ! Convective transfer coefficient (BC)
            !------------------------------------------------------------------------------
            HeatTransferArray => ListGetConstRealArray( BC, 'Heat Transfer Coefficient', Found )
            IF (Found) THEN 
                Work(1:n) =  HeatTransferArray(1, 1)
            ELSE
                Work(1:n) =  0.0
            END IF
            !------------------------------------------------------------------------------
            ! Heat gap for convective transfer (BC)
            !------------------------------------------------------------------------------             
            IF ( GetLogical( BC, 'Heat Gap', Found ) ) THEN
                IF ( .NOT. GetLogical( BC, 'Heat Gap Implicit', Found ) ) THEN
                    AText(1:n) = GapTemperature( Solver, Element, ElementNodes, Temperature, TempPerm, n)
                END IF
            ELSE
                ATextArray => ListGetConstRealArray( BC, 'External Temperature', Found )
                IF (Found) THEN
                    IF ( ANY(Work(1:n) /= 0.0d0) ) THEN
                        AText(1:n) = ATextArray(1, 1)
                    END IF
                ELSE 
                    IF ( ANY(Work(1:n) /= 0.0d0) ) THEN
                        AText(1:n) = 310.0
                    END IF
                END IF
            END IF  
            !------------------------------------------------------------------------------
            ! Transfer BC: -k@T/@n = \alpha(T - Text)
            !------------------------------------------------------------------------------
            DO j=1,n
                LOAD(j) = LOAD(j) + Work(j) * AText(j)
                HeatTransferCoeff(j) = HeatTransferCoeff(j) + Work(j)
            END DO
            !------------------------------------------------------------------------------
            ! Flux BC: k@T/@n = g
            !------------------------------------------------------------------------------ 
            HeatFluxArray => ListGetConstRealArray( BC, 'Heat Flux', Found )
            IF (Found) THEN
                LOAD(1:n) = LOAD(1:n) + HeatFluxArray(1, 1)
            END IF
            !------------------------------------------------------------------------------
            ! Get element matrix and rhs due to boundary conditions 
            !------------------------------------------------------------------------------
            CALL TemperatureBoundary( LocalSTIFF,LocalFORCE, &
                LOAD,HeatTransferCoeff,Element,n,ElementNodes)
            !------------------------------------------------------------------------------
            ! If time dependent simulation, add the time derivative coefficients 
            ! terms to stiff matrix
            !------------------------------------------------------------------------------
            IF ( TransientSimulation ) THEN
                LocalMASS = 0.d0
                CALL Add1stOrderTime( LocalMASS, LocalSTIFF, LocalFORCE, dt, n, 1, &
                    TempPerm(Element % NodeIndexes(1:n)), Solver )  
            END IF
            !------------------------------------------------------------------------------
            ! Add heat gap in the stiffmatrix (if implicit treatment)
            !------------------------------------------------------------------------------
            IF ( GetLogical( BC, 'Heat Gap', Found ) ) THEN
                IF ( GetLogical( BC, 'Heat Gap Implicit', Found ) ) &
                    CALL AddHeatGap( Solver, Element, LocalSTIFF, TempPerm, ElementNodes, N)
            END IF
            !------------------------------------------------------------------------------
            ! Update global matrices from local matrices
            !------------------------------------------------------------------------------         
            CALL UpdateGlobalEquations( StiffMatrix, LocalSTIFF, &
                forcevector, LocalFORCE, n, 1, TempPerm(Element % NodeIndexes) )
        !------------------------------------------------------------------------------
        ELSE
        !------------------------------------------------------------------------------
        ! Maybe a flux BC has to be a pplied as a modification of Diriclet BC:
        !------------------------------------------------------------------------------
            IF (ModifDBC .AND. ModifDBCToFluxBC) THEN   
            !------------------------------------------------------------------------------
            ! If a dirichlet BC is locally modified to flux BC, check if element selected:
            !------------------------------------------------------------------------------
                    ModifDBC_Select_Element = .FALSE.
                    !------------------------------------------------------------------------------
                    !   Selection test for locally modified boundary condition:
                    !------------------------------------------------------------------------------
                    ! Get the nodes characteristics
                    !------------------------------------------------------------------------------
                    CALL GetElementNodes( ElementNodes )
                    !------------------------------------------------------------------------------
                    !   Compute the maximum distance from the nodes to the target:
                    !------------------------------------------------------------------------------
                    ModifDBC_Dist = sqrt( (ElementNodes % x(1)-ModifDBC_xc)**2 + &
                        (ElementNodes % y(1)-ModifDBC_yc)**2 + &
                        (ElementNodes % z(1)-ModifDBC_zc)**2 )
        
                    DO i=2,n
                        ModifDBC_Dist = Max (ModifDBC_Dist, sqrt( (ElementNodes % x(i)-ModifDBC_xc)**2 + &
                            (ElementNodes % y(i)-ModifDBC_yc)**2 + &
                            (ElementNodes % z(i)-ModifDBC_zc)**2 ) )
                    END DO
                    !------------------------------------------------------------------------------
                    !   Check the condition on distance from the target and time:
                    !------------------------------------------------------------------------------
                    IF ( ((ModifDBC_Dist<ModifDBC_DistMax) .OR. &
                           (ModifDBC_DistMax<0.0)) .AND. (Time>=ModifDBC_TimeStart) ) THEN
                        !------------------------------------------------------------------------------
                        !   Compute the diameter of the element:
                        !------------------------------------------------------------------------------                             
                        Diameter = ElementDiameter( Element, ElementNodes )
                        !------------------------------------------------------------------------------
                        !   Check the condition on the diameter of the element:
                        !------------------------------------------------------------------------------
                        IF ( (Diameter<ModifDBC_DiamMax) .OR. (ModifDBC_DiamMax<0.0) ) THEN
                        !------------------------------------------------------------------------------
                            ModifDBC_Select_Element = .TRUE.
                        !------------------------------------------------------------------------------
                        END IF
                        !------------------------------------------------------------------------------
                    END IF
                    !------------------------------------------------------------------------------
                    !   If selected element than apply Flux BC:
                    !------------------------------------------------------------------------------
                    IF (ModifDBC_Select_Element) THEN
                        !------------------------------------------------------------------------------
                        ! Check that a flux condition is defined:
                        !------------------------------------------------------------------------------
                        IF ( GetLogical( Model % BCs(ModifDBC_Index) % Values, 'Heat Flux BC',Found) ) THEN
                            !------------------------------------------------------------------------------
                            ! Get physical parameters from input file
                            !------------------------------------------------------------------------------
                            HeatTransferCoeff = 0.0D0
                            LOAD  = 0.0D0
                            Work = 0.0D0
                            AText = 0.0D0
                            !------------------------------------------------------------------------------
                            ! Convective transfer coefficient (BC)
                            !------------------------------------------------------------------------------
                            HeatTransferArray => ListGetConstRealArray( Model % BCs(ModifDBC_Index) % Values, &
                                'Heat Transfer Coefficient', Found )
                                IF (Found) THEN 
                                    Work(1:n) =  HeatTransferArray(1, 1)
                                ELSE
                                    Work(1:n) =  0.0
                                END IF
                            !------------------------------------------------------------------------------
                            ! Heat gap for convective transfer (BC)
                            !------------------------------------------------------------------------------             
                            IF ( GetLogical( Model % BCs(ModifDBC_Index) % Values, 'Heat Gap', Found ) ) THEN
                                    IF ( .NOT. GetLogical( Model % BCs(ModifDBC_Index) % Values, &
                                        'Heat Gap Implicit', Found ) ) THEN
                                        AText(1:n) = GapTemperature( Solver, Element, ElementNodes, Temperature, TempPerm, n)
                                    END IF
                            ELSE
                                ATextArray => ListGetConstRealArray( Model % BCs(ModifDBC_Index) % Values, &
                                    'External Temperature', Found )
                                IF (Found) THEN
                                    IF ( ANY(Work(1:n) /= 0.0d0) ) THEN
                                        AText(1:n) = ATextArray(1, 1)
                                    END IF
                                ELSE 
                                    IF ( ANY(Work(1:n) /= 0.0d0) ) THEN
                                        AText(1:n) = 310.0
                                    END IF
                                END IF
                            END IF  
                            !------------------------------------------------------------------------------
                            ! Transfer BC: -k@T/@n = \alpha(T - Text)
                            !------------------------------------------------------------------------------
                            DO j=1,n
                                LOAD(j) = LOAD(j) + Work(j) * AText(j)
                                HeatTransferCoeff(j) = HeatTransferCoeff(j) + Work(j)
                            END DO
                            !------------------------------------------------------------------------------
                            ! Flux BC: k@T/@n = g
                            !------------------------------------------------------------------------------ 
                            HeatFluxArray => ListGetConstRealArray( Model % BCs(ModifDBC_Index) % Values, &
                                'Heat Flux', Found )
                            IF (Found) THEN
                                LOAD(1:n) = LOAD(1:n) +  HeatFluxArray(1, 1)
                            END IF
                            !------------------------------------------------------------------------------
                            ! Get element matrix and rhs due to boundary conditions 
                            !------------------------------------------------------------------------------
                            CALL TemperatureBoundary( LocalSTIFF,LocalFORCE, &
                                LOAD,HeatTransferCoeff,Element,n,ElementNodes )               
                            !------------------------------------------------------------------------------
                            ! If time dependent simulation, add the time derivative coefficients 
                            ! terms to stiff matrix
                            !------------------------------------------------------------------------------
                            IF ( TransientSimulation ) THEN
                                LocalMASS = 0.d0
                                CALL Add1stOrderTime( LocalMASS, LocalSTIFF, LocalFORCE, dt, n, 1, &
                                    TempPerm(Element % NodeIndexes(1:n)), Solver )  
                            END IF
                            !------------------------------------------------------------------------------
                            ! Add heat gap in the stiffmatrix (if implicit treatment)
                            !------------------------------------------------------------------------------
                            IF ( GetLogical( Model % BCs(ModifDBC_Index) % Values, 'Heat Gap', Found ) ) THEN
                                IF ( GetLogical( Model % BCs(ModifDBC_Index) % Values, 'Heat Gap Implicit', Found ) ) &
                                    CALL AddHeatGap( Solver, Element, LocalSTIFF, TempPerm, ElementNodes, n)
                            END IF
                            !------------------------------------------------------------------------------
                            ! Update global matrices from local matrices
                            !------------------------------------------------------------------------------         
                            CALL UpdateGlobalEquations( StiffMatrix, LocalSTIFF, &
                                forcevector, LocalFORCE, n, 1, TempPerm(Element % NodeIndexes) )
                        !------------------------------------------------------------------------------ 
                        END IF ! Heat flux BC
                    !------------------------------------------------------------------------------
                    END IF ! ModifDBC_Select_Element
            !------------------------------------------------------------------------------
            END IF ! ModifDBCToFluxBC
        !------------------------------------------------------------------------------
        END IF ! of heat-flux bc
    !------------------------------------------------------------------------------     
    END DO   ! Boundary elements    
    !------------------------------------------------------------------------------
    CALL DefaultFinishAssembly()

    CONTAINS
        SUBROUTINE Allocations()
            TYPE(ValueList_t),POINTER :: SolverParams

            N = Solver % Mesh % MaxElementNodes

            IF (AllocationsDone) THEN
                DEALLOCATE( &
                    ElementNodes % x,                &
                    ElementNodes % y,                &
                    ElementNodes % z,                &
                    LocalSTIFF,    & 
                    LocalMASS,     & 
                    LocalFORCE,              & 
                    Work,   &
                    HeatTransferCoeff,         &
                    AText,                     &
                    LOAD,                      &
                )
            END IF

            ALLOCATE( &
                ElementNodes % x( N ),                &
                ElementNodes % y( N ),                &
                ElementNodes % z( N ),                &
                LocalSTIFF( 2*N,2*N ),    & 
                LocalMASS( 2*N,2*N ),     & 
                LocalFORCE( 2*N ),              & 
                Work( N ),   &
                HeatTransferCoeff( N ),         &
                AText( N ),                     &
                LOAD( N ),                      &
            )

            SolverParams => GetSolverParams()

            !------------------------------------------------------------------------------
            !   Check if some Dirichlet BC have to be modified to flux BC at some elements
            !------------------------------------------------------------------------------
            ModifDBC = .FALSE.
            ModifDBCToFluxBC = .FALSE.

            ModifDBC = ListGetLogical( SolverParams, 'Dirichlet Modified BC ', Found )
            !------------------------------------------------------------------------------
            ! Check if modification to a flux BC:
            !------------------------------------------------------------------------------
            ModifDBCToFluxBC = ListGetLogical( SolverParams, &
                'Dirichlet Modified BC To Flux BC', Found )
            !------------------------------------------------------------------------------
            IF (ModifDBC .AND. ModifDBCToFluxBC) THEN
                !------------------------------------------------------------------------------
                ! Index of the boundary condition where is specified the modification:
                !------------------------------------------------------------------------------
                ModifDBC_Index = GetInteger( SolverParams, 'Dirichlet Modified BC Index', Found )
                IF (ModifDBC_Index>Model % NumberOfBCs) THEN
                    CALL Fatal( 'NumaSetDirichletBoundaries', 'Check Dirichlet Modified BC Index!' )
                END IF
                !------------------------------------------------------------------------------
                ! Distance from target under which is applied the modification:
                !------------------------------------------------------------------------------
                ModifDBC_DistMax = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                    'Dirichlet Modified BC Distance Max', Found )
                IF (.NOT. Found) THEN
                    CALL Error( 'NumaHeatSolve', 'Check Dirichlet Modified BC Distance Max!' )
                    ModifDBC_DistMax = 0.0 
                END IF
                !------------------------------------------------------------------------------
                ! x-coord of the target around which is applied the modification:
                !------------------------------------------------------------------------------
                ModifDBC_xc = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                    'Dirichlet Modified BC xc', Found )
                IF (.NOT. Found) THEN
                    CALL Error( 'NumaHeatSolve', 'Check Dirichlet Modified BC xc!' )
                END IF
                !------------------------------------------------------------------------------
                ! y-coord of the target around which is applied the modification:
                !------------------------------------------------------------------------------
                ModifDBC_yc = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                    'Dirichlet Modified BC yc', Found )
                IF (.NOT. Found) THEN
                    CALL Error( 'NumaHeatSolve', 'Check Dirichlet Modified BC yc!' )
                END IF
                !------------------------------------------------------------------------------
                ! z-coord of the target around which is applied the modification:
                !------------------------------------------------------------------------------
                ModifDBC_zc = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                    'Dirichlet Modified BC zc', Found )
                IF (.NOT. Found) THEN
                    CALL Error( 'NumaHeatSolve', 'Check Dirichlet Modified BC zc!' )
                END IF
                !------------------------------------------------------------------------------
                ! Diameter max of the elements on which is applied the modification:
                !------------------------------------------------------------------------------
                ModifDBC_DiamMax = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                    'Dirichlet Modified BC Diameter Max', Found )
                IF (.NOT. Found) THEN
                    CALL Info( 'NumaHeatSolve', 'No Dirichlet Modified BC Diameter Max specified' )
                    ModifDBC_DiamMax = -1.0 
                END IF
                !------------------------------------------------------------------------------
                ! Start time at which is applied the modification:
                !------------------------------------------------------------------------------
                ModifDBC_TimeStart = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                    'Dirichlet Modified BC Time Start', Found )
                IF (.NOT. Found) THEN
                    ModifDBC_TimeStart = 0.0
                END IF
            !------------------------------------------------------------------------------
            END IF
     !------------------------------------------------------------------------------ 

            AllocationsDone = .TRUE.
        END SUBROUTINE
END SUBROUTINE AssemblyNeumann
