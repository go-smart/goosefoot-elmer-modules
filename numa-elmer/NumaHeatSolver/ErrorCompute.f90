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

SUBROUTINE ErrorCompute( Solver, Model, Temperature, Reorder, ErrorH1, ErrorL2 )
!------------------------------------------------------------------------------
!******************************************************************************
!
!     Compute error at model nodes between numerical and analytical solution
!
!     ARGUMENTS:
!
!       TYPE(Model_t) :: Model
!           INPUT: All model information (mesh, materials, BCs, etc...)
!
!       REAL(KIND=dp) :: Temperature(:)
!           INPUT: Temperature, solution of the linear system 
!
!       INTEGER :: Reorder
!           INPUT: Element local numbering
!
!
!******************************************************************************
    USE MaterialModels
    USE DefUtils
    USE NumaAdaptive
!------------------------------------------------------------------------------
    IMPLICIT NONE
        TYPE(Solver_t) :: Solver
        TYPE(Model_t) :: Model
        REAL(KIND=dp) :: Temperature(:), ErrorH1, ErrorL2
        INTEGER :: Reorder(:)
!------------------------------------------------------------------------------
!     Local variables
!------------------------------------------------------------------------------
        TYPE(Element_t), POINTER :: Element
        TYPE(Nodes_t) :: Nodes 
        TYPE(GaussIntegrationPoints_t), TARGET :: IntegStuff

        REAL(KIND=dp), POINTER :: U_Integ(:), V_Integ(:), W_Integ(:), S_Integ(:)
        
        REAL(KIND=dp) :: &
            Basis(Model % MaxElementNodes), &
            dBasisdx(Model % MaxElementNodes,3), &
            SqrtElementMetric, &
            ElementTemperature(Model % MaxElementNodes), &
            ElementAnalyticalSol(Model % MaxElementNodes), &
            s, ug, vg, wg, Err, GradError,&
            SqrtMetric, Metric(3,3), Symb(3,3,3), dSymb(3,3,3,3), &
            x, y, z
            
        INTEGER, POINTER :: NodeIndexes(:)
        INTEGER :: N_Integ, t, tg, i, j, dim, k, n
        LOGICAL :: Stat         

        REAL(KIND=dp), EXTERNAL :: AnalyticalSolution

!------------------------------------------------------------------------------
!       Allocate the nodes coordinates vectors
!------------------------------------------------------------------------------
        ALLOCATE( Nodes % x( Model % MaxElementNodes ) )
        ALLOCATE( Nodes % y( Model % MaxElementNodes ) )
        ALLOCATE( Nodes % z( Model % MaxElementNodes ) )
!------------------------------------------------------------------------------
!       Get the model dimension
!------------------------------------------------------------------------------     
        dim = CoordinateSystemDimension()
!------------------------------------------------------------------------------
!       Initialize the error
!------------------------------------------------------------------------------         
        ErrorL2 = 0.0D0
        ErrorH1 = 0.0D0
        !------------------------------------------------------------------------------
        !     Go through the elements, we will compute on average of elementwise
        !     fluxes to nodes of the model
        !------------------------------------------------------------------------------
        DO t = 1,Solver % NumberOfActiveElements
            !------------------------------------------------------------------------------
            ! Check if this element belongs to a body where error
            ! should be calculated
            !------------------------------------------------------------------------------
            Element => Solver % Mesh % Elements( Solver % ActiveElements( t ) )
            !------------------------------------------------------------------------------
            ! Global indexes of the nodes of the element
            !------------------------------------------------------------------------------
            NodeIndexes => Element % NodeIndexes
            !------------------------------------------------------------------------------
            ! Number of nodes of the element
            !------------------------------------------------------------------------------
            n = Element % TYPE % NumberOfNodes
            !------------------------------------------------------------------------------
            ! Check if the element is active
            !------------------------------------------------------------------------------
            IF ( ANY(Reorder(NodeIndexes) == 0) ) CYCLE
            !------------------------------------------------------------------------------
            ! Get coordinates of nodes:
            !------------------------------------------------------------------------------
            Nodes % x(1:n) = Model % Nodes % x( NodeIndexes )
            Nodes % y(1:n) = Model % Nodes % y( NodeIndexes )
            Nodes % z(1:n) = Model % Nodes % z( NodeIndexes )
            !------------------------------------------------------------------------------
            ! Get (element local) numerical solution:
            !------------------------------------------------------------------------------
            ElementTemperature = 0.0D0
            DO i=1,n
                k = Reorder(NodeIndexes(i))
                ElementTemperature(i) = Temperature(k)
            END DO
            !------------------------------------------------------------------------------
            ! Get (element local) analytical solution:
            !------------------------------------------------------------------------------
            ElementAnalyticalSol = 0.0D0
            DO i=1,n
                ElementAnalyticalSol(i) = AnalyticalSolution(Nodes % x(i),Nodes % y(i),Nodes % z(i))
            END DO
            !------------------------------------------------------------------------------
            ! Gauss integration stuff
            !------------------------------------------------------------------------------
            IntegStuff = GaussPoints( Element )
            U_Integ => IntegStuff % u
            V_Integ => IntegStuff % v
            W_Integ => IntegStuff % w
            S_Integ => IntegStuff % s
            N_Integ =  IntegStuff % n
            !------------------------------------------------------------------------------
            ! Loop over Gauss integration points
            !------------------------------------------------------------------------------
            DO tg=1,N_Integ
            !------------------------------------------------------------------------------
                ug = U_Integ(tg)
                vg = V_Integ(tg)
                wg = W_Integ(tg)
                !------------------------------------------------------------------------------
                ! Need SqrtElementMetric and Basis at the integration point
                !------------------------------------------------------------------------------
                stat = ElementInfo( Element, Nodes,ug,vg,wg, &
                    SqrtElementMetric,Basis,dBasisdx )
                !------------------------------------------------------------------------------
                ! Coordinatesystem dependent info
                !------------------------------------------------------------------------------
                s = 1
                CALL CoordinateSystemInfo( Metric,SqrtMetric,Symb,dSymb,x,y,z )
                s = s * SqrtMetric * SqrtElementMetric * S_Integ(tg)
                !------------------------------------------------------------------------------
                ! Approximation of the L2 and H1 error by using the basis functions
                !------------------------------------------------------------------------------
                Err = 0.0d0
                GradError = 0.0d0

                !------------------------------------------------------------------------------
                ! L^2-part of error norm 
                !------------------------------------------------------------------------------
                Err = SUM( Basis(1:n) * &
                    abs(ElementTemperature(1:n)-ElementAnalyticalSol(1:n)) )

                ErrorL2 = ErrorL2 + s * Err * Err  
                ErrorH1 = ErrorH1 + s * Err * Err
                !------------------------------------------------------------------------------
                ! H^1_0-part of error norm 
                !------------------------------------------------------------------------------
                DO j = 1, DIM
                    GradError =  SUM( dBasisdx(1:n,j) * &
                    abs(ElementTemperature(1:n)-ElementAnalyticalSol(1:n)) )
                    ErrorH1 = ErrorH1 + s * GradError * GradError
                END DO
            !------------------------------------------------------------------------------
        END DO ! of the Gauss integration points
    !------------------------------------------------------------------------------
    END DO ! of the bulk elements
    !------------------------------------------------------------------------------
    ErrorL2 = sqrt(ErrorL2) 
    !------------------------------------------------------------------------------
    DEALLOCATE( Nodes % x, Nodes % y, Nodes % z)
    !------------------------------------------------------------------------------
END SUBROUTINE ErrorCompute
