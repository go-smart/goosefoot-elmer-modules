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
FUNCTION TemperatureInsideResidual( Model, Element, Mesh, &
    Quant, Perm, Fnorm, NDOFs ) RESULT( Indicator )
!------------------------------------------------------------------------------
!******************************************************************************
!
! Compute an indicator for remeshing process
!
! ARGUMENTS:
!
!  TYPE(Model_t) :: Model,  
!     INPUT: All model information (mesh,materials,BCs,etc...)
!
!  TYPE(Element_t), POINTER :: Element
!       INPUT: Structure describing the element (dimension,nof nodes,
!               interpolation degree, etc...)
!
!  TYPE( Mesh_t ), POINTER :: Mesh
!     INPUT: Current mesh
!
!   REAL(KIND=dp) :: Quant(:)
!       INPUT: Quantity computed on the mesh
!
!   INTEGER :: Perm(:)
!       INPUT: Element local numbering
!
!   REAL(KIND=dp) :: Fnorm
!       OUTPUT: volumic force norm
!
!   REAL(KIND=dp) :: Indicator(2)
!       OUTPUT: Remeshing criterion
!
!  INTEGER :: NDOFs
!   INPUT: Degrees of freedom of the solution Quant
!******************************************************************************
    USE CoordinateSystems
    USE ElementDescription
    !------------------------------------------------------------------------------
    IMPLICIT NONE
    !------------------------------------------------------------------------------
    TYPE(Model_t) :: Model
    INTEGER :: Perm(:), NDOFs
    REAL(KIND=dp) :: Quant(:), Indicator(2), Fnorm
    TYPE( Mesh_t ), POINTER    :: Mesh
    TYPE( Element_t ), POINTER :: Element
    !------------------------------------------------------------------------------
    ! Local variables
    !------------------------------------------------------------------------------
    TYPE(Nodes_t) :: Nodes
    TYPE(GaussIntegrationPoints_t), TARGET :: IntegStuff
    
    INTEGER :: i,j,n,t,DIM,k_dof

    LOGICAL :: stat

    REAL(KIND=dp), ALLOCATABLE :: Temperature(:,:), &
        Basis(:), dBasisdx(:,:), Grad(:,:)

    REAL(KIND=dp) :: u, v, w, s, detJ, GradNorm(NDOFs)
    !------------------------------------------------------------------------------
    ! Initialization:
    !------------------------------------------------------------------------------
    Indicator = 0.0d0
    Fnorm = 0.0
    GradNorm = 0.0d0
    
    DIM = CoordinateSystemDimension()
    !------------------------------------------------------------------------------
    ! Check if this eq. computed in this element:
    !------------------------------------------------------------------------------
    IF ( ANY( Perm( Element % NodeIndexes ) <= 0 ) ) RETURN
    !------------------------------------------------------------------------------
    ! Element nodal points:
    !------------------------------------------------------------------------------
    n = Element % TYPE % NumberOfNodes

    ALLOCATE( Nodes % x(n), Nodes % y(n), Nodes % z(n) )
    Nodes % x = Mesh % Nodes % x(Element % NodeIndexes)
    Nodes % y = Mesh % Nodes % y(Element % NodeIndexes)
    Nodes % z = Mesh % Nodes % z(Element % NodeIndexes)
    !------------------------------------------------------------------------------
    ALLOCATE( Temperature(NDOFs,n), Basis(n), dBasisdx(n,3), Grad(NDOFs,3) )
    !------------------------------------------------------------------------------
    ! Elementwise nodal solution:
    !------------------------------------------------------------------------------
    DO k_dof =1,NDOFs
        DO i=1,n
            j = Perm(Element % NodeIndexes(i))
            Temperature(k_dof,i) = Quant((j-1)*NDOFs + k_dof )  
        END DO
    END DO
    !------------------------------------------------------------------------------
    ! Integrate square of residual over element:
    !------------------------------------------------------------------------------
    IntegStuff = GaussPoints( Element )
    !------------------------------------------------------------------------------
    DO t=1,IntegStuff % n
    !------------------------------------------------------------------------------
        u = IntegStuff % u(t)
        v = IntegStuff % v(t)
        w = IntegStuff % w(t)
        
        Basis = 0.0d0
        dBasisdx = 0.0d0
        s=1
        
        stat = ElementInfo( Element, Nodes, u, v, w, detJ, Basis, dBasisdx )
        s = IntegStuff % s(t) * detJ

        Grad = 0.0d0
        
        DO k_dof =1,NDOFs   
            DO i = 1, DIM
                Grad(k_dof,i) = SUM( dBasisdx(1:n,i) * Temperature(k_dof,1:n) )
            END DO
            GradNorm(k_dof) = GradNorm(k_dof) + s * SUM( Grad(k_dof,1:DIM) * Grad(k_dof,1:DIM) ) 
        END DO !k_dof
    !------------------------------------------------------------------------------
    END DO
    !------------------------------------------------------------------------------
    Indicator = Element % hK**2 * MAXVAL(GradNorm)
    !------------------------------------------------------------------------------
    DEALLOCATE( Temperature, Basis, dBasisdx, Grad  )
!------------------------------------------------------------------------------
END FUNCTION TemperatureInsideResidual
!------------------------------------------------------------------------------
