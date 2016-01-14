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
    SUBROUTINE TemperatureBoundary( BoundaryMatrix,BoundaryVector, &
               LoadVector,NodalAlpha,Element,n,Nodes )
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Return element local matrices and RHS vector for boundary conditions
!  of diffusion convection equation: 
!
!  ARGUMENTS:
!
!  REAL(KIND=dp) :: BoundaryMatrix(:,:)
!     OUTPUT: coefficient matrix if equations
!
!  REAL(KIND=dp) :: BoundaryVector(:)
!     OUTPUT: RHS vector
!
!  REAL(KIND=dp) :: LoadVector(:,:)
!     INPUT: coefficient of the force term
!
!  REAL(KIND=dp) :: NodalAlpha(:,:)
!     INPUT: coefficient for temperature dependent term
!
!  TYPE(Element_t) :: Element
!       INPUT: Structure describing the element (dimension,nof nodes,
!               interpolation degree, etc...)
!
!   INTEGER :: n
!       INPUT: Number  of element nodes
!
!  TYPE(Nodes_t) :: Nodes
!       INPUT: Element node coordinates
!
!******************************************************************************
        USE MaterialModels
        USE DefUtils
        USE NumaAdaptive
        IMPLICIT NONE

        REAL(KIND=dp) :: BoundaryMatrix(:,:),BoundaryVector(:), &
            LoadVector(:),NodalAlpha(:), &
            Basis(n), dBasisdx(n,3), SqrtElementMetric, &
            U,V,W,S, &
            Force,Alpha
        
        REAL(KIND=dp), POINTER :: U_Integ(:),V_Integ(:),W_Integ(:),S_Integ(:)
        
        INTEGER :: n, t, q, p, N_Integ

        LOGICAL :: stat
        
        TYPE(Nodes_t)   :: Nodes
        TYPE(Element_t) :: Element
        TYPE(GaussIntegrationPoints_t), TARGET :: IntegStuff
!------------------------------------------------------------------------------
        BoundaryVector = 0.0D0
        BoundaryMatrix = 0.0D0
!------------------------------------------------------------------------------
!     Integration stuff
!------------------------------------------------------------------------------
        IntegStuff = GaussPoints( Element )
        U_Integ => IntegStuff % u
        V_Integ => IntegStuff % v
        W_Integ => IntegStuff % w
        S_Integ => IntegStuff % s
        N_Integ =  IntegStuff % n
!------------------------------------------------------------------------------
!     Loop over Gauss integration points
!------------------------------------------------------------------------------
        DO t=1,N_Integ
            U = U_Integ(t)
            V = V_Integ(t)
            W = W_Integ(t)
!------------------------------------------------------------------------------
!        Basis function values & derivatives at the integration point
!------------------------------------------------------------------------------
            stat = ElementInfo( Element, Nodes, u, v, w, SqrtElementMetric, Basis, dBasisdx )
            S = SqrtElementMetric * S_Integ(t)
!------------------------------------------------------------------------------
            Force = SUM( LoadVector(1:n)*Basis )
            Alpha = SUM( NodalAlpha(1:n)*Basis )
!--------------------------------------------------------------------------         
!           Update local boundary matrix
!--------------------------------------------------------------------------         
            DO p=1,N
                DO q=1,N
                    BoundaryMatrix(p,q) = BoundaryMatrix(p,q) + &
                        s * Alpha * Basis(q) * Basis(p)
                END DO
            END DO
!--------------------------------------------------------------------------         
!           Update local boundary vector
!-------------------------------------------------------------------------- 
            DO q=1,N
                BoundaryVector(q) = BoundaryVector(q) &
                    + s * Basis(q) * Force
!--------------------------------------------------------------------------                 
            END DO
!--------------------------------------------------------------------------         
        END DO ! Gauss integration points
!------------------------------------------------------------------------------     
    END SUBROUTINE TemperatureBoundary
!------------------------------------------------------------------------------
