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
    SUBROUTINE TemperatureCompose( MassMatrix,StiffMatrix,ForceVector,  &
      LoadVector, &
      NodalCT,NodalC0,NodalC1,NodalC2, &
      UX,UY,UZ, NodalDensity, NodalPerfusionCoeff, &
      Stabilize, UseBubbles,Element,n,Nodes, Integ_Force, LoadOnly )
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Return element local matrices and RHS vector for diffusion-convection
!  equation: 
!
!  ARGUMENTS:
!
!  REAL(KIND=dp) :: MassMatrix(:,:)
!     OUTPUT: time derivative coefficient matrix
!
!  REAL(KIND=dp) :: StiffMatrix(:,:)
!     OUTPUT: rest of the equation coefficients
!
!  REAL(KIND=dp) :: ForceVector(:)
!     OUTPUT: RHS vector
!
!  REAL(KIND=dp) :: LoadVector(:,:)
!     INPUT: Nodal values of RHS
!
!  !REAL(KIND=dp) :: NodalInterMaterialCoeff(:,:)
!  !   INPUT: Nodal values of the transfer coeff. if convection transfer between materials
!
!  REAL(KIND=dp) :: NodalCT(:),NodalC0,NodalC1(:,:)
!     INPUT: Coefficient of the time derivative term, 0 degree term, and
!            the convection term respectively
!
!  REAL(KIND=dp) :: NodalC2(:,:,:,:)
!     INPUT: Nodal values of the diffusion term coefficient tensor
!
!  REAL(KIND=dp) :: UX(:,:),UY(:,:),UZ(:,:)
!     INPUT: Nodal values of velocity components from previous iteration
!           used only if coefficient of the convection term (C1) is nonzero
!
!  REAL(KIND=dp) :: NodalPerfusionCoeff(:)
!     INPUT: Nodal values of perfusion coefficient for classical bio-heat
!       equations
!          
!
!  LOGICAL :: Stabilize
!     INPUT: Should stabilzation be used ? Used only if coefficient of the
!            convection term (C1) is nonzero
!
!  LOGICAL :: UseBubbles
!     INPUT: Should bubbles be used for stabilzation? Used only if coefficient of the
!            convection term (C1) is nonzero
!
!  TYPE(Element_t) :: Element
!       INPUT: Structure describing the element (dimension,nof nodes,
!               interpolation degree, etc...)
!
!  INTEGER :: n
!       INPUT: Number of element nodes
!
!  TYPE(Nodes_t) :: Nodes
!       INPUT: Element node coordinates
!
!  LOGICAL :: LoadOnly
!     INPUT: Should mass and stiffness be reassembled or only load? Overridden by
!            convect and stabilize
!******************************************************************************
        USE MaterialModels
        USE DefUtils
        USE NumaAdaptive
        IMPLICIT NONE

        REAL(KIND=dp) :: ForceVector(:), &
            MassMatrix(:,:),StiffMatrix(:,:),LoadVector(:),UX(:),UY(:),UZ(:), &
            NodalDensity(:), NodalC2(:,:,:), &
            !NodalInterMaterialCoeff(:), &
            NodalC0(:), NodalC1(:), NodalCT(:), &
            Integ_Force, NodalPerfusionCoeff(:)
        LOGICAL :: Stabilize, UseBubbles

        INTEGER :: n

        TYPE(Nodes_t) :: Nodes
        TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
!    Local variables
!------------------------------------------------------------------------------
        REAL(KIND=dp) :: &
            Basis(2*n), dBasisdx(2*n,3), ddBasisddx(n,3,3), &
            SqrtElementMetric, s, u, v, w, &
            Velo(3), Force, dVelodx(3,3),&
            A,M, &
            Load, &
            VNorm, hK, mK, SU(n), SW(n), &
            Pe,Tau, Density, &
            C00,C0,C1,CT,C2(3,3),dC2dx(3,3,3) !B
            
        REAL(KIND=dp), DIMENSION(:), POINTER :: U_Integ,V_Integ,W_Integ,S_Integ
            
        INTEGER :: i,j,k,p,q,t,dim,N_Integ,NBasis
     
        LOGICAL :: stat, Convection, Bubbles, LoadOnly
        
        TYPE(GaussIntegrationPoints_t), TARGET :: IntegStuff
!------------------------------------------------------------------------------
!       Get model dimension
!------------------------------------------------------------------------------
        dim = CoordinateSystemDimension()
!------------------------------------------------------------------------------
!       Initialization
!------------------------------------------------------------------------------
        ForceVector = 0.0D0
        StiffMatrix = 0.0D0
        MassMatrix  = 0.0D0
        Load = 0.0D0
        A = 0.0D0
        !B = 0.0D0
        M = 0.0D0
!------------------------------------------------------------------------------
!       Check if any convection
!------------------------------------------------------------------------------     
!       Get numbre of basis functions       
!------------------------------------------------------------------------------ 
        NBasis = n
        !BUBBLES:
!     Integration stuff
!------------------------------------------------------------------------------
        !BUBBLES:       
        IntegStuff = GaussPoints( element )

        U_Integ => IntegStuff % u
        V_Integ => IntegStuff % v
        W_Integ => IntegStuff % w
        S_Integ => IntegStuff % s
        N_Integ =  IntegStuff % n
!------------------------------------------------------------------------------
!    Stabilization parameters: hK, mK (take a look at Franca et.al.)
!    If there is no convection term we don t need stabilization.
!------------------------------------------------------------------------------
!     Loop over Gauss integration points
!------------------------------------------------------------------------------
        DO t=1,N_Integ

            u = U_Integ(t)
            v = V_Integ(t)
            w = W_Integ(t)
!------------------------------------------------------------------------------
!     Basis function values & derivatives at the integration point
!------------------------------------------------------------------------------
            !BUBBLES:
            stat = ElementInfo( Element,Nodes,u,v,w,SqrtElementMetric, &
                     Basis,dBasisdx,ddBasisddx,.FALSE. )    

            s = SqrtElementMetric * S_Integ(t)      

            IF (.NOT. LoadOnly) THEN
!-----------    -------------------------------------------------------------------
!        Coe    fficient of the convection (C1), time derivative (CT), and O-order  
!               derivative (C0) terms at the integration point
!-----------    -------------------------------------------------------------------         
                C0 = SUM( NodalC0(1:n) * Basis(1:n) )   
                C1 = SUM( NodalC1(1:n) * Basis(1:n) )
                CT = SUM( NodalCT(1:n) * Basis(1:n) )  
!-----------    -------------------------------------------------------------------
!        For     a given DOF, C00 is the coefficient relative to the other DOF in 
!               the convective  transfer term
!-----------    -------------------------------------------------------------------                 
                C00 = 0.0D0
                
                !C0 = C0 + SUM( NodalInterMaterialCoeff(1:n) * Basis(1:n) )    
                !C00 = C00 - SUM( NodalInterMaterialCoeff(1:n) * Basis(1:n) )
                C0 = C0 + SUM( NodalPerfusionCoeff(1:n) * Basis(1:n) )
!-----------    -------------------------------------------------------------------
!        App    roximation of density and coefficient of the diffusion term 
!-----------    -------------------------------------------------------------------
                Density = SUM( NodalDensity(1:n) * Basis(1:n) ) 

                DO i=1,dim
                    DO j=1,dim
                        C2(i,j) = SUM( NodalC2(i,j,1:n) * Basis(1:n) )
                    END DO
                END DO
!-----------    -------------------------------------------------------------------
!        Loo    p over basis functions of both unknowns and weights
!-----------    -------------------------------------------------------------------
                DO p=1,NBasis
                    DO q=1,NBasis
        !---    -----------------------------------------------------------------------
        !           The diffusive-convective equation without stabilization
        !---    -----------------------------------------------------------------------
                        M = CT * Basis(q) * Basis(p)
                        A = C0 * Basis(q) * Basis(p)
                        !B = C00 * Basis(q) * Basis(p)
        !---    -----------------------------------------------------------------------
        !           The diffusion term
        !---    -----------------------------------------------------------------------
                        DO i=1,dim
                            DO j=1,dim
                                A = A + C2(i,j) * dBasisdx(q,i) * dBasisdx(p,j)
                            END DO
                        END DO

        !---    -----------------------------------------------------------------------
        !           Stiff matrix: diffusion, convection, 0-degree term
        !---    -----------------------------------------------------------------------
                        StiffMatrix(p,q) = &
                            StiffMatrix(p,q) + s * A
        !---    -----------------------------------------------------------------------
        !           Mass matrix: time derivative
        !---    -----------------------------------------------------------------------             
                        MassMatrix(p,q)  = &
                            MassMatrix(p,q)  + s * M

                    END DO ! q
                END DO ! p
            END IF

!------------------------------------------------------------------------------
!        The righthand side
!------------------------------------------------------------------------------
!        Force at the integration point
!------------------------------------------------------------------------------
            Force = SUM( LoadVector(1:n)*Basis(1:n) )
!------------------------------------------------------------------------------
            DO q=1,NBasis
                Load = Basis(q)
!--------------------------------------------------------------------------
!           Force vector
!--------------------------------------------------------------------------                     
                ForceVector(q) = ForceVector(q) + s * Force * Load
            END DO
            Integ_Force =  Integ_Force + Force * s 
!------------------------------------------------------------------------------
        END DO ! Gauss integration points
!------------------------------------------------------------------------------
   END SUBROUTINE TemperatureCompose
!------------------------------------------------------------------------------
