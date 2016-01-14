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
    FUNCTION TotalPowerCompose( LoadVector,Element,n,Nodes) RESULT(TotalPower)
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Return element local matrices and RHS vector for diffusion-convection
!  equation: 
!
!  ARGUMENTS:
!
!
!  REAL(KIND=dp) :: LoadVector(:,:)
!     INPUT: Nodal values of RHS
!
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
!******************************************************************************
    USE MaterialModels
    USE DefUtils
    USE NumaAdaptive
!------------------------------------------------------------------------------
    IMPLICIT NONE
        REAL(KIND=dp) :: LoadVector(:), TotalPower
        INTEGER :: n
        TYPE(Nodes_t) :: Nodes
        TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
!    Local variables
!------------------------------------------------------------------------------
        REAL(KIND=dp) :: Basis(2*n), dBasisdx(2*n,3), ddBasisddx(n,3,3), &
            SqrtElementMetric, s, u, v, w, Force

        REAL(KIND=dp), DIMENSION(:), POINTER :: U_Integ,V_Integ,W_Integ,S_Integ
            
        INTEGER :: t,N_Integ,NBasis
        LOGICAL :: stat
        TYPE(GaussIntegrationPoints_t), TARGET :: IntegStuff
!------------------------------------------------------------------------------
!       Get numbre of basis functions       
!------------------------------------------------------------------------------ 
        NBasis = n
!------------------------------------------------------------------------------
!     Integration stuff
!------------------------------------------------------------------------------     
        IntegStuff = GaussPoints( element )

        U_Integ => IntegStuff % u
        V_Integ => IntegStuff % v
        W_Integ => IntegStuff % w
        S_Integ => IntegStuff % s
        N_Integ =  IntegStuff % n
!------------------------------------------------------------------------------
!     Loop over Gauss integration points
!------------------------------------------------------------------------------
        DO t=1,N_Integ
!------------------------------------------------------------------------------
            u = U_Integ(t)
            v = V_Integ(t)
            w = W_Integ(t)
!------------------------------------------------------------------------------
!     Basis function values & derivatives at the integration point
!------------------------------------------------------------------------------
            stat = ElementInfo( Element,Nodes,u,v,w,SqrtElementMetric, &
                 Basis,dBasisdx,ddBasisddx,.FALSE. )         
            s = SqrtElementMetric * S_Integ(t)      
            Force = SUM( LoadVector(1:n)*Basis(1:n) )
            TotalPower =  TotalPower + Force * s 
!------------------------------------------------------------------------------
        END DO ! Gauss integration points
!------------------------------------------------------------------------------
   END FUNCTION TotalPowerCompose
!------------------------------------------------------------------------------
