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
MODULE HeatIntegOverA
 CONTAINS
  SUBROUTINE IntegOverA( BoundaryMatrix, BoundaryVector, &
       LOAD, NodalAlpha, Element, n, m, Nodes )

       USE DefUtils
       USE MaterialModels
       USE HeatPhaseDefs
       IMPLICIT NONE
  !------------------------------------------------------------------------------
       REAL(KIND=dp) :: BoundaryMatrix(:,:),BoundaryVector(:), &
                      LOAD(:),NodalAlpha(:)

       TYPE(Nodes_t)   :: Nodes
       TYPE(Element_t) :: Element

       INTEGER :: n,  m

       REAL(KIND=dp) :: Basis(n)
       REAL(KIND=dp) :: dBasisdx(n,3),SqrtElementMetric

       REAL(KIND=dp) :: u,v,w,s,x,y,z
       REAL(KIND=dp) :: Force,Alpha
       REAL(KIND=dp), POINTER :: U_Integ(:),V_Integ(:),W_Integ(:),S_Integ(:)

       INTEGER :: t,q,p,N_Integ

       TYPE(GaussIntegrationPoints_t), TARGET :: IntegStuff

       LOGICAL :: stat
  !------------------------------------------------------------------------------

       BoundaryVector = 0.0D0
       BoundaryMatrix = 0.0D0
  !------------------------------------------------------------------------------
  !    Integration stuff
  !------------------------------------------------------------------------------
       IntegStuff = GaussPoints( Element )
       U_Integ => IntegStuff % u
       V_Integ => IntegStuff % v
       W_Integ => IntegStuff % w
       S_Integ => IntegStuff % s
       N_Integ =  IntegStuff % n

  !------------------------------------------------------------------------------
  !   Now we start integrating
  !------------------------------------------------------------------------------
       DO t=1,N_Integ
         u = U_Integ(t)
         v = V_Integ(t)
         w = W_Integ(t)
  !------------------------------------------------------------------------------
  !     Basis function values & derivatives at the integration point
  !------------------------------------------------------------------------------
         stat = ElementInfo( Element,Nodes,u,v,w,SqrtElementMetric, &
                    Basis,dBasisdx )

         s = SqrtElementMetric * S_Integ(t)
  !------------------------------------------------------------------------------
  !      Coordinatesystem dependent info
  !------------------------------------------------------------------------------
         IF ( CurrentCoordinateSystem() /= Cartesian ) THEN
           x = SUM( Nodes % x(1:n)*Basis )
           y = SUM( Nodes % y(1:n)*Basis )
           z = SUM( Nodes % z(1:n)*Basis )
           s = s * CoordinateSqrtMetric( x,y,z )
         END IF
  !------------------------------------------------------------------------------
         Force = SUM( LOAD(1:n) * Basis )
         Alpha = SUM( NodalAlpha(1:n) * Basis )

         DO p=1,N
           DO q=1,M
             BoundaryMatrix(p,q) = BoundaryMatrix(p,q) + &
                    s * Alpha * Basis(p) / m
           END DO
         END DO

         DO p=1,N
           BoundaryVector(p) = BoundaryVector(p) + s * Force * Basis(p)
         END DO
       END DO
  END SUBROUTINE IntegOverA
END MODULE HeatIntegOverA
