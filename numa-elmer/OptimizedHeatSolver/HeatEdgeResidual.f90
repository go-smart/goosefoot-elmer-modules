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
MODULE HeatEdgeResidual
 CONTAINS
!------------------------------------------------------------------------------
  FUNCTION EdgeResidual( Model, Edge, Mesh, Quant, Perm ) RESULT( Indicator )
!------------------------------------------------------------------------------
     USE DefUtils
     IMPLICIT NONE

     TYPE(Model_t) :: Model
     INTEGER :: Perm(:)
     REAL(KIND=dp) :: Quant(:), Indicator(2)
     TYPE( Mesh_t ), POINTER    :: Mesh
     TYPE( Element_t ), POINTER :: Edge
!------------------------------------------------------------------------------

     TYPE(Nodes_t) :: Nodes, EdgeNodes
     TYPE(Element_t), POINTER :: Element

     INTEGER :: i,j,k,l,n,t,DIM,En,Pn
     LOGICAL :: stat
     REAL(KIND=dp), POINTER :: Hwrk(:,:,:)

     REAL(KIND=dp) :: SqrtMetric, Metric(3,3), Symb(3,3,3), dSymb(3,3,3,3)

     REAL(KIND=dp), ALLOCATABLE :: NodalConductivity(:), x(:), y(:), z(:), &
            EdgeBasis(:), Basis(:), dBasisdx(:,:), Temperature(:)

     REAL(KIND=dp) :: Grad(3,3), Normal(3), EdgeLength, Jump, Conductivity

     REAL(KIND=dp) :: u, v, w, s, detJ

     REAL(KIND=dp) :: ResidualNorm

     TYPE(GaussIntegrationPoints_t), TARGET :: IntegStuff

     LOGICAL :: First = .TRUE.
     SAVE Hwrk, First
!------------------------------------------------------------------------------

     !    Initialize:
     !    -----------
     IF ( First ) THEN
        First = .FALSE.
        NULLIFY( Hwrk )
     END IF

     SELECT CASE( CurrentCoordinateSystem() )
        CASE( AxisSymmetric, CylindricSymmetric )
           DIM = 3
        CASE DEFAULT
           DIM = CoordinateSystemDimension()
     END SELECT

     Metric = 0.0d0
     DO i = 1,3
        Metric(i,i) = 1.0d0
     END DO

     Grad = 0.0d0
!
!    ---------------------------------------------

     Element => Edge % BoundaryInfo % Left
     n = Element % TYPE % NumberOfNodes

     Element => Edge % BoundaryInfo % Right
     n = MAX( n, Element % TYPE % NumberOfNodes )

     ALLOCATE( Nodes % x(n), Nodes % y(n), Nodes % z(n) )

     En = Edge % TYPE % NumberOfNodes
     ALLOCATE( EdgeNodes % x(En), EdgeNodes % y(En), EdgeNodes % z(En) )

     EdgeNodes % x = Mesh % Nodes % x(Edge % NodeIndexes)
     EdgeNodes % y = Mesh % Nodes % y(Edge % NodeIndexes)
     EdgeNodes % z = Mesh % Nodes % z(Edge % NodeIndexes)

     ALLOCATE( NodalConductivity(En), EdgeBasis(En), Basis(n), &
        dBasisdx(n,3), x(En), y(En), z(En), Temperature(n) )

!    Integrate square of jump over edge:
!    -----------------------------------
     ResidualNorm = 0.0d0
     EdgeLength   = 0.0d0
     Indicator    = 0.0d0

     IntegStuff = GaussPoints( Edge )

     DO t=1,IntegStuff % n

        u = IntegStuff % u(t)
        v = IntegStuff % v(t)
        w = IntegStuff % w(t)

        stat = ElementInfo( Edge, EdgeNodes, u, v, w, detJ, &
             EdgeBasis, dBasisdx )

        Normal = NormalVector( Edge, EdgeNodes, u, v, .FALSE. )

        IF ( CurrentCoordinateSystem() == Cartesian ) THEN
           s = IntegStuff % s(t) * detJ
        ELSE
           u = SUM( EdgeBasis(1:En) * EdgeNodes % x(1:En) )
           v = SUM( EdgeBasis(1:En) * EdgeNodes % y(1:En) )
           w = SUM( EdgeBasis(1:En) * EdgeNodes % z(1:En) )

           CALL CoordinateSystemInfo( Metric, SqrtMetric, &
                      Symb, dSymb, u, v, w )
           s = IntegStuff % s(t) * detJ * SqrtMetric
        END IF

        ! 
        ! Compute flux over the edge as seen by elements
        ! on both sides of the edge:
        ! ----------------------------------------------
        DO i = 1,2
           SELECT CASE(i)
              CASE(1)
                 Element => Edge % BoundaryInfo % Left
              CASE(2)
                 Element => Edge % BoundaryInfo % Right
           END SELECT
!
!          Can this really happen (maybe it can...)  ?      
!          -------------------------------------------
           IF ( ANY( Perm( Element % NodeIndexes ) <= 0 ) ) CYCLE
!
!          Next, get the integration point in parent
!          local coordinates:
!          -----------------------------------------
           Pn = Element % TYPE % NumberOfNodes

           DO j = 1,En
              DO k = 1,Pn
                 IF ( Edge % NodeIndexes(j) == Element % NodeIndexes(k) ) THEN
                    x(j) = Element % TYPE % NodeU(k)
                    y(j) = Element % TYPE % NodeV(k)
                    z(j) = Element % TYPE % NodeW(k)
                    EXIT
                 END IF
              END DO
           END DO

           u = SUM( EdgeBasis(1:En) * x(1:En) )
           v = SUM( EdgeBasis(1:En) * y(1:En) )
           w = SUM( EdgeBasis(1:En) * z(1:En) )
!
!          Get parent element basis & derivatives at the integration point:
!          -----------------------------------------------------------------
           Nodes % x(1:Pn) = Mesh % Nodes % x(Element % NodeIndexes)
           Nodes % y(1:Pn) = Mesh % Nodes % y(Element % NodeIndexes)
           Nodes % z(1:Pn) = Mesh % Nodes % z(Element % NodeIndexes)

           stat = ElementInfo( Element, Nodes, u, v, w, detJ, &
             Basis, dBasisdx )
!
!          Material parameters:
!          --------------------
           k = ListGetInteger( Model % Bodies( &
                    Element % BodyId) % Values, 'Material', &
                     minv=1, maxv=Model % NumberOFMaterials )

           CALL ListGetRealArray( Model % Materials(k) % Values, &
                   'Heat Conductivity', Hwrk,En, Edge % NodeIndexes )

           NodalConductivity( 1:En ) = Hwrk( 1,1,1:En )
           Conductivity = SUM( NodalConductivity(1:En) * EdgeBasis(1:En) )
!
!          Temperature at element nodal points:
!          ------------------------------------
           Temperature(1:Pn) = Quant( Perm(Element % NodeIndexes) )
!
!          Finally, the flux:
!          ------------------
           DO j=1,DIM
              Grad(j,i) = Conductivity * SUM( dBasisdx(1:Pn,j) * Temperature(1:Pn) )
           END DO
        END DO

!       Compute squre of the flux jump:
!       -------------------------------   
        EdgeLength  = EdgeLength + s
        Jump = 0.0d0
        DO k=1,DIM
           IF ( CurrentCoordinateSystem() == Cartesian ) THEN
              Jump = Jump + (Grad(k,1) - Grad(k,2)) * Normal(k)
           ELSE
              DO l=1,DIM
                 Jump = Jump + &
                       Metric(k,l) * (Grad(k,1) - Grad(k,2)) * Normal(l)
              END DO
           END IF
        END DO
        ResidualNorm = ResidualNorm + s * Jump ** 2
     END DO

     IF ( CoordinateSystemDimension() == 3 ) THEN
        EdgeLength = SQRT(EdgeLength)
     END IF
     Indicator = EdgeLength * ResidualNorm

     DEALLOCATE( Nodes % x, Nodes % y, Nodes % z, x, y, z)
     DEALLOCATE( EdgeNodes % x, EdgeNodes % y, EdgeNodes % z)
     DEALLOCATE( NodalConductivity, EdgeBasis, Basis, dBasisdx, Temperature)

!------------------------------------------------------------------------------
  END FUNCTION EdgeResidual
!------------------------------------------------------------------------------
END MODULE HeatEdgeResidual
