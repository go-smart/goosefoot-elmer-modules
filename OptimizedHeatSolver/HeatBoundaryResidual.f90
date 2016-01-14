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
MODULE HeatBoundaryResidual
 CONTAINS
!------------------------------------------------------------------------------
  FUNCTION BoundaryResidual( Model, Edge, Mesh, Quant, Perm,Gnorm ) RESULT( Indicator )
!------------------------------------------------------------------------------
     USE DefUtils
     USE Radiation

     IMPLICIT NONE
!------------------------------------------------------------------------------
     TYPE(Model_t) :: Model
     INTEGER :: Perm(:)
     REAL(KIND=dp) :: Quant(:), Indicator(2), Gnorm
     TYPE( Mesh_t ), POINTER    :: Mesh
     TYPE( Element_t ), POINTER :: Edge
!------------------------------------------------------------------------------

     TYPE(Nodes_t) :: Nodes, EdgeNodes
     TYPE(Element_t), POINTER :: Element

     INTEGER :: i,j,k,l,t,DIM,Pn,En
     LOGICAL :: stat, Found

     REAL(KIND=dp), POINTER :: Hwrk(:,:,:), NodalEmissivityPtr(:)

     REAL(KIND=dp) :: SqrtMetric, Metric(3,3), Symb(3,3,3), dSymb(3,3,3,3)

     REAL(KIND=dp), ALLOCATABLE :: NodalConductivity(:), ExtTemperature(:), &
       TransferCoeff(:), EdgeBasis(:), Basis(:), x(:), y(:), z(:), &
       dBasisdx(:,:), Temperature(:), Flux(:), NodalEmissivity(:)

     REAL(KIND=dp) :: Conductivity, Emissivity, StefanBoltzmann

     REAL(KIND=dp) :: Normal(3), EdgeLength, gx, gy, gz

     REAL(KIND=dp) :: u, v, w, s, detJ

     REAL(KIND=dp) :: Residual, ResidualNorm

     TYPE(GaussIntegrationPoints_t), TARGET :: IntegStuff

     LOGICAL :: First = .TRUE., Dirichlet
     SAVE Hwrk, First
!------------------------------------------------------------------------------

!    Initialize:
!    -----------
     IF ( First ) THEN
        First = .FALSE.
        NULLIFY( Hwrk )
     END IF

     Indicator = 0.0d0
     Gnorm     = 0.0d0

     Metric = 0.0d0
     DO i=1,3
        Metric(i,i) = 1.0d0
     END DO

     SELECT CASE( CurrentCoordinateSystem() )
        CASE( AxisSymmetric, CylindricSymmetric )
           dim = 3
        CASE DEFAULT
           dim = CoordinateSystemDimension()
     END SELECT
!    
!    ---------------------------------------------

     Element => Edge % BoundaryInfo % Left

     IF ( .NOT. ASSOCIATED( Element ) ) THEN
        Element => Edge % BoundaryInfo % Right
     ELSE IF ( ANY( Perm( Element % NodeIndexes ) <= 0 ) ) THEN
        Element => Edge % BoundaryInfo % Right
     END IF

     IF ( .NOT. ASSOCIATED( Element ) ) RETURN
     IF ( ANY( Perm( Element % NodeIndexes ) <= 0 ) ) RETURN

     En = Edge % TYPE % NumberOfNodes
     Pn = Element % TYPE % NumberOfNodes

     ALLOCATE( EdgeNodes % x(En), EdgeNodes % y(En), EdgeNodes % z(En) )

     EdgeNodes % x = Mesh % Nodes % x(Edge % NodeIndexes)
     EdgeNodes % y = Mesh % Nodes % y(Edge % NodeIndexes)
     EdgeNodes % z = Mesh % Nodes % z(Edge % NodeIndexes)

     ALLOCATE( Nodes % x(Pn), Nodes % y(Pn), Nodes % z(Pn) )

     Nodes % x = Mesh % Nodes % x(Element % NodeIndexes)
     Nodes % y = Mesh % Nodes % y(Element % NodeIndexes)
     Nodes % z = Mesh % Nodes % z(Element % NodeIndexes)

     ALLOCATE( Temperature(Pn), Basis(Pn), ExtTemperature(En), &
        TransferCoeff(En), x(En), y(En), z(En), EdgeBasis(En), &
        dBasisdx(Pn,3), NodalConductivity(En), Flux(En), &
        NodalEmissivity(En) ) 

     DO l = 1,En
       DO k = 1,Pn
          IF ( Edge % NodeIndexes(l) == Element % NodeIndexes(k) ) THEN
             x(l) = Element % TYPE % NodeU(k)
             y(l) = Element % TYPE % NodeV(k)
             z(l) = Element % TYPE % NodeW(k)
             EXIT
          END IF
       END DO
     END DO
!
!    Integrate square of residual over boundary element:
!    ---------------------------------------------------

     Indicator    = 0.0d0
     EdgeLength   = 0.0d0
     ResidualNorm = 0.0d0

     DO j=1,Model % NumberOfBCs
        IF ( Edge % BoundaryInfo % Constraint /= Model % BCs(j) % Tag ) CYCLE

!       IF ( .NOT. ListGetLogical( Model % BCs(j) % Values, &
!                 'Heat Flux BC', Found ) ) CYCLE

!
!       Check if dirichlet BC given:
!       ----------------------------
        s = ListGetConstReal( Model % BCs(j) % Values,'Temperature',Dirichlet )

!       Get various flux bc options:
!       ----------------------------

!       ...given flux:
!       --------------
        Flux(1:En) = ListGetReal( Model % BCs(j) % Values, &
          'Heat Flux', En, Edge % NodeIndexes, Found )

!       ...convective heat transfer:
!       ----------------------------
        TransferCoeff(1:En) =  ListGetReal( Model % BCs(j) % Values, &
          'Heat Transfer Coefficient', En, Edge % NodeIndexes, Found )

        ExtTemperature(1:En) = ListGetReal( Model % BCs(j) % Values, &
          'External Temperature', En, Edge % NodeIndexes, Found )

!       ...black body radiation:
!       ------------------------
        Emissivity      = 0.0d0
        StefanBoltzmann = 0.0d0

        SELECT CASE(ListGetString(Model % BCs(j) % Values,'Radiation',Found))
           !------------------
           CASE( 'idealized' )
           !------------------

              NodalEmissivity(1:En) = ListGetReal( Model % BCs(j) % Values, &
                   'Emissivity', En, Edge % NodeIndexes, Found)
              IF(.NOT. Found) THEN
                 NodalEmissivityPtr => GetParentMatProp( 'Emissivity', Edge)
                 NodalEmissivity = NodalEmissivityPtr
              END IF
              Emissivity = SUM( NodalEmissivity(1:En)) / En

              StefanBoltzMann = &
                    ListGetConstReal( Model % Constants,'Stefan Boltzmann' )

           !---------------------
           CASE( 'diffuse gray' )
           !---------------------

              NodalEmissivity(1:En) = ListGetReal( Model % BCs(j) % Values, &
                   'Emissivity', En, Edge % NodeIndexes, Found)
              IF(.NOT. Found) THEN
                 NodalEmissivityPtr => GetParentMatProp( 'Emissivity', Edge)
                 NodalEmissivity = NodalEmissivityPtr
              END IF
              Emissivity = SUM( NodalEmissivity(1:En)) / En

              StefanBoltzMann = &
                    ListGetConstReal( Model % Constants,'Stefan Boltzmann' )

              ExtTemperature(1:En) =  ComputeRadiationLoad( Model, &
                      Mesh, Edge, Quant, Perm, Emissivity )
        END SELECT

!       get material parameters:
!       ------------------------
        k = ListGetInteger(Model % Bodies(Element % BodyId) % Values,'Material', &
                    minv=1, maxv=Model % NumberOFMaterials)

        CALL ListGetRealArray( Model % Materials(k) % Values, &
               'Heat Conductivity', Hwrk, En, Edge % NodeIndexes )

        NodalConductivity( 1:En ) = Hwrk( 1,1,1:En )

!       elementwise nodal solution:
!       ---------------------------
        Temperature(1:Pn) = Quant( Perm(Element % NodeIndexes) )

!       do the integration:
!       -------------------
        EdgeLength   = 0.0d0
        ResidualNorm = 0.0d0

        IntegStuff = GaussPoints( Edge )

        DO t=1,IntegStuff % n
           u = IntegStuff % u(t)
           v = IntegStuff % v(t)
           w = IntegStuff % w(t)

           stat = ElementInfo( Edge, EdgeNodes, u, v, w, detJ, &
               EdgeBasis, dBasisdx )

           Normal = NormalVector( Edge, EdgeNodes, u, v, .TRUE. )

           IF ( CurrentCoordinateSystem() == Cartesian ) THEN
              s = IntegStuff % s(t) * detJ
           ELSE
              gx = SUM( EdgeBasis(1:En) * EdgeNodes % x(1:En) )
              gy = SUM( EdgeBasis(1:En) * EdgeNodes % y(1:En) )
              gz = SUM( EdgeBasis(1:En) * EdgeNodes % z(1:En) )
      
              CALL CoordinateSystemInfo( Metric, SqrtMetric, &
                         Symb, dSymb, gx, gy, gz )

              s = IntegStuff % s(t) * detJ * SqrtMetric
           END IF

!
!          Integration point in parent element local
!          coordinates:
!          -----------------------------------------
           u = SUM( EdgeBasis(1:En) * x(1:En) )
           v = SUM( EdgeBasis(1:En) * y(1:En) )
           w = SUM( EdgeBasis(1:En) * z(1:En) )

           stat = ElementInfo( Element, Nodes, u, v, w, detJ, &
                 Basis, dBasisdx )
!
!          Heat conductivity at the integration point:
!          --------------------------------------------
           Conductivity = SUM( NodalConductivity(1:En) * EdgeBasis(1:En) )
!
!          given flux at integration point:
!          --------------------------------
           Residual = -SUM( Flux(1:En) * EdgeBasis(1:En) )

!          convective ...:
!          ----------------
           Residual = Residual + SUM(TransferCoeff(1:En) * EdgeBasis(1:En)) * &
                     ( SUM( Temperature(1:Pn) * Basis(1:Pn) ) - &
                       SUM( ExtTemperature(1:En) * EdgeBasis(1:En) ) )

!          black body radiation...:
!          -------------------------
           Residual = Residual + &
                Emissivity * StefanBoltzmann * &
                     ( SUM( Temperature(1:Pn) * Basis(1:Pn) ) ** 4 - &
                       SUM( ExtTemperature(1:En) * EdgeBasis(1:En) ) ** 4 )

!          flux given by the computed solution, and 
!          force norm for scaling the residual:
!          -----------------------------------------
           IF ( CurrentCoordinateSystem() == Cartesian ) THEN
              DO k=1,DIM
                 Residual = Residual + Conductivity  * &
                    SUM( dBasisdx(1:Pn,k) * Temperature(1:Pn) ) * Normal(k)

                 Gnorm = Gnorm + s * (Conductivity * &
                       SUM(dBasisdx(1:Pn,k) * Temperature(1:Pn)) * Normal(k))**2
              END DO
           ELSE
              DO k=1,DIM
                 DO l=1,DIM
                    Residual = Residual + Metric(k,l) * Conductivity  * &
                       SUM( dBasisdx(1:Pn,k) * Temperature(1:Pn) ) * Normal(l)

                    Gnorm = Gnorm + s * (Metric(k,l) * Conductivity * &
                      SUM(dBasisdx(1:Pn,k) * Temperature(1:Pn) ) * Normal(l))**2
                 END DO
              END DO
           END IF

           EdgeLength   = EdgeLength + s
           IF ( .NOT. Dirichlet ) THEN
              ResidualNorm = ResidualNorm + s * Residual ** 2
           END IF
        END DO
        EXIT
     END DO

     IF ( CoordinateSystemDimension() == 3 ) THEN
        EdgeLength = SQRT(EdgeLength)
     END IF

!    Gnorm = EdgeLength * Gnorm
     Indicator = EdgeLength * ResidualNorm

     DEALLOCATE( Nodes % x, Nodes % y, Nodes % z)
     DEALLOCATE( EdgeNodes % x, EdgeNodes % y, EdgeNodes % z)

     DEALLOCATE( Temperature, Basis, ExtTemperature, TransferCoeff,  &
        x, y, z, EdgeBasis, dBasisdx, NodalConductivity, Flux, &
        NodalEmissivity ) 
!------------------------------------------------------------------------------
  END FUNCTION BoundaryResidual
!------------------------------------------------------------------------------
END MODULE HeatBoundaryResidual
