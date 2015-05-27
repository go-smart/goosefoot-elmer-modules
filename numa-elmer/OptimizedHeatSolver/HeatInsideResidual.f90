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
MODULE HeatInsideResidual
  CONTAINS
!------------------------------------------------------------------------------
   FUNCTION InsideResidual( Model, Element, Mesh, &
        Quant, Perm, Fnorm ) RESULT( Indicator )
!------------------------------------------------------------------------------
     USE DefUtils
!------------------------------------------------------------------------------
     IMPLICIT NONE
!------------------------------------------------------------------------------
     TYPE(Model_t) :: Model
     INTEGER :: Perm(:)
     REAL(KIND=dp) :: Quant(:), Indicator(2), Fnorm
     TYPE( Mesh_t ), POINTER    :: Mesh
     TYPE( Element_t ), POINTER :: Element
!------------------------------------------------------------------------------

     TYPE(Nodes_t) :: Nodes

     INTEGER :: i,j,k,l,n,t,DIM

     LOGICAL :: stat, Found, Compressible
     TYPE( Variable_t ), POINTER :: Var

     REAL(KIND=dp), POINTER :: Hwrk(:,:,:)

     REAL(KIND=dp) :: SqrtMetric, Metric(3,3), Symb(3,3,3), dSymb(3,3,3,3)

     REAL(KIND=dp), ALLOCATABLE :: NodalDensity(:)
     REAL(KIND=dp), ALLOCATABLE :: NodalCapacity(:)
     REAL(KIND=dp), ALLOCATABLE :: NodalConductivity(:)
     REAL(KIND=dp), ALLOCATABLE :: Velo(:,:), Pressure(:)
     REAL(KIND=dp), ALLOCATABLE :: NodalSource(:), Temperature(:), PrevTemp(:)
     REAL(KIND=dp), ALLOCATABLE :: Basis(:), dBasisdx(:,:), ddBasisddx(:,:,:)

     REAL(KIND=dp) :: u, v, w, s, detJ, Density, Capacity

     REAL(KIND=dp) :: SpecificHeatRatio, ReferencePressure, dt
     REAL(KIND=dp) :: Residual, ResidualNorm, Area, Conductivity

     TYPE( ValueList_t ), POINTER :: Material

     TYPE(GaussIntegrationPoints_t), TARGET :: IntegStuff

     LOGICAL :: First = .TRUE.
     SAVE Hwrk, First
!------------------------------------------------------------------------------

!    Initialize:
!    -----------
     Indicator = 0.0d0
     Fnorm     = 0.0d0
!
!    Check if this eq. computed in this element:
!    -------------------------------------------
     IF ( ANY( Perm( Element % NodeIndexes ) <= 0 ) ) RETURN

     IF ( First ) THEN
        First = .FALSE.
        NULLIFY( Hwrk )
     END IF

     Metric = 0.0d0
     DO i=1,3
        Metric(i,i) = 1.0d0
     END DO

     SELECT CASE( CurrentCoordinateSystem() )
        CASE( AxisSymmetric, CylindricSymmetric )
           DIM = 3
        CASE DEFAULT
           DIM = CoordinateSystemDimension()
     END SELECT
!
!    Element nodal points:
!    ---------------------
     n = Element % TYPE % NumberOfNodes

     ALLOCATE( Nodes % x(n), Nodes % y(n), Nodes % z(n) )

     Nodes % x = Mesh % Nodes % x(Element % NodeIndexes)
     Nodes % y = Mesh % Nodes % y(Element % NodeIndexes)
     Nodes % z = Mesh % Nodes % z(Element % NodeIndexes)

     ALLOCATE( NodalDensity(n), NodalCapacity(n), NodalConductivity(n),       &
         Velo(3,n), Pressure(n), NodalSource(n), Temperature(n), PrevTemp(n), &
         Basis(n), dBasisdx(n,3), ddBasisddx(n,3,3) )
!
!    Elementwise nodal solution:
!    ---------------------------
     Temperature(1:n) = Quant( Perm(Element % NodeIndexes) )
!
!    Check for time dep.
!    -------------------
     PrevTemp(1:n) = Temperature(1:n)
     dt = Model % Solver % dt
     IF ( ListGetString( Model % Simulation, 'Simulation Type') == 'transient' ) THEN
        Var => VariableGet( Model % Variables, 'Temperature', .TRUE. )
        PrevTemp(1:n) = Var % PrevValues(Var % Perm(Element % NodeIndexes),1)
     END IF
!
!    Material parameters: conductivity, heat capacity and density
!    -------------------------------------------------------------
     k = ListGetInteger( Model % Bodies(Element % BodyId) % Values, 'Material', &
                     minv=1, maxv=Model % NumberOfMaterials )

     Material => Model % Materials(k) % Values

     CALL ListGetRealArray( Material, &
                  'Heat Conductivity', Hwrk,n, Element % NodeIndexes )

     NodalConductivity( 1:n ) = Hwrk( 1,1,1:n )

     NodalDensity(1:n) = ListGetReal( Material, &
            'Density', n, Element % NodeIndexes, Found )

     NodalCapacity(1:n) = ListGetReal( Material, &
          'Heat Capacity', n, Element % NodeIndexes, Found )
!
!    Check for compressible flow equations:
!    --------------------------------------
     Compressible = .FALSE.

     IF (  ListGetString( Material, 'Compressibility Model', Found ) == &
                 'perfect gas equation 1' ) THEN

        Compressible = .TRUE.

        Pressure = 0.0d0
        Var => VariableGet( Mesh % Variables, 'Pressure', .TRUE. )
        IF ( ASSOCIATED( Var ) ) THEN
           Pressure(1:n) = &
               Var % Values( Var % Perm(Element % NodeIndexes) )
        END IF

        ReferencePressure = ListGetConstReal( Material, &
                   'Reference Pressure' )

        SpecificHeatRatio = ListGetConstReal( Material, &
                   'Specific Heat Ratio' )

        NodalDensity(1:n) =  (Pressure(1:n) + ReferencePressure) * SpecificHeatRatio / &
              ( (SpecificHeatRatio - 1) * NodalCapacity(1:n) * Temperature(1:n) )
     END IF
!
!    Get (possible) convection velocity at the nodes of the element:
!    ----------------------------------------------------------------
     k = ListGetInteger( Model % Bodies(Element % BodyId) % Values, 'Equation', &
                minv=1, maxv=Model % NumberOFEquations )

     Velo = 0.0d0
     SELECT CASE( ListGetString( Model % Equations(k) % Values, &
                         'Convection', Found ) )

        !-----------------
        CASE( 'constant' )
        !-----------------

           Velo(1,1:n) = ListGetReal( Material, &
              'Convection Velocity 1', n, Element % NodeIndexes, Found )

           Velo(2,1:n) = ListGetReal( Material, &
              'Convection Velocity 2', n, Element % NodeIndexes, Found )

           Velo(3,1:n) = ListGetReal( Material, &
              'Convection Velocity 3', n, Element % NodeIndexes, Found )

        !-----------------
        CASE( 'computed' )
        !-----------------

           Var => VariableGet( Mesh % Variables, 'Velocity 1', .TRUE. )
           IF ( ASSOCIATED( Var ) ) THEN
              IF ( ALL( Var % Perm( Element % NodeIndexes ) > 0 ) ) THEN
                 Velo(1,1:n) = Var % Values(Var % Perm(Element % NodeIndexes))
   
                 Var => VariableGet( Mesh % Variables, 'Velocity 2', .TRUE. )
                 IF ( ASSOCIATED( Var ) ) &
                    Velo(2,1:n) = Var % Values( &
                              Var % Perm(Element % NodeIndexes ) )
   
                 Var => VariableGet( Mesh % Variables, 'Velocity 3', .TRUE. )
                 IF ( ASSOCIATED( Var ) ) &
                    Velo(3,1:n) = Var % Values( &
                             Var % Perm( Element % NodeIndexes ) )
              END IF
           END IF

     END SELECT

!
!    Heat source:
!    ------------
!
     k = ListGetInteger( &
         Model % Bodies(Element % BodyId) % Values,'Body Force',Found, &
                 1, Model % NumberOFBodyForces)

     NodalSource = 0.0d0
     IF ( Found .AND. k > 0  ) THEN
        NodalSource(1:n) = ListGetReal( Model % BodyForces(k) % Values, &
               'Heat Source', n, Element % NodeIndexes, Found )
     END IF

!
!    Integrate square of residual over element:
!    ------------------------------------------

     ResidualNorm = 0.0d0
     Area = 0.0d0

     IntegStuff = GaussPoints( Element )

     DO t=1,IntegStuff % n
        u = IntegStuff % u(t)
        v = IntegStuff % v(t)
        w = IntegStuff % w(t)

        stat = ElementInfo( Element, Nodes, u, v, w, detJ, &
            Basis, dBasisdx, ddBasisddx, .TRUE., .FALSE. )

        IF ( CurrentCoordinateSystem() == Cartesian ) THEN
           s = IntegStuff % s(t) * detJ
        ELSE
           u = SUM( Basis(1:n) * Nodes % x(1:n) )
           v = SUM( Basis(1:n) * Nodes % y(1:n) )
           w = SUM( Basis(1:n) * Nodes % z(1:n) )

           CALL CoordinateSystemInfo( Metric, SqrtMetric, &
                       Symb, dSymb, u, v, w )
           s = IntegStuff % s(t) * detJ * SqrtMetric
        END IF

        Capacity     = SUM( NodalCapacity(1:n) * Basis(1:n) )
        Density      = SUM( NodalDensity(1:n) * Basis(1:n) )
        Conductivity = SUM( NodalConductivity(1:n) * Basis(1:n) )
!
!       Residual of the convection-diffusion (heat) equation:
!        R = \rho * c_p * (@T/@t + u.grad(T)) - &
!            div(C grad(T)) + p div(u) - h,
!       ---------------------------------------------------
!
!       or more generally:
!
!        R = \rho * c_p * (@T/@t + u^j T_{,j}) - &
!          g^{jk} (C T_{,j}}_{,k} + p div(u) - h
!       ---------------------------------------------------
!
        Residual = -Density * SUM( NodalSource(1:n) * Basis(1:n) )

        IF ( CurrentCoordinateSystem() == Cartesian ) THEN
           DO j=1,DIM
!
!             - grad(C).grad(T):
!             --------------------
!
              Residual = Residual - &
                 SUM( Temperature(1:n) * dBasisdx(1:n,j) ) * &
                 SUM( NodalConductivity(1:n) * dBasisdx(1:n,j) )

!
!             - C div(grad(T)):
!             -------------------
!
              Residual = Residual - Conductivity * &
                 SUM( Temperature(1:n) * ddBasisddx(1:n,j,j) )
           END DO
        ELSE
           DO j=1,DIM
              DO k=1,DIM
!
!                - g^{jk} C_{,k}T_{j}:
!                ---------------------
!
                 Residual = Residual - Metric(j,k) * &
                    SUM( Temperature(1:n) * dBasisdx(1:n,j) ) * &
                    SUM( NodalConductivity(1:n) * dBasisdx(1:n,k) )

!
!                - g^{jk} C T_{,jk}:
!                -------------------
!
                 Residual = Residual - Metric(j,k) * Conductivity * &
                    SUM( Temperature(1:n) * ddBasisddx(1:n,j,k) )
!
!                + g^{jk} C {_jk^l} T_{,l}:
!                ---------------------------
                 DO l=1,DIM
                    Residual = Residual + Metric(j,k) * Conductivity * &
                      Symb(j,k,l) * SUM( Temperature(1:n) * dBasisdx(1:n,l) )
                 END DO
              END DO
           END DO
        END IF

!       + \rho * c_p * (@T/@t + u.grad(T)):
!       -----------------------------------
        Residual = Residual + Density * Capacity *  &
           SUM((Temperature(1:n)-PrevTemp(1:n))*Basis(1:n)) / dt

        DO j=1,DIM
           Residual = Residual + &
              Density * Capacity * SUM( Velo(j,1:n) * Basis(1:n) ) * &
                    SUM( Temperature(1:n) * dBasisdx(1:n,j) )
        END DO


        IF ( Compressible ) THEN
!
!          + p div(u) or p u^j_{,j}:
!          -------------------------
!
           DO j=1,DIM
              Residual = Residual + &
                 SUM( Pressure(1:n) * Basis(1:n) ) * &
                      SUM( Velo(j,1:n) * dBasisdx(1:n,j) )

              IF ( CurrentCoordinateSystem() /= Cartesian ) THEN
                 DO k=1,DIM
                    Residual = Residual + &
                       SUM( Pressure(1:n) * Basis(1:n) ) * &
                           Symb(j,k,j) * SUM( Velo(k,1:n) * Basis(1:n) )
                 END DO
              END IF
           END DO
        END IF

!
!       Compute also force norm for scaling the residual:
!       -------------------------------------------------
        DO i=1,DIM
           Fnorm = Fnorm + s * ( Density * &
             SUM( NodalSource(1:n) * Basis(1:n) ) ) ** 2
        END DO

        Area = Area + s
        ResidualNorm = ResidualNorm + s *  Residual ** 2
     END DO

!    Fnorm = Element % hk**2 * Fnorm
     Indicator = Element % hK**2 * ResidualNorm

     DEALLOCATE( NodalDensity, NodalCapacity, NodalConductivity,    &
         Velo, Pressure, NodalSource, Temperature, PrevTemp, Basis, &
         dBasisdx, ddBasisddx )
!------------------------------------------------------------------------------
  END FUNCTION InsideResidual
!------------------------------------------------------------------------------
END MODULE HeatInsideResidual
