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
MODULE HeatDiffuseGrayRadiation
  USE HeatIntegOverA
 CONTAINS
  SUBROUTINE DiffuseGrayRadiation( Model, Solver, Element, Emissivity, &
      Temperature, TempPerm, ForceVector, AngleFraction, Text, dt, ElementNodes, &
      TransientAssembly, NewtonLinearization, StefanBoltzmann)
!------------------------------------------------------------------------------
      USE MaterialModels
      USE Radiation
      USE DefUtils
      USE NumaAdaptive
      IMPLICIT NONE

      TYPE(Model_t)  :: Model
      TYPE(Solver_t) :: Solver
      TYPE(Element_t), POINTER :: Element, RadiationElement
      TYPE(Nodes_t) :: ElementNodes
      INTEGER :: TempPerm(:)
      LOGICAL :: NewtonLinearization, TransientAssembly
      REAL(KIND=dp) :: Temperature(:), ForceVector(:), Emissivity, AngleFraction, Text, dt
!------------------------------------------------------------------------------
      TYPE(Matrix_t), POINTER :: StiffMatrix
      REAL(KIND=dp) :: Area, Asum, S, StefanBoltzmann
      INTEGER :: j,k,l,m,ImplicitFactors, k1, k2, n
      INTEGER, POINTER :: ElementList(:)
      LOGICAL :: AllocationsDone
      REAL(KIND=dp), ALLOCATABLE :: HeatTransferCoeff(:), LOAD(:), STIFF(:,:), MASS(:,:), &
          FORCE(:)

      SAVE HeatTransferCoeff, LOAD, STIFF, MASS, FORCE, AllocationsDone
!------------------------------------------------------------------------------
!     If linear iteration compute radiation load
!------------------------------------------------------------------------------

      n = GetElementNOFNodes()
      StiffMatrix => GetMatrix()

      Asum = 0.0d0
      IF ( .NOT. NewtonLinearization ) THEN
        Text = ComputeRadiationLoad( Model, Solver % Mesh, Element, &
                 Temperature, TempPerm, Emissivity, AngleFraction)
      ELSE   !  Full Newton-Raphson solver
!------------------------------------------------------------------------------
!       Go trough surfaces (j) this surface (i) is getting
!       radiated from.
!------------------------------------------------------------------------------

        Area  = ElementArea( Solver % Mesh, Element, n )
        ElementList => Element % BoundaryInfo % GebhardtFactors % Elements

        DO j=1,Element % BoundaryInfo % GebhardtFactors % NumberOfFactors

          RadiationElement => Solver % Mesh % Elements( ElementList(j) )

          Text = ComputeRadiationCoeff(Model,Solver % Mesh,Element,j) / ( Area )
          Asum = Asum + Text
!------------------------------------------------------------------------------
!         Gebhardt factors are given elementwise at the center
!         of the element, so take avarage of nodal temperatures
!         (or integrate over surface j)
!------------------------------------------------------------------------------

          k = RadiationElement % TYPE % NumberOfNodes
          ImplicitFactors = Element % BoundaryInfo % GebhardtFactors % NumberOfImplicitFactors
          IF(ImplicitFactors == 0) &
              ImplicitFactors = Element % BoundaryInfo % GebhardtFactors % NumberOfFactors

          IF(j <= ImplicitFactors) THEN
            
            S = (SUM( Temperature( TempPerm( RadiationElement % &
                NodeIndexes))**4 )/k )**(1.0d0/4.0d0)
!------------------------------------------------------------------------------
!         Linearization of the G_jiT^4_j term
!------------------------------------------------------------------------------
            HeatTransferCoeff(1:n) = -4 * Text * S**3 * StefanBoltzmann
            LOAD(1:n) = -3 * Text * S**4 * StefanBoltzmann
!------------------------------------------------------------------------------
!         Integrate the contribution of surface j over surface i
!         and add to global matrix
!------------------------------------------------------------------------------
            CALL IntegOverA( STIFF, FORCE, LOAD, &
                HeatTransferCoeff, Element, n, k, ElementNodes ) 
            
            IF ( TransientAssembly ) THEN
              MASS = 0.d0
              CALL Add1stOrderTime( MASS, STIFF, &
                  FORCE,dt,n,1,TempPerm(Element % NodeIndexes),Solver )
            END IF
            
            DO m=1,n
              k1 = TempPerm( Element % NodeIndexes(m) )
              DO l=1,k
                k2 = TempPerm( RadiationElement % NodeIndexes(l) )
                CALL AddToMatrixElement( StiffMatrix,k1, &
                    k2,STIFF(m,l) )
              END DO
              ForceVector(k1) = ForceVector(k1) + FORCE(m)
            END DO

          ELSE

            S = (SUM( Temperature( TempPerm( RadiationElement % &
                NodeIndexes))**4 )/k )
            
            HeatTransferCoeff(1:n) = 0.0d0
            LOAD(1:n) = Text * S * StefanBoltzmann
            
            CALL IntegOverA( STIFF, FORCE, LOAD, &
                HeatTransferCoeff, Element, n, k, ElementNodes ) 
            
            DO m=1,n
              k1 = TempPerm( Element % NodeIndexes(m) )
              ForceVector(k1) = ForceVector(k1) + FORCE(m)
            END DO
            
          END IF 

        END DO

!------------------------------------------------------------------------------
!       We have already added all external temperature contributions
!       to the matrix for the Newton type iteration
!------------------------------------------------------------------------------
        AngleFraction = Asum / Emissivity
        Text = 0.0

      END IF  !  of newton-raphson

  CONTAINS
      SUBROUTINE Allocations
          INTEGER :: N, istat

          N = Solver % Mesh % MaxElementDOFs

          IF (AllocationsDone) THEN
              DEALLOCATE( &
                  LOAD, &
                  STIFF, &
                  MASS, &
                  FORCE &
              )
          END IF

          ALLOCATE( &
              LOAD(N), &
              FORCE(2*N), &
              STIFF(2*N, 2*N), &
              MASS(2*N, 2*N), &
              STAT=istat &
          )

          IF ( istat /= 0 ) THEN
            CALL Fatal( 'HeatSolve', 'Memory allocation error' )
          END IF

          AllocationsDone = .TRUE.
      END SUBROUTINE Allocations
  END SUBROUTINE DiffuseGrayRadiation
END MODULE HeatDiffuseGrayRadiation
!------------------------------------------------------------------------------
