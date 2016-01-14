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
MODULE HeatAddHeatFluxBC
    USE DiffuseConvective
    USE DiffuseConvectiveGeneral
    USE HeatAddHeatGap
    USE HeatFindGapIndexes
    USE HeatDiffuseGrayRadiation
 CONTAINS
   SUBROUTINE AddHeatFluxBC(Model, Solver, Element, ElementNodes, ForceVector, ConstantBulk, &
           NewtonLinearization, HeatGapBC, n, IsRadiation, TempSol, TransientAssembly, dt)
      USE DefUtils
      USE MaterialModels
      USE HeatPhaseDefs
      IMPLICIT NONE

      TYPE(Model_t) :: Model
      TYPE(Solver_t) :: Solver
      TYPE(Nodes_t) :: ElementNodes
      TYPE(Element_t), POINTER :: Element
      TYPE(Variable_t) :: TempSol
      REAL(KIND=dp), POINTER :: ForceVector(:)
      REAL(KIND=dp) :: dt
      LOGICAL :: ConstantBulk, HeatGapBC, IsRadiation, NewtonLinearization, TransientAssembly
      INTEGER :: n,Panch

      CHARACTER(LEN=MAX_NAME_LEN) :: RadiationFlag, VarName
      TYPE(Element_t), POINTER :: Parent
      INTEGER :: i, j, k
      LOGICAL :: InfBC, Found, AllocationsDone = .FALSE.
      REAL(KIND=dp), ALLOCATABLE :: AText(:), HeatTransferCoeff(:), &
          PhaseVelocity(:,:), MASS(:,:), STIFF(:,:), FORCE(:), LOAD(:), &
          Density(:), LatentHeat(:), Work(:), HeatConductivityIso(:)
      REAL(KIND=dp), POINTER :: ParPtr(:), &
          NodalEmissivity(:), Temperature(:)
      REAL(KIND=dp) :: Normal(3), Emissivity, StefanBoltzmann, Text, VisibleFraction
      INTEGER, POINTER :: TempPerm(:)
      TYPE(ValueList_t), POINTER :: BC

      SAVE AText, HeatTransferCoeff, PhaseVelocity, Work, &
          LOAD, HeatConductivityIso, AllocationsDone, FORCE, STIFF, &
          MASS

      CALL GetElementNodes(ElementNodes)

      IF ( .NOT. AllocationsDone .OR. Solver % Mesh % Changed ) THEN
          CALL Allocations()
      END IF

      VarName = GetVarName( TempSol ) 
      TempPerm    => TempSol % Perm
      Temperature => TempSol % Values

!------------------------------------------------------------------------------
      BC => GetBC() ! We know this is available because this sub is guarded in AssemblyNeumann
      HeatTransferCoeff = 0.0D0
      LOAD  = 0.0D0
!------------------------------------------------------------------------------
!     BC: -k@T/@n = \epsilon\sigma(T^4 - Text^4)
!------------------------------------------------------------------------------
      IF( IsRadiation ) THEN
        StefanBoltzmann = ListGetConstReal( Model % Constants, &
                      'Stefan Boltzmann' )
      END IF

      RadiationFlag = GetString( BC, 'Radiation', Found )

      IF ( Found .AND. RadiationFlag(1:4) /= 'none' ) THEN

        NodalEmissivity => GetReal(BC, 'Emissivity', Found)
        IF(.NOT. Found) THEN
           NodalEmissivity => GetParentMatProp( 'Emissivity' )
        END IF
        Emissivity = SUM( NodalEmissivity(1:n) ) / n

!------------------------------------------------------------------------------
        IF (  RadiationFlag(1:9) == 'idealized' ) THEN
          ParPtr => GetReal( BC, 'Radiation External Temperature',Found )
          IF(.NOT. Found) ParPtr => GetReal( BC, 'External Temperature' )
          AText = ParPtr
        ELSE
          CALL DiffuseGrayRadiation( Model, Solver, Element, Emissivity, &
              Temperature, TempPerm, ForceVector, VisibleFraction, Text, dt, ElementNodes, &
              TransientAssembly, NewtonLinearization, StefanBoltzmann)

          IF( GetLogical( BC, 'Radiation Boundary Open', Found) ) THEN
            ParPtr => GetReal( BC, 'Radiation External Temperature',Found )
            IF(.NOT. Found) ParPtr => GetReal( BC, 'External Temperature' )
            AText = ParPtr
            IF( VisibleFraction >= 1.0_dp ) THEN
              Atext(1:n) = Text
            ELSE
              Atext(1:n) = ( (1 - VisibleFraction) * Atext(1:n)**4 + &
                  VisibleFraction * Text**4 ) ** 0.25_dp
            END IF
          ELSE
            AText(1:n) = Text
          END IF
        END IF
!------------------------------------------------------------------------------
!       Add our own contribution to surface temperature (and external
!       if using linear type iteration or idealized radiation)
!------------------------------------------------------------------------------
        DO j=1,n
          k = TempPerm(Element % NodeIndexes(j))
          Text = AText(j)

          IF ( .NOT. HeatGapBC .AND. NewtonLinearization ) THEN
             HeatTransferCoeff(j) = Emissivity * 4*Temperature(k)**3 * &
                               StefanBoltzmann
             LOAD(j) = Emissivity*(3*Temperature(k)**4+Text**4) * &
                               StefanBoltzmann
          ELSE
             HeatTransferCoeff(j) = Emissivity * (Temperature(k)**3 + &
             Temperature(k)**2*Text+Temperature(k)*Text**2 + Text**3) * &
                               StefanBoltzmann 
             LOAD(j) = HeatTransferCoeff(j) * Text
          END IF
        END DO
      END IF  ! of radition
!------------------------------------------------------------------------------

      ParPtr => GetReal( BC, 'Heat Transfer Coefficient',Found )
      Work = ParPtr
      IF ( Found ) THEN
       ParPtr => GetReal( BC, 'External Temperature',Found )
       AText = ParPtr
!!       WRITE(*,*) 'HeatTransferCoeff',HeatTransferCoeff
!!       WRITE(*,*) 'Value for Panch',Work,AText
!!       READ(*,*) Panch

       DO j=1,n
!------------------------------------------------------------------------------
!         BC: -k@T/@n = \alpha(T - Text)
!------------------------------------------------------------------------------
          k = TempPerm(Element % NodeIndexes(j))
          LOAD(j) = LOAD(j) + Work(j) * AText(j)
          HeatTransferCoeff(j) = HeatTransferCoeff(j) + Work(j)
        END DO
      END IF
!------------------------------------------------------------------------------
!     BC: -k@T/@n = (rho*L)*v.n 
!     Heating related to pulling is possible only in ss cases where pull velocity
!     is desrcibed.
!------------------------------------------------------------------------------

      IF( GetLogical( BC, 'Phase Change',Found ) ) THEN
         ParPtr => GetReal( BC,'Phase Velocity 1', Found  )
         PhaseVelocity(1,1:n) = ParPtr
         ParPtr => GetReal( BC,'Phase Velocity 2', Found  )
         PhaseVelocity(2,1:n) = ParPtr
         ParPtr => GetReal( BC,'Phase Velocity 3', Found  )
         PhaseVelocity(3,1:n) = ParPtr
  
         ! Ensure that the latent heat and density come from the same side
         ParPtr => GetParentMatProp( 'Latent Heat', &
              UElement = Element, UParent = Parent )
         LatentHeat = ParPtr
         IF(.NOT. ASSOCIATED(Parent) ) THEN
           CALL Warn('HeatSolve','Parent not associated')
         ELSE
           k = GetInteger(Model % Bodies(Parent % BodyId) % Values,'Material')
           ParPtr => GetReal( Model % Materials(k) % Values, 'Density' )
           Density = ParPtr
         END IF

         ! This could be rather put as a new type of BC into the assembly routine and 
         ! then the Normal could be taken at the proper Gaussian integration points. 
         Normal = NormalVector( Element, ElementNodes, 0.0_dp, 0.0_dp, .TRUE. )

         DO i=1,n
            LOAD(i) = LOAD(i) + &
                 LatentHeat(i) * Density(i) * SUM( Normal(1:3) * PhaseVelocity(1:3,i))
         END DO
      END IF

!------------------------------------------------------------------------------
!     BC: -k@T/@n = g
!------------------------------------------------------------------------------
      LOAD(1:n) = LOAD(1:n) +  GetReal( BC, 'Heat Flux', Found )
!      WRITE(*,*) 'Value for LOAD',LOAD
      HeatConductivityIso = 0.0_dp
      InfBC = ListGetLogical( BC,'Infinity BC '//TRIM(VarName),Found)
      IF( InfBC ) THEN
        ParPtr => GetReal( BC,'Infinity BC '//TRIM(VarName)//' Offset',Found)
        AText = ParPtr
        ! currently only isotropic heat conductivity supported
        ParPtr => GetParentMatProp('Heat Conductivity',Element,Found)
        HeatConductivityIso = ParPtr
!        WRITE(*,*) 'Panch',HeatConductivityIso
        IF(.NOT. Found) THEN
          CALL Fatal( 'HeatSolver','Could not find > Heat Conductivity < for parent!' )           
        END IF
      END IF

!------------------------------------------------------------------------------
!     Get element matrix and rhs due to boundary conditions ...
!------------------------------------------------------------------------------
      IF ( CurrentCoordinateSystem() == Cartesian ) THEN
!!        WRITE(*,*) 'Panch',HeatConductivityIso
        CALL DiffuseConvectiveBoundary( STIFF,FORCE, &
            LOAD,HeatTransferCoeff,InfBC,HeatConductivityIso,AText(1:n),&
            Element,n,ElementNodes )
      ELSE
        IF( InfBC ) THEN
          CALL Fatal('HeatSolver','Infinity BC not implemented only for cartersian case!')
        END IF
        CALL DiffuseConvectiveGenBoundary(STIFF,FORCE,&
            LOAD,HeatTransferCoeff,Element,n,ElementNodes ) 
      END IF

!------------------------------------------------------------------------------
!     Update global matrices from local matrices
!------------------------------------------------------------------------------
      IF ( TransientAssembly .AND. .NOT. ConstantBulk ) THEN
        MASS = 0.d0
        CALL Default1stOrderTime( MASS, STIFF, FORCE )
      END IF

      IF ( HeatGapBC ) &
        CALL AddHeatGap( Solver, Element, ElementNodes, STIFF, TempSol, n)

      CALL DefaultUpdateEquations( STIFF, FORCE )
!------------------------------------------------------------------------------
   CONTAINS
      SUBROUTINE Allocations
          INTEGER :: N, istat

          N = Solver % Mesh % MaxElementDOFs

          IF (AllocationsDone) THEN
              DEALLOCATE( &
                  NodalEmissivity, &
                  AText, &
                  HeatTransferCoeff, &
                  PhaseVelocity, &
                  LatentHeat, &
                  Density, &
                  Work, &
                  HeatConductivityIso, &
                  LOAD, &
                  MASS, &
                  STIFF, &
                  FORCE &
              )
          END IF

          ALLOCATE( &
              NodalEmissivity(N), &
              AText(N), &
              HeatTransferCoeff(N), &
              PhaseVelocity(3,N), &
              LatentHeat(N), &
              Density(N), &
              Work(N), &
              LOAD(N), &
              MASS(2*N, 2*N), &
              STIFF(2*N, 2*N), &
              FORCE(2*N), &
              HeatConductivityIso(N), &
              STAT=istat &
          )

          IF ( istat /= 0 ) THEN
            CALL Fatal( 'HeatSolve', 'Memory allocation error' )
          END IF

          AllocationsDone = .TRUE.
      END SUBROUTINE Allocations
  END SUBROUTINE AddHeatFluxBC
END MODULE
