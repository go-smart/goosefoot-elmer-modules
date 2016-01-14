! /**
!  * This file is part of the Go-Smart Simulation Architecture (GSSA).
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
!  */
! 
SUBROUTINE AlternatingBCSolver( Model,Solver,Timestep,TransientSimulation)
      USE DefUtils

      IMPLICIT None

      TYPE(Model_t) :: Model
      TYPE(Solver_t) :: Solver
      REAL(KIND=dp) :: TimeStep,tt!anode,
      REAL(KIND=dp), POINTER :: anode(:)
      LOGICAL :: TransientSimulation,FLAG

      TYPE(Element_t), POINTER :: BoundaryElement
      TYPE(ValueList_t), POINTER :: BC, Simulation
      INTEGER :: i, j, k,ks,maxN, time, tag, partn, bndry, left, right, &
          nodes(MAX_ELEMENT_NODES), eio_info, TYPE,Panch
      REAL(KIND=dp), POINTER :: coord(:,:)
!      REAL*8, POINTER :: anode(:), cathode(:)
      INTEGER, POINTER :: Plist(:)
      LOGICAL :: AllocationsDone = .FALSE., Found, AlternatingBoundary
      LOGICAL, POINTER :: anodeLogic(:)
      IF (.NOT. AllocationsDone) THEN
!          maxN = Model % Solver % Mesh % MaxElementNodes
          ALLOCATE(Plist(10))
!          

          AllocationsDone = .TRUE.
      END IF

      Simulation => GetSimulation()

!      WRITE(*,*) "Number of BCs",Model % NumberOfBCs,Solver % Mesh % NumberOfBoundaryElements
!      READ(*,*) Panch      
!      DO j = 1,Solver % Mesh % NumberOfBulkElements + 1, &
!                         Solver % Mesh % NumberOfBulkElements + &
!                         Solver % Mesh % NumberOfBoundaryElements
!        IF (Solver % Mesh % Elements(j) % BodyId /=1) THEN
!!          WRITE(*,*) Solver % Mesh % Elements(j) % BodyId
!        END IF
!      END DO
!      READ(*,*) Panch 
       DO j = 1, Model % NumberOfBCs
        tt=GetConstReal(Model % BCs(j) % Values, 'BoundaryCheck', Found)
!         WRITE(*,*) "anode ",j,anode         
         IF (FOUND) THEN
          !Plist=>ListGetIntegerArray(Model % BCs(j)%Values,"Target Boundaries")     
          Plist=>ListGetIntegerArray(Model % BCs(j)%Values,"Body Id")             
          DO i = Solver % Mesh % NumberOfBulkElements + 1, &
                         Solver % Mesh % NumberOfBulkElements + &
                         Solver % Mesh % NumberOfBoundaryElements

                    BoundaryElement => Solver % Mesh % Elements(i)
                    bndry = BoundaryElement % BodyId              
          k=SIZE(Plist)
!          WRITE(*,*) "i ",i         
          Do ks=1,k
!          WRITE(*,*) 'ks = Plist',ks, Plist(ks),bndry
            IF (Plist(ks) == bndry) THEN
                     anode => GetReal(Model % BCs(j) % Values, 'BoundaryCheck', Found)
                IF (anode(1)>0.0) THEN
                  BoundaryElement % BoundaryInfo % Constraint = j
!                  WRITE(*,*) 'Dirichlet Boundary'
!                  READ(*,*) Panch                  
                ELSE
                  BoundaryElement % BoundaryInfo % Constraint = 0
!                  WRITE(*,*) 'No Boundary'
!                  READ(*,*) Panch
                END IF
             END IF
            END DO
          END DO
         END IF
       END DO

          
!      time = Solver%Time
!      DO j = 1, Model % NumberOfBCs
!        anode => ListGetRealArray(Simulation, 'DirichletSwitch', Found)
!        INNER: Do k=1,SIZE(anode)
!          IF (anode(k)>Simulation%Time)
!            anodeLogic => ListGetLogicalArray(Simulation, 'DirichletStatus', Found)            
!            FLAG=anodeLogic(k)
!            EXIT INNER
!          END IF
!          END DO INNER
!        IF ( Found ) THEN
!          DO i = Solver % Mesh % NumberOfBulkElements + 1, &
!                 Solver % Mesh % NumberOfBulkElements + &
!                 Solver % Mesh % NumberOfBoundaryElements

!            BoundaryElement => Solver % Mesh % Elements(i)
!            bndry = BoundaryElement % BodyId

!            IF (FLAG) THEN
!                BoundaryElement % BoundaryInfo % Constraint = j
!            ELSE
!                BoundaryElement % BoundaryInfo % Constraint = 0
!            END IF
!          END DO
!        END IF
!      END DO

END SUBROUTINE AlternatingBCSolver
