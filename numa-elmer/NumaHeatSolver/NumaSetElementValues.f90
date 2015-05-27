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
    SUBROUTINE NumaSetElementValues (A,b,Name,DOF,NDOFs,ValueList,n,NodeIndexes,Perm)
!------------------------------------------------------------------------------ 
!******************************************************************************
!
! Set values related to a specific boundary or bulk element
!
! ARGUMENTS:
!
! TYPE(Matrix_t), POINTER :: A
!   INOUT: The global stiff matrix
!
! REAL(KIND=dp) :: b
!   INOUT: The global RHS vector
! 
! CHARACTER(LEN=*) :: Name
!   INPUT: name of the dof to be set
!
! INTEGER :: DOF, NDOFs
!   INPUT: The order number of the dof and the total number of DOFs for
!          this equation
!
! TYPE(ValueList_t), POINTER :: ValueList
! INPUT: Values to be set as Dirichlet BC
!
! INTEGER :: n
!   INPUT: Number of entries to be modified
!
! INTEGER :: NodeIndexes(:)
!   INPUT: List indexes of nodes modified
!
! INTEGER :: Perm(:)
!   INPUT: The node reordering info, this has been generated at the
!          beginning of the simulation for bandwidth optimization
!******************************************************************************   
        USE MaterialModels
        USE DefUtils
        USE NumaAdaptive
        IMPLICIT NONE

        TYPE(Matrix_t), POINTER :: A
        REAL(KIND=dp) :: b(:)
        CHARACTER(LEN=*) :: Name 
        INTEGER :: n,DOF,NDOFs, Perm(:),NodeIndexes(:)
        TYPE(ValueList_t), POINTER :: ValueList
!------------------------------------------------------------------------------ 
!       Local variables
!------------------------------------------------------------------------------         
        REAL(KIND=dp) :: Work(n)        
        INTEGER :: j,k,k1,l
        REAL(KIND=dp), POINTER :: WorkA(:,:,:) => NULL()
        LOGICAL :: GotIt
!------------------------------------------------------------------------------
!       Get the nodes indexes in Work or WorkA in function of DOF order
!------------------------------------------------------------------------------
        IF ( DOF > 0 ) THEN
            Work(1:n)  = ListGetReal( ValueList, Name, n, NodeIndexes, gotIt )
        ELSE
            CALL ListGetRealArray( ValueList, Name, WorkA, n, NodeIndexes, gotIt )
        END IF
!------------------------------------------------------------------------------ 
        IF ( gotIt ) THEN
!------------------------------------------------------------------------------
!           Go through the nodes indexes in Work or WorkA 
!------------------------------------------------------------------------------
            DO j=1,n
!------------------------------------------------------------------------------
!               Check if the index is valid
!------------------------------------------------------------------------------
                IF ( NodeIndexes(j) > SIZE(Perm) .OR. NodeIndexes(j) < 1 ) THEN
                    CALL Warn('NumaSetDirichletBoundaries','Invalid Node Number')
                    CYCLE
                END IF
!------------------------------------------------------------------------------
!               Check if the index is valid
!------------------------------------------------------------------------------
                k = Perm(NodeIndexes(j))
                IF ( k > 0 ) THEN
!------------------------------------------------------------------------------ 
                    IF ( DOF>0 ) THEN
!------------------------------------------------------------------------------ 
                        k = NDOFs * (k-1) + DOF
!------------------------------------------------------------------------------
!                       Band matrix
!------------------------------------------------------------------------------
                        IF ( A % FORMAT == MATRIX_SBAND ) THEN
                            CALL SBand_SetDirichlet( A,b,k,Work(j) )
!------------------------------------------------------------------------------
!                       CRS & symmetric matrix
!------------------------------------------------------------------------------
                        ELSE IF ( A % FORMAT == MATRIX_CRS .AND. A % Symmetric ) THEN
                            CALL CRS_SetSymmDirichlet( A,b,k,Work(j) )
!------------------------------------------------------------------------------
!                       General case 
!------------------------------------------------------------------------------             
                        ELSE
                            b(k) = Work(j)
                            CALL ZeroRow( A,k )
                            CALL SetMatrixElement( A,k,k,1.0d0 )
                        END IF
!------------------------------------------------------------------------------ 
                    ELSE
!------------------------------------------------------------------------------ 
                        DO l=1,MIN( NDOFs, SIZE(Worka,1) )
!------------------------------------------------------------------------------ 
                            k1 = NDOFs * (k-1) + l
!------------------------------------------------------------------------------
!                           Band matrix
!------------------------------------------------------------------------------
                            IF ( A % FORMAT == MATRIX_SBAND ) THEN
                                CALL SBand_SetDirichlet( A,b,k1,WorkA(l,1,j) )
!------------------------------------------------------------------------------
!                           CRS & symmetric matrix
!------------------------------------------------------------------------------
                            ELSE IF ( A % FORMAT == MATRIX_CRS .AND. A % Symmetric ) THEN
                                CALL CRS_SetSymmDirichlet( A,b,k1,WorkA(l,1,j) )
!------------------------------------------------------------------------------
!                           General case 
!------------------------------------------------------------------------------             
                            ELSE
                                b(k1) = WorkA(l,1,j)
                                CALL ZeroRow( A,k1 )
                                CALL SetMatrixElement( A,k1,k1,1.0d0 )
                            END IF
!------------------------------------------------------------------------------
                        END DO ! l
!------------------------------------------------------------------------------
                    END IF ! DOF>0
!------------------------------------------------------------------------------ 
                END IF !k>0
!------------------------------------------------------------------------------ 
            END DO ! j
!------------------------------------------------------------------------------ 
        END IF ! gotit
!------------------------------------------------------------------------------  
    END SUBROUTINE NumaSetElementValues
!------------------------------------------------------------------------------  
