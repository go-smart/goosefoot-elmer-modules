! *****************************************************************************/
! *  Subroutines for the resolution of heat transfer equation.
! *****************************************************************************/
! *  This file is based on the file "HeatSolve.f90" of Elmer (Finite Element 
! *  Software for Multiphysical Problems)
! *  Copyright 1st April 1995 - , CSC - Scientific Computing Ltd., Finland
! *****************************************************************************/
! *****************************************************************************/
! * Version 1 (30/07/09)
! * Main changes with Elmer file HeatSolve.f90  (28/07/09)
! *****************************************************************************/
! *  - Multi DOF solutions: each DOF corresponds to a material
! *  - Initial condition treatment modified to enforce the saving of all DOFs
! *  - Dirichlet conditions on interior nodes sets by specifying bounds coordinates  
! *  - Convective transfer between DOFs modeled
! *  - Volume fraction parameter attached to each material
! *  - Cells death modelled
! *  - No phase change, no radiation, no mesh refinement
! *****************************************************************************/
! *****************************************************************************/
! * List of subroutines:
! *****************************************************************************/
! *  - NumaHeatSolver
! *  - ErrorCompute
! *  - TemperatureBoundary
! *  - TemperatureCompose
! *  - AnalyticalSolution
! *  - GapTemperature
! *  - AddHeatGap
! *  - ComputeMeshSize
! *  - NumaSetInitialConditions
! *  - NumaDefaultDirichletConditions
! *  - NumaSetInteriorDirichletConditions
! *  - NumaSetPointValues
! *****************************************************************************/
! *****************************************************************************/
! *  08/08/09:
! *  - Modification of function NumaSetInteriorDirichletConditions to handle 
! *    spherical zones -> new keywords added in SOLVER.KEYWORDS
! *****************************************************************************/ 
! *****************************************************************************/
! *  13/08/09:
! *  - Variable "NonLinearProblem" (Logical) removed
! *    -> keyword removed in SOLVER.KEYWORDS
! *    -> to prevent from multi iterations in linear cases, set Tolerance = 3.0
! *       in input file
! *****************************************************************************/
! *****************************************************************************/
! *  17/08/09:
! *  - Modification of the name of the registration file for max temperature and   
!      iterations in order to run in parallel
! *****************************************************************************/ 
! *****************************************************************************/
! *  18/08/09:
! *  - Volume fraction added as a multiplying factor for conduction and 
!      convection
! *****************************************************************************/ 
! *****************************************************************************/
! *  25/08/09:
! *  - Tumour localization removed -> new solver for tumour
! *  - Alive state detection removed -> new solver for alive/died state
! *****************************************************************************/ 
! *****************************************************************************/
! *  26/08/09:
! *  - Functions for mesh refinement added (based on Elmer counterparts):
! *     - TemperatureBoundaryResidual
! *     - TemperatureEdgeResidual
! *     - TemperatureInsideResidual
! *  - Use of NumaAdaptive module added
! *  - Call of mesh refinement added at the end of the iteration loop
! *****************************************************************************/
! *****************************************************************************/
! *  01/10/09:
! *  - Modification comparison of relative change and non-linear convergence 
! *    tolerance for parallel execution
! *     -Exchange the convergence information with other processors
! *     -Continue the iterations unless all processors have converged
! *****************************************************************************/ 
! *****************************************************************************/
! *  01/10/09:
! *  - DeathConductivity removed
! *  - DeathCapacity added
! *****************************************************************************/ 
! *****************************************************************************/
! *  09/10/09:
! *  - Call to SetNodalLoads (SolverUtils.src) added
! *****************************************************************************/ 
! *****************************************************************************/
! *  16/10/09:
! *  - Computation of real time of assembly and solve
! *****************************************************************************/
! *****************************************************************************/
! *  05/11/09: 
! *  - Call to SetNodalLoads removed in main function
! *****************************************************************************/
! *****************************************************************************/
! *  06/11/09: 
! *  - All coefficients of heat equation divided by volumefraction, in order to 
! *    take into account the volume fraction coeff associated to nodal load
! *    without modifiying SetNodalLoads
! *****************************************************************************/
! *****************************************************************************/
! *  12/11/09: 
! *  - Electric power added as a coefficient of Load
! *****************************************************************************/
! *****************************************************************************/
! *  13/11/09: 
! *  - Call to SetNodalLoads removed
! *****************************************************************************/
! *****************************************************************************/
! *  18/01/10: 
! *  - Nonlinear Iterations: Control of cv modified if parallel execution
! *     -For each proc, check of local cv
! *     -Global cv = cv(proc1) AND ... AND cv(procN) (use of MPIAllReduce())
! *     -Stop only if global cv is reached 
! *****************************************************************************/
! *****************************************************************************/
! *  18/02/10: 
! *  - Use of 'CellState' variable instead of 'Alive'
! *****************************************************************************/
! *****************************************************************************/
! *  04/03/10: 
! *  - Modifications to use either model 1 or 2 for cells death
! *****************************************************************************/
! *****************************************************************************/
! *  18/03/10: 
! *  - Modifications to change Dirichlet BC value in function of two criterions, 
!   *  element by element:
! *         - distance to some location defined by coordinates
!   *           - diameter of the element
!   *  - New subroutines:
! *         - NumaSetElementValues(): based on SetElementValues() (SolverUtils.src)
! *     - NumaSetDirichletBoundaries(): based on SetDirichletBoundaries
! *         (SolverUtils.src), allows to control the new criterion, called in 
!   *           NumaDefaultDirichletConditions()
! *****************************************************************************/
! *****************************************************************************/
! *  08/04/10: 
! *  - Modifications to use line and point electric sources, defined by points +
!   *  interpolation:
! *         - Compute the geometry of the electric source after allocations:
! *         - Get the interpolation points and the length of the tips
! *         - Compute the distance of each node from the interpolated sources
! *         - Compute the (unit) corresponding power distribution
! *     - During assembly, use the power distribution multiplied by the power 
! *       over time (specified in input file) to get the source
! *         - Functions NumaDistanceToElectricTip() and NumaElectricPowerMollifier()
!   *       added
! *****************************************************************************/
! *****************************************************************************/
! *  09/04/10: 
! *  - Modifications to read cell state model in input file, in simulation
!   *  section:
! *****************************************************************************/
! *****************************************************************************/
! *  13/04/10: 
! *  - Modification of NumaDistanceToElectricTip() to get the orthogonal projected
!   *    point of the node onto the tip 
!   *  - Function NumaElectricPowerMollifier() deleted
! *  - Constant power distribution only
! *****************************************************************************/
! *****************************************************************************/
! *  20/05/10: 
! *  - Modification of main routine to catch End Of File exception when interpolated
! *    electric power is used
! *****************************************************************************/
! *****************************************************************************/
! *  24/05/10: 
! *  - Modification of main routine: read electric tips file name from keyword 
! *    "Electric Tips Filename Root" in input file 
! *  - Modification of main routine: if text files (for electric tips) not 
! *    found or name not specified, stop the simulation and print error messages
! *****************************************************************************/
! *****************************************************************************/
! *  24/05/10: 
! *  - Modifications to locally modify a Dirichlet BC into a flux or convective BC
!   *    -Main routine: read just after allocations the keywords in input file, then
! *     when Flux BC are applied, treat also the case of Dirichlet BC modified to 
! *     flux BC
! *    -NumaSetDirichletBoundaries(): check if the Dirichlet BC has to be modified 
! *      into an another Dirichlet BC
! *****************************************************************************/
! *****************************************************************************/
! *  04/06/10: 
! *  - New variable:
!   *    -Main routine: ElectricPowerCutOff (logical)
! *     value is read in input file and used to decide electric power cut off 
! *****************************************************************************/
! *****************************************************************************/
! *  07/06/10:
! *  - In NumaSetInteriorDirichletConditions(), modifications to apply Dirichlet 
! *    BC in several spherical zones (for center and radius, real are replaced by
! *    arrays of reals)
! *****************************************************************************/
! *****************************************************************************/
! *  09/06/10:
! *  - In NumaSetInteriorDirichletConditions(), corrections 
! *****************************************************************************/
! *****************************************************************************/
! *  10/06/10:
! *  - In main routine, add the heat source as a new variable for visualization 
! *****************************************************************************/
! *****************************************************************************/
! *  24/06/10:
! *  - In main routine, computation of REAL/CPU time of allocation 
! *****************************************************************************/
! *****************************************************************************/
! *  16/06/10:
! *  - In main routine, correction of convective boundary condition
! *****************************************************************************/
! *****************************************************************************/
! *  27/07/10:
! *  - In main routine, correction of index for Heat Source added as a variable
! *  - In main routine, initialization of power to 0 instead of 1
! *****************************************************************************/
! *****************************************************************************/
! *  28/07/10:
! *  - In main routine, convective BC fixed
! *****************************************************************************/
! *****************************************************************************/
! *  02/12/10:
! *  - In main routine, addition of the heat source as a new variable for visualization 
! *  pushed after resolution and done at each timestep for variable location case
! *****************************************************************************/
! *****************************************************************************/
! *  06/01/11:
! *  - Modifications to use Residual Free Bubbles stabilization:
! *  - In main routine, read variable UseBubbles in input file (default value is F) 
! *  - UseBubble added as parameter of routine TemperatureCompose()
! *  - In main routine, call to Condensate added before UpdateGlobalEquations()
! *  - In TemperatureCompose(), modif of NBasis, IntegStuff, and Stat if UseBubbles
! *****************************************************************************/
! *****************************************************************************/
! *  10/01/11:
! *  - Modifications of routine TemperatureCompose(): convective transfer removed from 
! *  stabilization
! *****************************************************************************/
! *****************************************************************************/
! *  20/01/11:
! *  - Implementation of tissue perfusion [Krueger et. al]:
! *     - New variables: PerfusionCoeff(:), BodyTemperature
! *     - In main routine read PerfusionCoeff and BodyTemperature in input file
! *     - Stop porous convection and convective transfer if perfusion
! *     - Vb=0,Vt=1 if perfusion
! *     - PerfusionCoeff=0 if dead cell
! *     - Complete Perfusion coefficient=PerfusionCoeff*rho(b)*ct(b)
! *     - PerfusionCoeff added as parameter of TemperatureCompose()
! *     - In TemperatureCompose(), modification of CO coeff
! *
! *  -New variable DeadThresold (default value=0.2) read in input file and 
! *  used in tests in place of 0.8
! *****************************************************************************/
! *****************************************************************************/
! *  21/01/11:
! *  - Reading of variables (Stabilize, UseBubbles, NonLinearIter,NonLinearTol,
! *  NonLinearIterAbort, Relax,DeathCapacity,BodyTemperature) done in allocation
! *  - Save of the preceding variables + other variables
! *****************************************************************************/
! *****************************************************************************/
! *  24/01/11:
! *  - Correction of default value setting for DeadThresold
! *****************************************************************************/
! *****************************************************************************/
! *  25/01/11:
! *  - Default value of InterpolatedElectricPower set to .FALSE.
! *  - Default value of NbElectricTips set to 10
! *  - Default value of ElectricPowerEpsilon set to 1.5
! *  - Default value of CellStateModel set to 2
! *  - Default value of Stabilize set to .TRUE.
! *  - Default value of NonlinearTol set to 3.0
! *  - Default value of CellsDeath set to TRUE
! *  - Default value of ElectricPowerCutOff set to TRUE
! *  - Default value of Volume Fraction set to (0.1,0.9)
! *  - Default value of Convective Transfer Coefficient set to 24.4
! *  - Default value of Density set to 0.00000106
! *  - Default value of DeathCapacity set to (4180000000.0,670000000.0)
! *  - Default value of Hwrk set to 512.1
! *  - Default value of HeatCapacity set to (4180000000.0,3600000000.0)
! *  - Default value of Heat Transfer Coefficient set to 230.0
! *  - Default value of External Temperature set to 310.0
! *****************************************************************************/
! *****************************************************************************/
! *  31/01/11:
! *  - Default value of Heat Transfer Coefficient set to 0.0
! *****************************************************************************/
! *****************************************************************************/
! *  21/02/11:
! *  - New variable VisualizeHeatSource (logical, default value=false), used to 
! *  add heat source as variable for vtu file
! *****************************************************************************/
! *****************************************************************************/
! *  15/03/11:
! *  - Modification of NumaSetInitialConditions() for Restart case 
! *****************************************************************************/
! *****************************************************************************/
! *  16/03/11:
! *  - Call and definition of NumaSetInitialConditions() removed
! *****************************************************************************/
! *****************************************************************************/
! *  06/06/11:
! *  - Output of actual total power over time in a file
! *    -New variables: ControlTotalPower (Logical), TotalPowerFileName (CHARACTER),
! *     TotalPower(Real)
! *    -After allocations, checks if the file has to be written and writes the 
! *     header of the file
! *    -Before beginning of nonlinear iterations, writes the current total power.
! *     If parallel execution, only master proc writes (after getting others contribution)
! *    -New routine TotalPowerCompose() to integrate power over element and update
! *     variable TotalPower (TotalPower is different than Integ_force when perfusion)
! *****************************************************************************/
! *****************************************************************************/
! *  28/06/11: 
! *  - Visualization of perfusion map:
! *     - New variable VisualizePerfusion (logical, default value=false), used to 
! *     add perfusion as variable for vtu file
! *     - New variable Perfusion (global)
! *     - Save Perfusion after each timestep for vtu output
! *****************************************************************************/
! *****************************************************************************/
! *  21/07/11: 
! *  - Control of velocity Size to avoid numerical instabilities:
! *     - New variable ControlVelocitySize (logical, default value=false)
! *     read from inputfile
! *     - New variable MaxVelocitySize (real, default value=2.0) read from
! *     input file and used to scale velocity  
! *****************************************************************************/
! *****************************************************************************/
! *  27/07/11: 
! *     - Temperature Controlled electric power added: 
! *         -New variables: TemperatureControlledPower (logical), PowerControl_Kp (real), 
! *     PowerControl_Ki (real), PowerControl_Kd (real), TargetTemperature (real), 
! *     PreviousPower (real), CurrentPower (real), ErrorToTargetTemperature (real), 
! *     IntegErrorToTargetTemperature (real),DerivErrorToTargetTemperature (real)
! *     ErrorToTargetTemperature (real) 
! *        -At allloc time, read the parameters in input file and initialize the
! *         relative variables
! *        -When computing heat source, check if power is read in input file or
! *         computed with PID controller (using the relative variables)
! *        -After system resolution, update the variables relative to PID controller
! *****************************************************************************/
! *****************************************************************************/
! *  05/08/11: 
! *     - Temperature Controlled electric power added: 
! *         -New variables: HistoryErrorToTargetTemperature (real) to save only a few
! *      previous steps for integral gain, PowerControl_Integ_Length to specify
! *      the number of time-step to use (read in input file)
! *     -Variables ErrorToTargetTemperature and ErrorToTargetTemperature removed
! *****************************************************************************/
! *****************************************************************************/
! *  29/08/11: 
! *     - If controlpower, use format F16.8 for power output: 
! *****************************************************************************/

SUBROUTINE NumaHeatSolver_init( Model,Solver,Timestep,TransientSimulation )

       USE DefUtils

       IMPLICIT NONE
       TYPE(Solver_t) :: Solver  
       TYPE(Model_t) :: Model    
       REAL(KIND=dp) :: Timestep
       LOGICAL :: TransientSimulation 

       !------------------------------------------------------------------------------
       !    Local variables
       !------------------------------------------------------------------------------
       TYPE(ValueList_t),POINTER :: SolverParams
       LOGICAL :: Found

       SolverParams => GetSolverParams()
       CALL ListAddString( SolverParams,&
            NextFreeKeyword('Exported Variable',SolverParams),'Heat Source Calc')
END SUBROUTINE NumaHeatSolver_init

!******************************************************************************
!------------------------------------------------------------------------------
SUBROUTINE NumaHeatSolver( Model,Solver,Timestep,TransientSimulation )
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Solve the heat equation 
!
!  ARGUMENTS:
!
!  TYPE(Model_t) :: Model,  
!     INPUT: All model information (mesh,materials,BCs,etc...)
!
!  TYPE(Solver_t) :: Solver
!     INPUT: Linear equation solver options
!
!  REAL(KIND=dp) :: Timestep
!     INPUT: Timestep size for time dependent simulations
!
!   LOGICAL :: TransientSimulation
!       INPUT: Steady state or transient simulation
!
!******************************************************************************
    USE MaterialModels
    USE DefUtils
    USE NumaAdaptive
!------------------------------------------------------------------------------
    IMPLICIT NONE
!------------------------------------------------------------------------------
    TYPE(Model_t)  :: Model
    TYPE(Solver_t), TARGET :: Solver
    LOGICAL :: TransientSimulation
    REAL(KIND=dp) :: Timestep
!------------------------------------------------------------------------------
!  Local variables
!------------------------------------------------------------------------------
    TYPE(Matrix_t), POINTER :: StiffMatrix
    TYPE(Variable_t), POINTER :: TempSol, CellStateSol, JouleHeatingSol, LoadVar
    TYPE(ValueList_t), POINTER :: Equation,Material,SolverParams,BodyForce,BC
    TYPE(Nodes_t)   :: ElementNodes
    TYPE(Element_t),POINTER :: Element
    TYPE(Variable_t), POINTER :: TimeVar
    
    INTEGER :: i,j,k,l,m,n,t,iter,body_id,eq_id,istat,LocalNodes, &
        bf_id, k_dof, TDOFs, NonlinearIter, DoneTime=0, ios,MaxNonLinearIterDone, &
        MaxCouplingIterDone,CouplingIter, ADOFs, JHDOFs, CellStateModel, NbElectricTips, MaxNbElectricPoints, &
        ModifDBC_Index, ierr,PowerControl_Integ_Length
        
    INTEGER, POINTER :: NodeIndexes(:), TempPerm(:), CellStatePerm(:), JouleHeatingPerm(:)
        
    INTEGER, ALLOCATABLE :: nbElectricpoints(:)

    CHARACTER(LEN=100) :: char_k_dof, char_MyPe, MaxTemperatureFilename,iterationsFilename, &
        TestName, char_ElectricTip, line, str1, str2, ElectricTipFile,TotalPowerFilename

    LOGICAL :: Stabilize = .FALSE., FirstTime, &
        Found, GotIt, AllocationsDone = .FALSE., NonlinearIterAbort = .TRUE., &
        CellsDeath= .FALSE., VarVolumeFraction= .FALSE., &
        ControlMaxTemperature= .FALSE., ControlIterations = .FALSE., &
        ControlError = .FALSE.,ConvergenceDone = .FALSE.,InterpolatedElectricPower = .FALSE., &
        ModifDBC, ModifDBCToFluxBC, ModifDBC_Select_Element, ElectricPowerCutOff = .FALSE., &
        UseBubbles = .FALSE., VisualizeHeatSource = .FALSE.,ControlTotalPower= .FALSE., &
        VisualizePerfusion = .FALSE.,ControlVelocitySize = .FALSE.,TemperatureControlledPower = .FALSE., &
        UseNormedJH = .FALSE.
    
    REAL(KIND=dp) :: NonlinearTol, Relax, &
        SaveRelax, dt, CumulativeTime, RelativeChange, Norm, PrevNorm, &
        at,at0,totat,st,totst,CPUTime,RealTime,Time,PrevTime,ErrorL2,ErrorH1,ErrorL0, &
        MeshSize,srealt,arealt, x,y,z, TotalElectricTipsMesure, ElectricPowerEpsilon, DistanceToTip, &
        xProjTip, yProjTip, zProjTip, TotalDistanceToTip, &
        ModifDBC_DistMax, ModifDBC_xc, ModifDBC_yc, ModifDBC_zc, ModifDBC_DiamMax, &
        Diameter, ModifDBC_TimeStart, ModifDBC_Dist, &
        allocrealtime,alloctime, BodyTemperature, DeadThreshold,tmpPower,MaxVelocitySize, &
        PowerControl_Kp, PowerControl_Ki, PowerControl_Kd,TargetTemperature

    REAL(KIND=dp), POINTER :: Temperature(:), ForceVector(:), Hwrk(:,:,:),C1(:,:), &
        HeatFluxArray(:,:), HeatTransferArray(:,:), AtextArray(:,:), CellState(:), VolF(:), &
        Perfusion(:), HeatSource(:), ElectricPowerVisualization(:), ElectricPower(:), &
        HistoryErrorToTargetTemperatureVariable(:), HistoryErrorToTargetTemperature(:,:), &
        JouleHeating(:)
        
    REAL(KIND=dp), ALLOCATABLE ::  &
        LOAD(:,:), HeatConductivity(:,:,:,:), U(:,:), V(:,:), W(:,:), &
        Density(:,:), HeatTransferCoeff(:,:), &
        HeatCapacity(:,:), Work(:,:), C0(:), AText(:,:), &
        LocalMASS(:,:), LocalSTIFF(:,:), LocalFORCE(:), &
        IntermaterialCoeff(:,:),HeatCapacityConvect(:,:), &
        VolumeFraction(:,:), MaxTemperature(:), LocalCellState(:,:), &
        DeathCapacity(:), DeltaTemperature(:), Integ_Force(:), power(:,:), &
        x_ElectricTip(:,:),y_ElectricTip(:,:),z_ElectricTip(:,:),ElectricTipMesure(:), &
        LocalHeatSource(:,:),TimeForce(:),PerfusionCoeff(:), TotalPower(:), &
        PreviousPower(:),CurrentPower(:), &
        IntegErrorToTargetTemperature(:),DerivErrorToTargetTemperature(:)

    SAVE U, V, W, LOAD, &
        ElementNodes, HeatConductivity, HeatCapacity, HeatTransferCoeff, &
        Density, AllocationsDone, LocalNodes, &
        Work, C0, AText,Hwrk, DoneTime, LocalMASS, LocalSTIFF, LocalFORCE, &
        IntermaterialCoeff,HeatCapacityConvect,C1,VolumeFraction,MaxTemperature, &
        VolF, LocalCellState, &
        Time,PrevTime,MaxNonLinearIterDone,MaxCouplingIterDone,CouplingIter,DeathCapacity, &
        DeltaTemperature,TestName,Integ_Force, power,ElectricPower,CellStateModel, &
        ModifDBC_Index, ModifDBC, ModifDBCToFluxBC, ModifDBC_DistMax, &
        ModifDBC_xc, ModifDBC_yc, ModifDBC_zc, ModifDBC_DiamMax,ModifDBC_TimeStart, &
        ElectricPowerCutOff,HeatSource,LocalHeatSource,TimeForce,PerfusionCoeff,BodyTemperature, &
        DeadThreshold,Stabilize,UseBubbles,NonlinearIter,NonlinearIterAbort,NonlinearTol,Relax, &
        VarVolumeFraction,CellsDeath,ControlMaxTemperature,ControlError,ControlIterations, &
        InterpolatedElectricPower,VisualizeHeatSource,ControlTotalPower, TotalPower, &
        VisualizePerfusion, Perfusion,ControlVelocitySize,MaxVelocitySize,TemperatureControlledPower, &
        PowerControl_Kp, PowerControl_Ki, PowerControl_Kd,TargetTemperature,PreviousPower,CurrentPower, &
        IntegErrorToTargetTemperature,DerivErrorToTargetTemperature,HistoryErrorToTargetTemperature, &
        PowerControl_Integ_Length
!------------------------------------------------------------------------------
!  Interfaces
!------------------------------------------------------------------------------     
    INTERFACE
!------------------------------------------------------------------------------
        FUNCTION TemperatureBoundaryResidual( Model,Edge,Mesh,Quant,Perm,Gnorm,NDOFs) RESULT(Indicator)
            USE Types
            TYPE(Element_t), POINTER :: Edge
            TYPE(Model_t) :: Model
            TYPE(Mesh_t), POINTER :: Mesh
            REAL(KIND=dp) :: Quant(:), Indicator(2), Gnorm
            INTEGER :: Perm(:)
            INTEGER :: NDOFs
        END FUNCTION TemperatureBoundaryResidual
!------------------------------------------------------------------------------
        FUNCTION TemperatureEdgeResidual( Model,Edge,Mesh,Quant,Perm,NDOFs) RESULT(Indicator)
            USE Types
            TYPE(Element_t), POINTER :: Edge
            TYPE(Model_t) :: Model
            TYPE(Mesh_t), POINTER :: Mesh
            REAL(KIND=dp) :: Quant(:), Indicator(2)
            INTEGER :: Perm(:)
            INTEGER :: NDOFs
        END FUNCTION TemperatureEdgeResidual
!------------------------------------------------------------------------------
        FUNCTION TemperatureInsideResidual( Model,Element,Mesh,Quant,Perm, Fnorm,NDOFs) RESULT(Indicator)
            USE Types
            TYPE(Element_t), POINTER :: Element
            TYPE(Model_t) :: Model
            TYPE(Mesh_t), POINTER :: Mesh
            REAL(KIND=dp) :: Quant(:), Indicator(2), Fnorm
            INTEGER :: Perm(:)
            INTEGER :: NDOFs
        END FUNCTION TemperatureInsideResidual
!------------------------------------------------------------------------------     
    END INTERFACE       
!------------------------------------------------------------------------------     
    CALL Info( 'NumaHeatSolve', ' ',Level=4 )
    CALL Info( 'NumaHeatSolve', '-------------------------------------',Level=4 )
    CALL Info( 'NumaHeatSolve', 'TEMPERATURE SOLVER:  ', Level=4 )
    CALL Info( 'NumaHeatSolve', '-------------------------------------',Level=4 )
    CALL Info( 'NumaHeatSolve', ' ',Level=4 )   
!------------------------------------------------------------------------------
!  Get variables needed for solution
!------------------------------------------------------------------------------
    IF ( .NOT. ASSOCIATED( Solver % Matrix ) ) RETURN

    StiffMatrix => Solver % Matrix
    ForceVector => Solver % Matrix % RHS

    TempSol => Solver % Variable
    TempPerm    => TempSol % Perm
    Temperature => TempSol % Values
    TDOFs =  TempSol % DOFs
    
    CellStateSol => VariableGet( Solver % Mesh % Variables, 'CellState' )
    IF ( ASSOCIATED( CellStateSol ) ) THEN
        CellStatePerm => CellStateSol % Perm
        CellState => CellStateSol % Values
        ADOFs =  CellStateSol % DOFs
    END IF
!------------------------------------------------------------------------------     
    LocalNodes = COUNT( TempPerm > 0 )
    IF ( LocalNodes <= 0 ) RETURN

    PRINT*, 'There are',LocalNodes,' nodes in the part of mesh treated by proc ',ParEnv % MyPe

    SolverParams => GetSolverParams()

!------------------------------------------------------------------------------
!  Allocate some permanent storage, this is done first time only
!------------------------------------------------------------------------------
    IF ( .NOT. AllocationsDone .OR. Solver % Mesh % Changed ) THEN
        
        !------------------------------------------------------------------------------ 
        !   Save time for computation of allocation time:
        !------------------------------------------------------------------------------
        alloctime = CPUTime()
        allocrealtime = RealTime()


        N = Solver % Mesh % MaxElementNodes
        
        IF ( AllocationsDone ) THEN
            DEALLOCATE(                     &
                U, V, W,                      &
                ElementNodes % x,             &
                ElementNodes % y,             &
                ElementNodes % z,             &
                Density,Work,                 &
                C0,                            &
                HeatTransferCoeff,            &
                HeatCapacity,                 &
                AText,                        &
                HeatConductivity,             &
                LOAD,                         &
                power,                                          &
                LocalMASS, LocalSTIFF,        &
                LocalFORCE,                   &
                InterMaterialCoeff,           &
                HeatCapacityConvect,          &
                C1,                           &
                VolumeFraction,              &
                MaxTemperature,              &
                VolF, LocalCellState,        &
                DeathCapacity,               &
                Integ_Force,                                &
                TotalPower,                              &
                DeltaTemperature,                       &
                ElectricPower,                          &
                HeatSource,                  &
                LocalHeatSource,                        &
                TimeForce,                                   &
                PerfusionCoeff,                         &               
                Perfusion,                                  &
                PreviousPower,                          &
                IntegErrorToTargetTemperature,   &
                DerivErrorToTargetTemperature,   &
                CurrentPower                                    )
        END IF

        ALLOCATE(                                         &
            U( TDOFs,N ),   V( TDOFs,N ),         &
            W( TDOFs,N ),                         &
            ElementNodes % x( N ),                &
            ElementNodes % y( N ),                &
            ElementNodes % z( N ),                &
            Density( TDOFs,N ),Work( TDOFs,N ),   &
            C0(N),                                &
            HeatTransferCoeff( TDOFs,N ),         &
            HeatCapacity( TDOFs,N ),              &
            AText( TDOFs,N ),                     &
            HeatConductivity( TDOFs,3,3,N ),      &
            LOAD( TDOFs,N ),                      &
            power( TDOFs,N ),                     &  
            LocalSTIFF( TDOFs*2*N,TDOFs*2*N ),    & 
            LocalMASS( TDOFs*2*N,TDOFs*2*N ),     & 
            LocalFORCE( TDOFs*2*N ),              & 
            HeatFluxArray( TDOFs,N ),             & 
            HeatTransferArray( TDOFs,N ),         & 
            AtextArray( TDOFs,N ),                & 
            InterMaterialCoeff( TDOFs,N ),        & 
            C1(TDOFs,N),                          & 
            HeatCapacityConvect( TDOFs,N ),       &
            VolumeFraction(TDOFs,N),              &
            MaxTemperature(TDOFs),                &
            VolF(LocalNodes),                     &
            LocalCellState(ADOFs,N),              &
            DeathCapacity(TDOFs),                 &
            Integ_Force(TDOFs),                                 &
            TotalPower(TDOFs),                                  &
            DeltaTemperature(TDOFs*LocalNodes),   &
            ElectricPower(TDOFs*LocalNodes),      &
            HeatSource(TDOFs*LocalNodes),         &
            LocalHeatSource(TDOFs,N),             &
            TimeForce(2*N),                                         &
            PerfusionCoeff( N ),                        &
            Perfusion(LocalNodes),                &
            PreviousPower(TDOFs*LocalNodes),          &
            IntegErrorToTargetTemperature(TDOFs*LocalNodes),&
            DerivErrorToTargetTemperature(TDOFs*LocalNodes),&
            CurrentPower(TDOFs*LocalNodes),         &
            STAT=istat                            )

        IF ( istat /= 0 ) THEN
            CALL Fatal( 'NumaHeatSolve', 'Memory allocation error' )
        END IF

        NULLIFY( Hwrk )
        AllocationsDone = .TRUE.
!------------------------------------------------------------------------------ 
!       Get the test name
!------------------------------------------------------------------------------
        TestName = GetString( CurrentModel % Simulation,'Test Name',GotIt ) 
!------------------------------------------------------------------------------ 
!   If space dependent volume fraction specified in input file, add the 
!       volume fraction of blood as a new variable:
!------------------------------------------------------------------------------ 
        VolF = 0.0D0
!------------------------------------------------------------------------------ 
!   Check if space dependent volume fraction specified in input file 
!------------------------------------------------------------------------------ 
        VarVolumeFraction = GetLogical( SolverParams,'Variable Volume Fraction',Found )
        IF ( .NOT.Found ) VarVolumeFraction = .FALSE.
        IF(VarVolumeFraction) THEN
!------------------------------------------------------------------------------ 
!       Add the volume fraction of blood as a new variable
!------------------------------------------------------------------------------     
            CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
                Solver, 'Blood Volume fraction', 1, &
                VolF, TempPerm )
        END IF
!------------------------------------------------------------------------------ 
!   Check if Cell Death Modelling specified in input file 
!------------------------------------------------------------------------------ 
        CellsDeath = GetLogical( SolverParams,'Cell Death Modelling',Found )
        IF ( .NOT.Found ) CellsDeath = .TRUE.
!------------------------------------------------------------------------------
!       Physical time of the current and previous coupled system iterations: 
!------------------------------------------------------------------------------             
        Time = 0.0
        PrevTime = 0.0
!------------------------------------------------------------------------------
!       If specified in the input file, compute the maximum temperature over the model: 
!------------------------------------------------------------------------------ 
        ControlMaxTemperature = GetLogical( SolverParams,'Control Max Temperature',Found )      
        IF ( .NOT.Found ) ControlMaxTemperature = .FALSE.
        IF (ControlMaxTemperature) THEN
            MaxTemperature = 0.0D0
            DO i=1,LocalNodes
                DO k_dof=1,TDOFs
                    IF (MaxTemperature(k_dof) < Temperature((i-1)*TDOFs+k_dof)) THEN
                        MaxTemperature(k_dof) = Temperature((i-1)*TDOFs+k_dof)
                    END IF
                END DO  
            END DO
!------------------------------------------------------------------------------ 
!       Write the header of the max-temperature control file
!------------------------------------------------------------------------------     
            IF(ParEnv % PEs>1) THEN
                WRITE(char_MyPe,*) ParEnv % MyPe
                char_MyPe = ADJUSTL(char_MyPe)
                MaxTemperatureFilename = 'maxtemp'//'_'//TRIM(TestName)//'_'//TRIM(char_MyPe)//'.dat'       
            ELSE 
                MaxTemperatureFilename = 'maxtemp'//'_'//TRIM(TestName)//'.dat'         
            END IF
            
            OPEN(UNIT=1,FILE=MaxTemperatureFilename)
            WRITE(UNIT=1,FMT=*) 'Time    ', '    Blood Temperature    ', '    Tissue Temperature'
            CLOSE(1)
!------------------------------------------------------------------------------
        END IF ! ControlMaxTemperature
!------------------------------------------------------------------------------ 
!       If saving the total electric power in a control file, write header:
!------------------------------------------------------------------------------ 
        TotalPower = 0.0
        ControlTotalPower = GetLogical( SolverParams,'Control Total Power',Found )      
        IF ( .NOT.Found ) ControlTotalPower = .TRUE.
        IF ((ControlTotalPower) .AND. (ParEnv % MyPe==0)) THEN  
            TotalPowerFilename = 'totalpower'//'_'//TRIM(TestName)//'.csv'          
            OPEN(UNIT=1,FILE=TotalPowerFilename)
            WRITE(UNIT=1,FMT=*) 'Time,', 'Blood-Total-Power,', 'Tissue-Total-Power'
            CLOSE(1)
        END IF ! ControlTotalPower      
!------------------------------------------------------------------------------
!       If specified in the input file, compute the error between numerical and 
!       analytical solution
!------------------------------------------------------------------------------ 
        ControlError = GetLogical( SolverParams,'Control Errors',Found )        
        IF ( .NOT.Found ) ControlError = .FALSE.
!------------------------------------------------------------------------------
!       If specified in the input file, compute the maximum number of non-linear 
!       iterations and coupling iterations between solvers done during the whole 
!       simulation
!------------------------------------------------------------------------------ 
        ControlIterations = GetLogical( SolverParams,'Control Iterations',Found )       
        IF ( .NOT.Found ) ControlIterations = .FALSE.
        IF (ControlIterations) THEN
!------------------------------------------------------------------------------     
!           Maximum number of non-linear iterations done during the whole simulation:
!------------------------------------------------------------------------------
            MaxNonLinearIterDone = 1
!------------------------------------------------------------------------------ 
!       Maximum number of coupling iterations between solvers done during the 
!           whole simulation:
!------------------------------------------------------------------------------             
            MaxCouplingIterDone = 0 
            CouplingIter = 0
!------------------------------------------------------------------------------ 
!       Header of iterations file
!------------------------------------------------------------------------------             
            IF(ParEnv % PEs>1) THEN
                WRITE(char_MyPe,*) ParEnv % MyPe
                char_MyPe = ADJUSTL(char_MyPe)
                iterationsFilename = 'iterations'//'_'//TRIM(TestName)//'_'//TRIM(char_MyPe)//'.dat'        
            ELSE 
                iterationsFilename = 'iterations'//'_'//TRIM(TestName)//'.dat'          
            END IF          
            OPEN(UNIT=1,FILE=iterationsFilename, FORM='FORMATTED', ACTION='WRITE', POSITION='APPEND', &
                IOSTAT=ios)
            WRITE(UNIT=1,FMT=*) 'Time    ', '    MaxNonlinearIterDone    ', '   CouplingIter'
            CLOSE(1)
!------------------------------------------------------------------------------ 
        END IF ! ControlIterations
!------------------------------------------------------------------------------ 
!       Get electric power geometry if interpolated (multi)line source:
!------------------------------------------------------------------------------ 
        InterpolatedElectricPower = GetLogical( Model % Simulation, &
            'Interpolated Electric Power',Found )
        IF (.NOT. Found) InterpolatedElectricPower = .FALSE.

        JouleHeatingSol => VariableGet(Solver % Mesh % Variables, 'Normed Joule Heating' )
        IF ( ASSOCIATED( JouleHeatingSol ) ) THEN
            JouleHeatingPerm => JouleHeatingSol % Perm
            JouleHeating => JouleHeatingSol % Values
            JHDOFs =  JouleHeatingSol % DOFs
            UseNormedJH = .TRUE.
        END IF

!------------------------------------------------------------------------------ 
        IF (InterpolatedElectricPower) THEN
!------------------------------------------------------------------------------
!           Get the number of tips:
!------------------------------------------------------------------------------ 
            NbElectricTips = GetInteger( Model % Simulation, &
                'Electric Tips Number', Found )
            IF ( .NOT.Found ) NbElectricTips = 10
!------------------------------------------------------------------------------
!           Get the (numerical) width of the tips:
!------------------------------------------------------------------------------ 
            ElectricPowerEpsilon = GetConstReal( Model % Simulation, &
                'Electric Power Epsilon', Found )
            IF ( .NOT.Found ) ElectricPowerEpsilon = 1.5
!------------------------------------------------------------------------------
!           Read the coordinates of the points defining the tips in text files
!------------------------------------------------------------------------------
            MaxNbElectricPoints = 10        
            ALLOCATE(x_ElectricTip(NbElectricTips,MaxNbElectricPoints), &
                y_ElectricTip(NbElectricTips,MaxNbElectricPoints), &
                z_ElectricTip(NbElectricTips,MaxNbElectricPoints), &
                ElectricTipMesure(NbElectricTips), &
                nbElectricpoints(NbElectricTips)) 

            x_ElectricTip = 0.0D0
            y_ElectricTip = 0.0D0
            z_ElectricTip = 0.0D0
            ElectricTipMesure = 0.0D0
            TotalElectricTipsMesure = 0.0D0
            NbElectricPoints = 0
            !------------------------------------------------------------------------------
            !   Go through the tips
            !------------------------------------------------------------------------------     
            DO j=1,NbElectricTips
            !------------------------------------------------------------------------------     
            !   Open the Tips file:
            !------------------------------------------------------------------------------  
                ElectricTipFile = GetString( Model % Simulation,'Electric Tips Filename Root',Found )   
                !------------------------------------------------------------------------------ 
                IF (Found) THEN
                !------------------------------------------------------------------------------ 
                    WRITE(char_ElectricTip,*) j
                    char_ElectricTip = ADJUSTL(char_ElectricTip)
                    ElectricTipFile = TRIM(ElectricTipFile) // "_" // TRIM(char_ElectricTip) // ".txt"
                    OPEN(UNIT=10,FILE=ElectricTipFile,STATUS='OLD',ACTION='READ',IOSTAT=ios)
                    !------------------------------------------------------------------------------ 
                    IF(ios/=0) THEN
                        PRINT*,'Could not open file ',ElectricTipFile
                        PRINT*,'I/O Fortran Error number ',ios
                        CALL Fatal( 'NumaHeatSolve', 'Unable to load electric tips file' )
                    ELSE
                        !------------------------------------------------------------------------------ 
                        !   Read the number of points defining the tip geometry:
                        !------------------------------------------------------------------------------ 
                        READ(10,*,END=1) line
                        READ(10,*,END=1) str1, NbElectricPoints(j), str2
                        !------------------------------------------------------------------------------
                        DO i=1,NbElectricPoints(j)  
                            !------------------------------------------------------------------------------
                            !       Read the coordinates:
                            !------------------------------------------------------------------------------
                            READ(10,*,END=1) x_ElectricTip(j,i), y_ElectricTip(j,i), z_ElectricTip(j,i)
                        !------------------------------------------------------------------------------
                        END DO
                        !------------------------------------------------------------------------------
                        1 CONTINUE
                        CLOSE(10)
                    !------------------------------------------------------------------------------ 
                    END IF
                !------------------------------------------------------------------------------ 
                ELSE
                !------------------------------------------------------------------------------
                !   If the file can't be found, print an error message and stop the simulation: 
                !------------------------------------------------------------------------------
                    CALL Info('NumaHeatSolve', &
                        'Please specify electric tips file name root in input file.', Level=1 )
                    CALL Fatal( 'NumaHeatSolve', 'Unable to load electric tips file' )
                !------------------------------------------------------------------------------
                END IF ! name of the tip file found
                !------------------------------------------------------------------------------
                ! Compute the length of the tip:
                !------------------------------------------------------------------------------
                ElectricTipMesure = 0.0D0
                !------------------------------------------------------------------------------
                !   Case of point source
                !------------------------------------------------------------------------------
                IF(NbElectricPoints(j)==1) THEN
                !------------------------------------------------------------------------------
                    ElectricTipMesure = 1.0D0
                !------------------------------------------------------------------------------
                ELSE
                !------------------------------------------------------------------------------
                    DO i=1,NbElectricPoints(j)-1    
                    !------------------------------------------------------------------------------
                        ElectricTipMesure(j) = ElectricTipMesure(j) + sqrt( (x_ElectricTip(j,i+1)-x_ElectricTip(j,i))**2 +  &
                            (y_ElectricTip(j,i+1)-y_ElectricTip(j,i))**2 + &
                            (z_ElectricTip(j,i+1)-z_ElectricTip(j,i))**2 )
                    !------------------------------------------------------------------------------
                    END DO
                !------------------------------------------------------------------------------
                END IF
                !------------------------------------------------------------------------------
                ! Update the total mesure of the electric source
                !------------------------------------------------------------------------------
                TotalElectricTipsMesure = TotalElectricTipsMesure + ElectricTipMesure(j)
            !------------------------------------------------------------------------------
            END DO !j
            !------------------------------------------------------------------------------
            !   Compute the electric power distribution:
            !------------------------------------------------------------------------------
            ElectricPower = 0.0D0
            !------------------------------------------------------------------------------
            ! Go through the nodes
            !------------------------------------------------------------------------------ 
            DO i=1,LocalNodes
            !------------------------------------------------------------------------------ 
                x = Model % Nodes % x(i)
                y = Model % Nodes % y(i)
                z = Model % Nodes % z(i)
                TotalDistanceToTip = 1000000
                !------------------------------------------------------------------------------ 
                ! Go through the tips
                !------------------------------------------------------------------------------ 
                DO j=1,NbElectricTips
                    !------------------------------------------------------------------------------ 
                    ! Compute the distance to the tip
                    !------------------------------------------------------------------------------ 
                    DistanceToTip = NumaDistanceToElectricTip (x, y, z, x_ElectricTip(j,:), &
                        y_ElectricTip(j,:), z_ElectricTip(j,:), nbElectricpoints(j), &
                        xProjTip, yProjTip, zProjTip )
                    !------------------------------------------------------------------------------ 
                    ! The electric power at each node comes from the closest tip
                    !------------------------------------------------------------------------------
                    IF (TotalDistanceToTip>DistanceToTip) THEN
                    !------------------------------------------------------------------------------
                        DO k_dof=1,TDOFs
                            !------------------------------------------------------------------------------
                            ! Test if we are in the neighboorhoof of the tip
                            !------------------------------------------------------------------------------
                            IF (DistanceToTip<ElectricPowerEpsilon) THEN
                                !------------------------------------------------------------------------------ 
                                ! If source = one point
                                !------------------------------------------------------------------------------ 
                                IF (nbElectricpoints(j)==1) THEN
                                    !------------------------------------------------------------------------------ 
                                    !   Constant power distibution
                                    !------------------------------------------------------------------------------ 
                                    ElectricPower((TempPerm(i)-1)*TDOFs+k_dof) = &
                                            (1+0.0*COS(Pi*DistanceToTip/ElectricPowerEpsilon))/&
                                            (4.0*Pi/3.0*ElectricPowerEpsilon**3)
                                    !------------------------------------------------------------------------------ 
                                    !   Smoothed power distibution
                                    !------------------------------------------------------------------------------ 
                                    ! TO BE DONE
                                !------------------------------------------------------------------------------ 
                                ELSE
                                    !------------------------------------------------------------------------------ 
                                    ! If source = closed line
                                    !------------------------------------------------------------------------------ 
                                    IF ((x_ElectricTip(j,1)==x_ElectricTip(j,nbElectricpoints(j))) .AND. &
                                    (y_ElectricTip(j,1)==y_ElectricTip(j,nbElectricpoints(j))) .AND. &
                                    (z_ElectricTip(j,1)==z_ElectricTip(j,nbElectricpoints(j)))) THEN
                                        !------------------------------------------------------------------------------ 
                                        !   Constant power distibution
                                        !------------------------------------------------------------------------------ 
                                        ElectricPower((TempPerm(i)-1)*TDOFs+k_dof) = &
                                            (1+0.0*COS(Pi*DistanceToTip/ElectricPowerEpsilon))/&
                                            (TotalElectricTipsMesure*Pi*ElectricPowerEpsilon**2)
                                        !------------------------------------------------------------------------------ 
                                        !   Smoothed power distibution
                                        !------------------------------------------------------------------------------ 
                                        ! TO BE DONE
                                    !------------------------------------------------------------------------------ 
                                    ELSE
                                        !------------------------------------------------------------------------------ 
                                        !   Constant power distibution
                                        !------------------------------------------------------------------------------     
                                        ElectricPower((TempPerm(i)-1)*TDOFs+k_dof) = &
                                            (1+0.0*COS(Pi*DistanceToTip/ElectricPowerEpsilon))/&
                                            (TotalElectricTipsMesure*Pi*ElectricPowerEpsilon**2+ &
                                            !------------------------------------------------------------------------------ 
                                            ! 0.5 coeff for particular case of electric probe 
                                            !------------------------------------------------------------------------------ 
                                            0.5*4.0*Pi/3.0*ElectricPowerEpsilon**3)
                                            !------------------------------------------------------------------------------ 
                                            !   Smoothed power distibution
                                            !------------------------------------------------------------------------------ 
                                            ! TO BE DONE
                                    !------------------------------------------------------------------------------ 
                                    END IF ! line type
                                    !------------------------------------------------------------------------------                         
                                END IF ! point or line source
                                !------------------------------------------------------------------------------ 
                            END IF !closest tip
                        !------------------------------------------------------------------------------
                        END DO !k_dof   
                    !------------------------------------------------------------------------------
                    END IF !neighbourhood of the tip
                    !------------------------------------------------------------------------------ 
                END DO !j
                !------------------------------------------------------------------------------
                ! Add the electric power as a variable for vizualization only   
                !------------------------------------------------------------------------------
                ElectricPowerVisualization => ElectricPower(2:TDOFs*(LocalNodes-1)+2:TDOFs)
                CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
                Solver, 'Electric power', 1, &
                ElectricPowerVisualization, TempPerm )  
                !------------------------------------------------------------------------------     
            END DO !i
        !------------------------------------------------------------------------------ 
        END IF !InterpolatedElectricPower

!------------------------------------------------------------------------------ 
!       Read the death model in input file
!------------------------------------------------------------------------------ 
        CellStateModel = GetInteger( Model % Simulation,'Cell State Model', Found )
        IF ( .NOT.Found ) CellStateModel = 2
        
        
        IF (CellStateModel == 2) THEN
            DeadThreshold = GetConstReal( SolverParams, 'Model 2 Dead Threshold', Found )
        END IF
        IF ( .NOT.Found ) DeadThreshold = 0.8
!------------------------------------------------------------------------------
!   Check if some Dirichlet BC have to be modified to flux BC at some elements
!------------------------------------------------------------------------------
        ModifDBC = .FALSE.
        ModifDBCToFluxBC = .FALSE.

        ModifDBC = ListGetLogical( SolverParams, 'Dirichlet Modified BC ', Found )
        !------------------------------------------------------------------------------
        ! Check if modification to a flux BC:
        !------------------------------------------------------------------------------
        ModifDBCToFluxBC = ListGetLogical( SolverParams, &
            'Dirichlet Modified BC To Flux BC', Found )
        !------------------------------------------------------------------------------
        IF (ModifDBC .AND. ModifDBCToFluxBC) THEN
            !------------------------------------------------------------------------------
            ! Index of the boundary condition where is specified the modification:
            !------------------------------------------------------------------------------
            ModifDBC_Index = GetInteger( SolverParams, 'Dirichlet Modified BC Index', Found )
            IF (ModifDBC_Index>Model % NumberOfBCs) THEN
                CALL Fatal( 'NumaSetDirichletBoundaries', 'Check Dirichlet Modified BC Index!' )
            END IF
            !------------------------------------------------------------------------------
            ! Distance from target under which is applied the modification:
            !------------------------------------------------------------------------------
            ModifDBC_DistMax = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                'Dirichlet Modified BC Distance Max', Found )
            IF (.NOT. Found) THEN
                CALL Error( 'NumaHeatSolve', 'Check Dirichlet Modified BC Distance Max!' )
                ModifDBC_DistMax = 0.0 
            END IF
            !------------------------------------------------------------------------------
            ! x-coord of the target around which is applied the modification:
            !------------------------------------------------------------------------------
            ModifDBC_xc = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                'Dirichlet Modified BC xc', Found )
            IF (.NOT. Found) THEN
                CALL Error( 'NumaHeatSolve', 'Check Dirichlet Modified BC xc!' )
            END IF
            !------------------------------------------------------------------------------
            ! y-coord of the target around which is applied the modification:
            !------------------------------------------------------------------------------
            ModifDBC_yc = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                'Dirichlet Modified BC yc', Found )
            IF (.NOT. Found) THEN
                CALL Error( 'NumaHeatSolve', 'Check Dirichlet Modified BC yc!' )
            END IF
            !------------------------------------------------------------------------------
            ! z-coord of the target around which is applied the modification:
            !------------------------------------------------------------------------------
            ModifDBC_zc = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                'Dirichlet Modified BC zc', Found )
            IF (.NOT. Found) THEN
                CALL Error( 'NumaHeatSolve', 'Check Dirichlet Modified BC zc!' )
            END IF
            !------------------------------------------------------------------------------
            ! Diameter max of the elements on which is applied the modification:
            !------------------------------------------------------------------------------
            ModifDBC_DiamMax = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                'Dirichlet Modified BC Diameter Max', Found )
            IF (.NOT. Found) THEN
                CALL Info( 'NumaHeatSolve', 'No Dirichlet Modified BC Diameter Max specified' )
                ModifDBC_DiamMax = -1.0 
            END IF
            !------------------------------------------------------------------------------
            ! Start time at which is applied the modification:
            !------------------------------------------------------------------------------
            ModifDBC_TimeStart = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                'Dirichlet Modified BC Time Start', Found )
            IF (.NOT. Found) THEN
                ModifDBC_TimeStart = 0.0
            END IF
        !------------------------------------------------------------------------------
        END IF
!------------------------------------------------------------------------------
!       Check if electric power has to be cut off when T>373 to approx water evaporation
!------------------------------------------------------------------------------
        ElectricPowerCutOff = ListGetLogical( SolverParams, 'Electric Power Cut Off ', Found )
        IF ( .NOT.Found ) ElectricPowerCutOff = .TRUE.
!------------------------------------------------------------------------------ 
!   Read some solver options in the input file
!------------------------------------------------------------------------------
    Stabilize = GetLogical( SolverParams,'Stabilize',Found )
    IF ( .NOT.Found ) Stabilize = .TRUE.

    UseBubbles = GetLogical( SolverParams,'Bubbles',Found )
    IF ( .NOT.Found ) UseBubbles = .FALSE.
!------------------------------------------------------------------------------ 
!   Maximum number of nonlinear iterations
!------------------------------------------------------------------------------ 
    NonlinearIter = GetInteger( SolverParams, &
        'Nonlinear System Max Iterations', Found )
    IF ( .NOT.Found ) NonlinearIter = 1
!------------------------------------------------------------------------------ 
!   Tolerance to be reached during nonlinear iterations
!------------------------------------------------------------------------------ 
    NonlinearTol = GetConstReal( SolverParams, &
        'Nonlinear System Convergence Tolerance', Found )
    IF ( .NOT.Found ) NonlinearTol = 3.0
!------------------------------------------------------------------------------ 
!   Stop criteria if NonlinearTol not reached during NonlinearIter
!------------------------------------------------------------------------------     
    NonlinearIterAbort = GetLogical( SolverParams, &
        'Nonlinear System Abort Not Converged',Found )
    IF ( .NOT.Found ) NonlinearIterAbort = .TRUE.
!------------------------------------------------------------------------------ 
!   Relaxation factor for convergence of the nonlinear iterations
!------------------------------------------------------------------------------ 
    Relax = GetCReal( SolverParams, &
        'Nonlinear System Relaxation Factor',Found )
    IF ( .NOT.Found ) Relax = 1
!------------------------------------------------------------------------------
!   Get body temperature from input file (for perfusion)
!------------------------------------------------------------------------------
    !BodyTemperature = GetConstReal( Material, &
    !    'Body Temperature', Found )
    BodyTemperature=310.0D0

!------------------------------------------------------------------------------ 
!   Check if velocity size has to be scaled to avoid unstabilities 
!------------------------------------------------------------------------------ 
        ControlVelocitySize = GetLogical( SolverParams,'Control Velocity Size',Found )
        IF ( .NOT.Found ) ControlVelocitySize = .FALSE.
        IF(ControlVelocitySize) THEN
!------------------------------------------------------------------------------ 
!           Get the max size for each component:
!------------------------------------------------------------------------------     
            MaxVelocitySize = GetConstReal( SolverParams, 'Max Velocity Size', Found )
            IF ( .NOT.Found ) MaxVelocitySize=2.0D0
        END IF
!------------------------------------------------------------------------------ 
!   Check if the electric power has to be controlled in function of temperature 
!------------------------------------------------------------------------------
        TemperatureControlledPower = GetLogical( Model % Simulation, &
            'Temperature Controlled Electric Power',Found )
        IF ( .NOT.Found ) TemperatureControlledPower = .FALSE.
!------------------------------------------------------------------------------
        IF(TemperatureControlledPower) THEN
!------------------------------------------------------------------------------ 
!       Get the target temperature:
!------------------------------------------------------------------------------     
            TargetTemperature = GetConstReal( Model % Simulation, &
                'Target Temperature For Electric Power Control', Found )
            IF ( .NOT.Found ) TargetTemperature = 373.15
!------------------------------------------------------------------------------ 
!       Get the PID parameters:
!------------------------------------------------------------------------------     
            PowerControl_Kp = GetConstReal( Model % Simulation, &
                'Proportional Gain For Electric Power Control', Found )
            IF ( .NOT.Found ) PowerControl_Kp = 0.0

            PowerControl_Kd = GetConstReal( Model % Simulation, &
                'Derivative Gain For Electric Power Control', Found )
            IF ( .NOT.Found ) PowerControl_Kd = 0.0

            PowerControl_Ki = GetConstReal( Model % Simulation, &
                'Integral Gain For Electric Power Control', Found )
            IF ( .NOT.Found ) PowerControl_Ki = 0.0

            PowerControl_Integ_Length = GetInteger( Model % Simulation, &
                'Integral Length For Electric Power Control', Found )
            IF ( .NOT.Found ) PowerControl_Integ_Length = 0

            ALLOCATE(HistoryErrorToTargetTemperature(MAX(PowerControl_Integ_Length,2),TDOFs*LocalNodes))

            PRINT *,'PID Temperature Controlled Electric Power, with parameters:'
            PRINT *,'- PID Proportional gain = ',PowerControl_Kp            
            PRINT *,'- PID Derivative gain = ',PowerControl_Kd  
            PRINT *,'- PID Integral gain = ',PowerControl_Ki    
            PRINT *,'- PID Target Temperature (K) = ',TargetTemperature 
            PRINT *,'- PID Integration Length = ',PowerControl_Integ_Length
!------------------------------------------------------------------------------     
!           Initialize the corresponding variables:
!------------------------------------------------------------------------------     
            CurrentPower = 0.0D0
            HistoryErrorToTargetTemperature = 0.0D0
            DO i=1,TDOFs*LocalNodes
                HistoryErrorToTargetTemperature(1,i) = TargetTemperature-Temperature(i)
            END DO
            IntegErrorToTargetTemperature(:) = HistoryErrorToTargetTemperature(1,:)*Timestep
            DerivErrorToTargetTemperature = 0.0D0
!------------------------------------------------------------------------------     
        END IF !TemperatureControlledPower
!------------------------------------------------------------------------------ 
!       Compute and print allocation time:
!------------------------------------------------------------------------------
        alloctime = CPUTime() - alloctime
        allocrealtime = RealTime() - allocrealtime
        WRITE(Message,'(a,F8.2,F8.2)') 'Allocation time (CPU,REAL): (s)', alloctime,allocrealtime
        CALL Info('NumaHeatSolve',Message,Level=4 )
!------------------------------------------------------------------------------
    END IF ! not allocations done
!------------------------------------------------------------------------------
!  Do some additional initialization, and go for it
!------------------------------------------------------------------------------
    dt = Timestep   
!------------------------------------------------------------------------------
    SaveRelax = Relax
    CumulativeTime = 0.0d0
!------------------------------------------------------------------------------
    FirstTime = .TRUE.
!------------------------------------------------------------------------------     
    DO WHILE( CumulativeTime < Timestep-1.0d-12 .OR. .NOT. TransientSimulation )
    PRINT *, "CC",CumulativeTime, "TT",Timestep
    !------------------------------------------------------------------------------
    !  The first time around this has been done by the caller
    !------------------------------------------------------------------------------
        IF ( TransientSimulation .AND. .NOT.FirstTime ) THEN
            CALL InitializeTimestep(Solver)
        END IF
        FirstTime = .FALSE.
        !------------------------------------------------------------------------------   
        totat = 0.0d0
        totst = 0.0d0
        !------------------------------------------------------------------------------ 
        !  Get current (physical) time
        !------------------------------------------------------------------------------
        TimeVar => VariableGet( CurrentModel % Solver % Mesh % Variables, 'Time' )
        Time = TimeVar % Values(1)  
        !------------------------------------------------------------------------------         
        !   Save Max temperature of the previous time in a file 
        !   Control that this is a new global time iteration, i.e. not only the nonlinear and 
        !  coupled system iterations have changed
        !------------------------------------------------------------------------------             
        IF ( (ControlMaxTemperature) .AND. (PrevTime < Time) ) THEN
            IF(ParEnv % PEs>1) THEN
                WRITE(char_MyPe,*) ParEnv % MyPe
                char_MyPe = ADJUSTL(char_MyPe)
                MaxTemperatureFilename = 'maxtemp'//'_'//TRIM(TestName)//'_'//TRIM(char_MyPe)//'.dat'       
            ELSE 
                MaxTemperatureFilename = 'maxtemp'//'_'//TRIM(TestName)//'.dat'         
            END IF
            OPEN(UNIT=1,FILE=MaxTemperatureFilename, FORM='FORMATTED', ACTION='WRITE', POSITION='APPEND', &
                IOSTAT=ios)
            WRITE(UNIT=1,FMT='(F13.4)', ADVANCE='no') PrevTime
            DO k_dof=1,TDOFs-1          
                WRITE(UNIT=1,FMT='(F13.4)', ADVANCE='no') MaxTemperature(k_dof)
            END DO
            WRITE(UNIT=1,FMT='(F13.4)', ADVANCE='yes') MaxTemperature(TDOFs)
            CLOSE(1)
        END IF
        !------------------------------------------------------------------------------         
        !   Save total power of the previous time in a file 
        !   Control that this is a new global time iteration, i.e. not only the nonlinear and 
        !  coupled system iterations have changed
        !  If parralel, only mester proc writes, after getting contribution from other procs
        !------------------------------------------------------------------------------         
        ! Get contribution from the other procs for parallel execution
        !------------------------------------------------------------------------------
        IF (ParEnv % PEs > 1) THEN
            DO k_dof=1,TDOFs
            !------------------------------------------------------------------------------
                tmpPower = TotalPower(k_dof)
                CALL MPI_ALLREDUCE( tmpPower, TotalPower(k_dof), 1, MPI_DOUBLE_PRECISION, &
                    MPI_SUM, MPI_COMM_WORLD, ierr )
            !------------------------------------------------------------------------------
            END DO
        END IF
        !------------------------------------------------------------------------------
        ! Update the variable PreviousPower
        !------------------------------------------------------------------------------
        IF(TemperatureControlledPower) THEN
        !------------------------------------------------------------------------------
            PreviousPower = CurrentPower
        !------------------------------------------------------------------------------
        END IF !TemperatureControlledPower
        !------------------------------------------------------------------------------
        IF ( (ControlTotalPower) .AND. (PrevTime < Time) .AND. (ParEnv % MyPe==0) ) THEN
        !------------------------------------------------------------------------------
            TotalPowerFilename = 'totalpower'//'_'//TRIM(TestName)//'.csv'  
            !------------------------------------------------------------------------------
            ! Modify the file
            !------------------------------------------------------------------------------         
            OPEN(UNIT=1,FILE=TotalPowerFilename, FORM='FORMATTED', ACTION='WRITE', POSITION='APPEND', &
                IOSTAT=ios)
            WRITE(UNIT=1,FMT='(F13.4)', ADVANCE='no') PrevTime
            WRITE(UNIT=1,FMT='(A)', ADVANCE='no') ','
            DO k_dof=1,TDOFs-1          
                WRITE(UNIT=1,FMT='(F16.4)', ADVANCE='no') TotalPower(k_dof)
                WRITE(UNIT=1,FMT='(A)', ADVANCE='no') ','
            END DO
            WRITE(UNIT=1,FMT='(F16.4)', ADVANCE='yes') TotalPower(TDOFs)
            CLOSE(1)
        !------------------------------------------------------------------------------
        END IF
        !------------------------------------------------------------------------------         
        !   Save number of coupled system iterations and max number of non-linear iterations 
        !  done in the time step in a file  
        !   Control that this is a new global time iteration, i.e. not only the nonlinear and 
        !  coupled system iterations have changed
        !------------------------------------------------------------------------------             
        IF ( (ControlIterations) .AND. (PrevTime < Time) ) THEN
            IF(ParEnv % PEs>1) THEN
                WRITE(char_MyPe,*) ParEnv % MyPe
                char_MyPe = ADJUSTL(char_MyPe)
                iterationsFilename = 'iterations'//'_'//TRIM(TestName)//'_'//TRIM(char_MyPe)//'.dat'        
            ELSE 
                iterationsFilename = 'iterations'//'_'//TRIM(TestName)//'.dat'          
            END IF
            OPEN(UNIT=1,FILE=iterationsFilename, FORM='FORMATTED', ACTION='WRITE', POSITION='APPEND', &
                IOSTAT=ios)
            WRITE(UNIT=1,FMT=*) PrevTime, MaxNonLinearIterDone, CouplingIter
            CLOSE(1)
        END IF
        !------------------------------------------------------------------------------ 
        ! Compute the norm of the solution
        !------------------------------------------------------------------------------         
        Norm = SQRT( SUM( Temperature**2 ) / (TDOFs*LocalNodes) )
        !------------------------------------------------------------------------------
        !  Non linear iterations
        !------------------------------------------------------------------------------
        ConvergenceDone = .FALSE.
        !------------------------------------------------------------------------------
        DO iter=1,NonlinearIter
            
            at  = CPUTime()
            at0 = RealTime()
            arealt = RealTime()

            CALL Info( 'NumaHeatSolve', ' ', Level=4 )
            CALL Info( 'NumaHeatSolve', '-------------------------------------',Level=4 )
            WRITE( Message,* ) 'TEMPERATURE non-linear iteration', iter
            CALL Info( 'NumaHeatSolve', Message, Level=4 )
            CALL Info( 'NumaHeatSolve', '-------------------------------------',Level=4 )
            CALL Info( 'NumaHeatSolve', ' ', Level=4 )
            CALL Info( 'NumaHeatSolve', 'Starting Assembly...', Level=4 )           
            !------------------------------------------------------------------------------
            CALL DefaultInitialize()
            !------------------------------------------------------------------------------
            body_id = -1
            Integ_Force = 0.0
            TotalPower = 0.0

            LoadVar => VariableGet(Solver % Mesh % Variables, 'Heat Source Calc')
            LoadVar % Values = 0._dp
            !------------------------------------------------------------------------------
            !  Go through bulk elements
            !------------------------------------------------------------------------------
            DO t=1,Solver % NumberOfActiveElements
            !------------------------------------------------------------------------------             
                IF ( RealTime() - at0 > 1.0 ) THEN
                    WRITE(Message,'(a,i3,a)' ) '   Assembly: ', INT(100.0 - 100.0 * &
                        (Solver % NumberOfActiveElements-t) / &
                        (1.0*Solver % NumberOfActiveElements)), ' % done'          
                    CALL Info( 'NumaHeatSolve', Message, Level=5 )
                    at0 = RealTime()
                END IF
                !------------------------------------------------------------------------------
                ! Check if this element belongs to a body where temperature 
                ! should be calculated
                !------------------------------------------------------------------------------
                Element => GetActiveElement(t)
                Material => GetMaterial()
                DO k_dof = 1,TDOFs
            !------------------------------------------------------------------------------ 
            !       Specified heat capacity after death
            !------------------------------------------------------------------------------                 
                    DeathCapacity(k_dof) = GetConstReal( Material, &
                        'Death Heat Capacity', Found )
                    IF ( .NOT.Found ) THEN
                        IF (k_dof==1) DeathCapacity(k_dof)= 4180.0
                        IF (k_dof==2) DeathCapacity(k_dof)= 670.0
                    END IF
            !------------------------------------------------------------------------------         
                END DO
                !------------------------------------------------------------------------------
                ! Get the element nodes characteristics
                !------------------------------------------------------------------------------
                n = GetElementNOFNodes()
                CALL GetElementNodes( ElementNodes )            
                !------------------------------------------------------------------------------
                ! Get element material parameters from the input file
                !------------------------------------------------------------------------------
                HeatCapacity = 1.0d0
                HeatConductivity = 0.0d0
                Density = 1.0d0
                InterMaterialCoeff = 0.0D0
                VolumeFraction = 0.0D0
                HeatCapacityConvect = 1.0D0
                PerfusionCoeff = 0.0D0         
                !------------------------------------------------------------------------------
                DO k_dof = 1, TDOFs     
                    !------------------------------------------------------------------------------
                    ! Volume fraction: Vi/V
                    !------------------------------------------------------------------------------
                    VolumeFraction(k_dof,1:n) = GetReal( Material, &
                        'Volume Fraction', Found )
                    IF (.NOT. Found) THEN
                        VolumeFraction(1,1:n)  = 0.1d0
                        IF (TDOFs==2) VolumeFraction(2,1:n)  = 0.9d0
                    END IF
                    !------------------------------------------------------------------------------
                    ! Intermaterial Transfer Coefficient: hA
                    !------------------------------------------------------------------------------
                    InterMaterialCoeff(k_dof,1:n) = GetReal( Material, &
                        'Convective Transfer Coefficient', Found )
                    IF (.NOT. Found) InterMaterialCoeff(k_dof,1:n) = 24.4
                    !------------------------------------------------------------------------------
                    ! Special treament of InterMaterialCoeff for VolumeFraction = 0 or 1 (in these
                    ! cases only 1 material, so no convective transfer)
                    ! Has to be modified if TDOFs > 2!
                    !------------------------------------------------------------------------------ 
                    DO i=1,n
                        IF ((VolumeFraction(k_dof,i) == 0.0) .OR. (VolumeFraction(k_dof,i) == 1.0) ) THEN
                            InterMaterialCoeff(k_dof,i) = 0.0
                        ELSE
                            InterMaterialCoeff(k_dof,i) = InterMaterialCoeff(k_dof,i) / VolumeFraction(k_dof,i)
                        END IF
                    END DO
                    !------------------------------------------------------------------------------
                    ! Heat capacity: c 
                    !------------------------------------------------------------------------------
                    HeatCapacity(k_dof,1:n) = GetReal( Material, &
                        'Heat Capacity', Found )
                    IF ( .NOT.Found ) THEN
                        IF (k_dof==1) HeatCapacity(k_dof,1:n)= 4180.0
                        IF (k_dof==2) HeatCapacity(k_dof,1:n)= 3600.0
                    END IF
                    !------------------------------------------------------------------------------
                    ! Heat Conductivity: k (might be a tensor)
                    !------------------------------------------------------------------------------
                    CALL ListGetRealArray( Material, &
                        'Heat Conductivity',Hwrk,n, Element % NodeIndexes, Found )
                    IF ( .NOT. Found ) THEN 
                        IF (ASSOCIATED(Hwrk)) DEALLOCATE( Hwrk )
                        ALLOCATE(Hwrk(1,1,n))
                        Hwrk = 0.5121
                    END IF
                    !------------------------------------------------------------------------------  
                    ! Distinguish cases in fonction of the dimension of the entry in the input file
                    ! In any case HeatConductivity will be a tensor 
                    !------------------------------------------------------------------------------ 
                    ! Case entry = scalar   
                    !------------------------------------------------------------------------------
                    IF ( SIZE(Hwrk,1) == 1 ) THEN
                        DO i=1,3
                            HeatConductivity( k_dof,i,i,1:n ) = Hwrk( 1,1,1:n ) 
                        END DO
                    !------------------------------------------------------------------------------ 
                    ! Case entry = vector   
                    !------------------------------------------------------------------------------
                    ELSE IF ( SIZE(Hwrk,2) == 1 ) THEN
                        DO i=1,MIN(3,SIZE(Hwrk,1))
                            HeatConductivity(k_dof,i,i,1:n) = Hwrk(i,1,1:n)
                            HeatConductivity(k_dof,i,i,1:n) = Hwrk(i,1,1:n) 
                        END DO
                    !------------------------------------------------------------------------------ 
                    ! Case entry = tensor   
                    !------------------------------------------------------------------------------
                    ELSE
                        DO i=1,MIN(3,SIZE(Hwrk,1))
                            DO j=1,MIN(3,SIZE(Hwrk,2))
                                HeatConductivity( k_dof,i,j,1:n ) = Hwrk(i,j,1:n)
                                HeatConductivity( k_dof,i,j,1:n ) = Hwrk(i,j,1:n) 
                            END DO
                        END DO
                    END IF
                    IF (ASSOCIATED(Hwrk)) DEALLOCATE( Hwrk )
                    !------------------------------------------------------------------------------
                    ! Density: rho
                    !------------------------------------------------------------------------------
                    Density(k_dof,1:n) = GetReal( Material, 'Density',Found )
                    IF (.NOT. Found) Density(k_dof,1:n) = 1060
                    !------------------------------------------------------------------------------
                    ! State of cells
                    !------------------------------------------------------------------------------
                    IF ( ASSOCIATED( CellStateSol ) ) THEN
                        DO i=1,n
                            k = CellStatePerm(Element % NodeIndexes(i))
                            LocalCellState(1,i) = CellState((k-1)*ADOFs + 1)
                            LocalCellState(2,i) = CellState((k-1)*ADOFs + 2)
                        END DO
                    ELSE
                        IF (CellStateModel==1) THEN
                            LocalCellState(1,1:n) = 1.0D0
                            LocalCellState(2,1:n) = 1.0D0
                        ELSE
                            LocalCellState(1,1:n) = 1.0D0
                            LocalCellState(2,1:n) = 0.0D0
                        END IF
                    ENDIF
                    !------------------------------------------------------------------------------
                END DO ! k_dof
                !------------------------------------------------------------------------------
                ! Get global Blood volume fraction from local variable for visualization
                !------------------------------------------------------------------------------
                IF (VarVolumeFraction) THEN
                    DO i=1,n
                        k=TempPerm(Element % NodeIndexes(i))
                        VolF(k)=VolumeFraction(1,i)
                    END DO
                END IF
                !------------------------------------------------------------------------------
                !  Check for convection model and get the convection velocities
                !  C1 = 0 : no convection
                !   C1 = 1 : convection in at least one direction
                !------------------------------------------------------------------------------
                C1 = 0.0D0
                U = 0.0D0
                V = 0.0D0
                W = 0.0D0
                !------------------------------------------------------------------------------
                DO k_dof = 1, TDOFs
                !------------------------------------------------------------------------------
                ! Read the velocity field in the input file 
                !------------------------------------------------------------------------------
                    U(k_dof,1:n) = GetReal( Material, &
                        'Convection Velocity 1', Found )
                    V(k_dof,1:n) = GetReal( Material, &
                        'Convection Velocity 2', Found )
                    W(k_dof,1:n) = GetReal( Material, &
                        'Convection Velocity 3', Found )    
                
                    !------------------------------------------------------------------------------
                    !Scale the velocity size to avoid unstabilities if specified:
                    !------------------------------------------------------------------------------
                    IF (ControlVelocitySize) THEN
                        DO i=1,n
                            IF(ABS(U(k_dof,i))>MaxVelocitySize) U(k_dof,i)=U(k_dof,i)*MaxVelocitySize/ABS(U(k_dof,i))
                            IF(ABS(V(k_dof,i))>MaxVelocitySize) V(k_dof,i)=V(k_dof,i)*MaxVelocitySize/ABS(V(k_dof,i))
                            IF(ABS(W(k_dof,i))>MaxVelocitySize) W(k_dof,i)=W(k_dof,i)*MaxVelocitySize/ABS(W(k_dof,i))
                        END DO
                    !------------------------------------------------------------------------------
                    END IF
                    !------------------------------------------------------------------------------
                    ! Update C1
                    !------------------------------------------------------------------------------
                    DO i=1,n
                        IF ( (U(k_dof,i) /= 0.0D0) .OR. (V(k_dof,i) /= 0.0D0) .OR. (W(k_dof,i) /= 0.0D0)) THEN 
                            C1(k_dof,i) = 1.0
                        END IF
                    END DO
                    
                END DO ! k_dof  
                !------------------------------------------------------------------------------
                !  No more convection in died cells...
                !------------------------------------------------------------------------------
                IF (CellsDeath) THEN
                !------------------------------------------------------------------------------
                    DO i=1,n
                        DO k_dof=1,TDOFs
                            !------------------------------------------------------------------------------
                            !  Adapt some parameters in function of the alive state of the cells
                            !------------------------------------------------------------------------------
                            IF (CellStateModel==1) THEN
                                IF (LocalCellState(k_dof,i) == 0) THEN
                                    C1(k_dof,i) = 0.0
                                    U(k_dof,i) = 0.0
                                    V(k_dof,i) = 0.0
                                    W(k_dof,i) = 0.0
                                END IF
                            ELSE
                                IF (LocalCellState(2,i) > DeadThreshold) THEN
                                    C1(k_dof,i) = 0.0
                                    U(k_dof,i) = 0.0
                                    V(k_dof,i) = 0.0
                                    W(k_dof,i) = 0.0
                                END IF
                            END IF  
                        END DO  
                    END DO
                    !------------------------------------------------------------------------------
                    !   Death Heat Capacity in died cells
                    !------------------------------------------------------------------------------
                    DO i = 1,n
                        DO k_dof=1,TDOFs
                            !------------------------------------------------------------------------------
                            !  Check the alive state of the cells
                            !------------------------------------------------------------------------------
                            IF (CellStateModel==1) THEN
                                IF ((LocalCellState(k_dof,i) == 0) .AND. (DeathCapacity( k_dof )/=0)) THEN
                                    HeatCapacity( k_dof,i ) = DeathCapacity(k_dof) 
                                END IF
                            ELSE
                                IF ((LocalCellState(2,i) > DeadThreshold) .AND. (DeathCapacity( k_dof )/=0)) THEN
                                    HeatCapacity( k_dof,i ) = DeathCapacity(k_dof) 
                                END IF
                            END IF
                        END DO
                    END DO  
                !------------------------------------------------------------------------------
                END IF ! Cells Death modelling
                !------------------------------------------------------------------------------
                ! Compute convection coefficient = c * rho * C1
                !------------------------------------------------------------------------------       
                DO i=1,n
                    DO k_dof = 1, TDOFs
                        HeatCapacityConvect(k_dof,i) =  Density(k_dof,i) * & 
                        C1(k_dof,i) * HeatCapacity(k_dof,i) 
                    END DO ! k_dof
                END DO
                !------------------------------------------------------------------------------
                ! Compute time derivative coefficient = c * rho 
                !------------------------------------------------------------------------------
                DO i=1,n
                    DO k_dof = 1, TDOFs
                        HeatCapacity(k_dof,i) = Density(k_dof,i) * HeatCapacity(k_dof,i)
                    END DO ! k_dof
                END DO


                !------------------------------------------------------------------------------
                ! Get perfusion coefficient for classical bio-heat eq with perfusion
                !------------------------------------------------------------------------------
                PerfusionCoeff(1:n) = GetReal( Material, &
                    'Tissue Perfusion Coefficient', Found )
                !------------------------------------------------------------------------------
                ! If classical bio-heat eq with perfusion, no porous flow and no heat transfer 
                ! between blood and tissue. No blood equation.
                !------------------------------------------------------------------------------
                IF (ANY(PerfusionCoeff/=0.0D0)) THEN
                    C1 = 0.0D0
                    U = 0.0D0
                    V = 0.0D0
                    W = 0.0D0
                    InterMaterialCoeff = 0.0D0
                    VolumeFraction(1,1:n) = 0.0D0
                    VolumeFraction(2,1:n) = 1.0D0
                END IF
                !------------------------------------------------------------------------------
                ! Perfusion stops when cell dies 
                !------------------------------------------------------------------------------
                IF (CellsDeath) THEN
                !------------------------------------------------------------------------------
                    DO i=1,n
                        IF (CellStateModel==1) THEN
                            IF (LocalCellState(2,i) == 0) THEN
                                PerfusionCoeff(i) = 0.0D0
                            END IF
                        ELSE
                            IF (LocalCellState(2,i) > DeadThreshold) THEN
                                PerfusionCoeff(i) = 0.0D0
                            END IF
                        END IF      
                    END DO
                !------------------------------------------------------------------------------
                END IF
                !------------------------------------------------------------------------------
                ! Multiply perfusion coeff by blood density and heat capacity (HeatCapacity(1,i)=product already)
                !------------------------------------------------------------------------------
                DO i=1,n
                    PerfusionCoeff(i) = PerfusionCoeff(i) * HeatCapacity(1,i)  
                END DO
                !------------------------------------------------------------------------------
                ! Add body forces, if any
                !------------------------------------------------------------------------------ 
                LOAD = 0.0D0
                power = 0.0D0         
                !------------------------------------------------------------------------------
                BodyForce => GetBodyForce()
                IF ( ASSOCIATED( BodyForce ) ) THEN
                    bf_id = GetBodyForceId()
                    !------------------------------------------------------------------------------
                    ! Given heat source = Vi/V * f
                    !------------------------------------------------------------------------------
                    DO k_dof=1,TDOFs
                        !------------------------------------------------------------------------------
                        IF(TemperatureControlledPower) THEN
                        !------------------------------------------------------------------------------
                        !   If temperature-controlled power, get the power from computation: 
                        !------------------------------------------------------------------------------
                            DO i = 1,n
                            !------------------------------------------------------------------------------
                                k = TempPerm(Element % NodeIndexes(i))
                                power(k_dof,i) = PreviousPower((k-1)*TDOFs + k_dof) + &
                                    PowerControl_Kp * (TargetTemperature-Temperature((k-1)*TDOFs + k_dof)) + & 
                                    PowerControl_Ki * IntegErrorToTargetTemperature((k-1)*TDOFs + k_dof) + & 
                                    PowerControl_Kd * DerivErrorToTargetTemperature((k-1)*TDOFs + k_dof)
                                !------------------------------------------------------------------------------
                                ! Control of max and min power: 
                                !------------------------------------------------------------------------------
                                power(k_dof,i) = MIN(power(k_dof,i),100000000.0)
                                power(k_dof,i) = MAX(power(k_dof,i),0.0)
                                !------------------------------------------------------------------------------
                                CurrentPower((k-1)*TDOFs + k_dof) = power(k_dof,i)
                                !------------------------------------------------------------------------------
                                ! If T>373K, conductivity=0, modelised by power=0 in heat equation: 
                                !------------------------------------------------------------------------------
                                IF ((ElectricPowerCutOff) .AND. (Temperature((k-1)*TDOFs + k_dof)>373)) THEN
                                    power(k_dof,i) = 0.0
                                END IF
                            !------------------------------------------------------------------------------ 
                            END DO !i
                        !------------------------------------------------------------------------------
                        ELSE
                            !------------------------------------------------------------------------------
                            ! Read the electric power used as a coefficient of heat source: 
                            !------------------------------------------------------------------------------
                            power(k_dof,1:n) = power(k_dof,1:n) + & 
                                GetReal( BodyForce, 'Electric Power', Found )
                            !------------------------------------------------------------------------------
                            ! If T>373K, conductivity=0, modelised by power=0: 
                            !------------------------------------------------------------------------------
                            IF (ElectricPowerCutOff) THEN
                                DO i = 1,n
                                    k = TempPerm(Element % NodeIndexes(i))
                                    IF (Temperature((k-1)*TDOFs + k_dof)>373) THEN
                                        power(k_dof,i) = 0.0
                                    END IF
                                END DO
                            END IF
                        !------------------------------------------------------------------------------
                        END IF
                        !------------------------------------------------------------------------------
                        ! Read the body force value in the input file and modify Load
                        !------------------------------------------------------------------------------
                        IF (InterpolatedElectricPower) THEN
                            DO i = 1,n
                                k = TempPerm(Element % NodeIndexes(i))
                                LOAD(k_dof,i) = LOAD(k_dof,i) + power(k_dof,i) * ElectricPower((k-1)*TDOFs + k_dof)
                            END DO
                        ELSEIF (UseNormedJH) THEN
                            DO i = 1,n
                                k = JouleHeatingPerm(Element % NodeIndexes(i))
                                LOAD(k_dof,i) = LOAD(k_dof,i) + power(k_dof,i) * JouleHeating((k-1)*JHDOFs + k_dof)
                            END DO
                        ELSE
                            WRITE(char_k_dof,*) k_dof
                            char_k_dof = ADJUSTL(char_k_dof)    
                            
                            LOAD(k_dof,1:n) = LOAD(k_dof,1:n) + power(k_dof,1:n) * & 
                                GetReal( BodyForce, 'Heat Source', Found ) 
                            IF (.not. Found) THEN
                                LOAD(k_dof,1:n) = LOAD(k_dof,1:n) + power(k_dof,1:n) * & 
                                    GetReal( BodyForce, 'Heat Source '//TRIM(char_k_dof), Found ) 
                            END IF
                DO i=1,n
                  k = LoadVar % Perm(Element % NodeIndexes(i))
                  IF ( k > 0 ) THEN
!----------     --------------------------------------------------------------------
                    LoadVar % Values(k) = power(1, i)
                  END IF
                END DO
                        END IF
                    !------------------------------------------------------------------------------ 
                    END DO
                !------------------------------------------------------------------------------
                END IF ! body force
                !------------------------------------------------------------------------------
                ! Integrate power over element
                !------------------------------------------------------------------------------
                CALL TotalPowerCompose( LOAD, Element, n, ElementNodes,TDOFs ) 
                !------------------------------------------------------------------------------
                ! Add one part of the perfusion term: rho(b)*cp(b)*T(body)
                !------------------------------------------------------------------------------     
                LOAD(1,1:n) = LOAD(1,1:n) + PerfusionCoeff(1:n) * BodyTemperature
                IF (t==50000) THEN
                PRINT *, "$$$", PerfusionCoeff(1:n) * BodyTemperature
                PRINT *, "$$$", PerfusionCoeff(1:n), BodyTemperature
                END IF

                !------------------------------------------------------------------------------
                ! Get element local matrices, and RHS vectors
                !------------------------------------------------------------------------------
                !------------------------------------------------------------------------------         
                CALL TemperatureCompose( &
                    LocalMASS, LocalSTIFF, LocalFORCE, LOAD, InterMaterialCoeff, &
                    HeatCapacity, C0, HeatCapacityConvect, HeatConductivity, &
                    U, V, W, Density, PerfusionCoeff, Stabilize, UseBubbles, Element, n, ElementNodes,TDOFs )        
                !------------------------------------------------------------------------------
                ! If time dependent simulation, add mass matrix to stiff matrix
                !------------------------------------------------------------------------------
                IF ( TransientSimulation ) THEN
                        CALL Add1stOrderTime( LocalMASS, LocalSTIFF, LocalFORCE, dt, n, TDOFs, &
                            TempPerm(Element % NodeIndexes(1:n)), Solver )   
                END IF 

                !BUBBLES:
                IF (UseBubbles) THEN
                    TimeForce  = 0.0
                    CALL Condensate( n*TDOFs, LocalSTIFF, LocalFORCE, TimeForce )
                END IF
  
                !------------------------------------------------------------------------------
                ! Update global matrices from local matrices
                !------------------------------------------------------------------------------
                CALL UpdateGlobalEquations( StiffMatrix, LocalSTIFF, &
                forcevector, LocalFORCE, n, TDOFs, TempPerm(Element % NodeIndexes) )     
            !------------------------------------------------------------------------------
            END DO     !  Bulk elements

            PRINT *,'TotalPower(k_dof)=',TotalPower
            PRINT *,'Integ_Force(k_dof)=',Integ_Force 
            !------------------------------------------------------------------------------
1000  CONTINUE

            !------------------------------------------------------------------------------
            ! Neumann boundary conditions
            !------------------------------------------------------------------------------

            !------------------------------------------------------------------------------
            ! Go through boundary elements
            !------------------------------------------------------------------------------
            DO t=1, Solver % Mesh % NumberOfBoundaryElements
            !------------------------------------------------------------------------------
                Element => GetBoundaryElement(t)
                !------------------------------------------------------------------------------
                ! Check if the element is active and of suitable type
                !------------------------------------------------------------------------------
                IF ( .NOT. ActiveBoundaryElement() ) CYCLE
                IF ( GetElementFamily() == 1 ) CYCLE
                !------------------------------------------------------------------------------
                ! Get the number of nodes 
                !------------------------------------------------------------------------------
                n = GetElementNOFNodes()
                !------------------------------------------------------------------------------
                ! Get the BC associated to the element
                !------------------------------------------------------------------------------
                BC => GetBC()
                !------------------------------------------------------------------------------
                ! Check that it is a Neumann condition
                !------------------------------------------------------------------------------
                IF ( GetLogical( BC, 'Heat Flux BC',Found) ) THEN
                    !------------------------------------------------------------------------------
                    ! Get the nodes characteristics
                    !------------------------------------------------------------------------------
                    CALL GetElementNodes( ElementNodes )
                    !------------------------------------------------------------------------------
                    ! Get physical parameters from input file
                    !------------------------------------------------------------------------------
                    HeatTransferCoeff = 0.0D0
                    LOAD  = 0.0D0
                    Work = 0.0D0
                    AText = 0.0D0
                    !------------------------------------------------------------------------------
                    ! Convective transfer coefficient (BC)
                    !------------------------------------------------------------------------------
                    HeatTransferArray => ListGetConstRealArray( BC, 'Heat Transfer Coefficient', Found )
                    IF (Found) THEN 
                        DO k_dof=1,TDOFS
                            Work(k_dof,1:n) =  HeatTransferArray(k_dof,1)
                        END DO ! k_dof
                    ELSE
                        DO k_dof=1,TDOFS
                            Work(k_dof,1:n) =  0.0
                        END DO ! k_dof
                    END IF
                    !------------------------------------------------------------------------------
                    ! Heat gap for convective transfer (BC)
                    !------------------------------------------------------------------------------             
                    IF ( GetLogical( BC, 'Heat Gap', Found ) ) THEN
                        IF ( .NOT. GetLogical( BC, 'Heat Gap Implicit', Found ) ) THEN
                            DO k_dof=1,TDOFS
                                AText(k_dof,1:n) = GapTemperature( Solver, Element, Temperature, TempPerm, n)
                            END DO
                        END IF
                    ELSE
                        ATextArray => ListGetConstRealArray( BC, 'External Temperature', Found )
                        IF (Found) THEN
                            DO k_dof=1,TDOFS
                                IF ( ANY(Work(k_dof,1:n) /= 0.0d0) ) THEN
                                    AText(k_dof,1:n) = ATextArray(k_dof,1)
                                END IF
                            END DO
                        ELSE 
                            DO k_dof=1,TDOFS
                                IF ( ANY(Work(k_dof,1:n) /= 0.0d0) ) THEN
                                    AText(k_dof,1:n) = 310.0
                                END IF
                            END DO
                        END IF
                    END IF  
                    !------------------------------------------------------------------------------
                    ! Transfer BC: -k@T/@n = \alpha(T - Text)
                    !------------------------------------------------------------------------------
                    DO k_dof=1,TDOFS
                        DO j=1,n
                            LOAD(k_dof,j) = LOAD(k_dof,j) + Work(k_dof,j) * AText(k_dof,j)
                            HeatTransferCoeff(k_dof,j) = HeatTransferCoeff(k_dof,j) + Work(k_dof,j)
                        END DO
                    END DO
                    !------------------------------------------------------------------------------
                    ! Flux BC: k@T/@n = g
                    !------------------------------------------------------------------------------ 
                    HeatFluxArray => ListGetConstRealArray( BC, 'Heat Flux', Found )
                    IF (Found) THEN
                        DO k_dof=1,TDOFS
                            LOAD(k_dof,1:n) = LOAD(k_dof,1:n) +  HeatFluxArray(k_dof,1)
                        END DO ! k_dof
                    END IF
                    !------------------------------------------------------------------------------
                    ! Get element matrix and rhs due to boundary conditions 
                    !------------------------------------------------------------------------------
                    CALL TemperatureBoundary( LocalSTIFF,LocalFORCE, &
                        LOAD,HeatTransferCoeff,Element,n,ElementNodes,TDOFs )
                    !------------------------------------------------------------------------------
                    ! If time dependent simulation, add the time derivative coefficients 
                    ! terms to stiff matrix
                    !------------------------------------------------------------------------------
                    IF ( TransientSimulation ) THEN
                        LocalMASS = 0.d0
                        CALL Add1stOrderTime( LocalMASS, LocalSTIFF, LocalFORCE, dt, n, TDOFs, &
                            TempPerm(Element % NodeIndexes(1:n)), Solver )  
                    END IF
                    !------------------------------------------------------------------------------
                    ! Add heat gap in the stiffmatrix (if implicit treatment)
                    !------------------------------------------------------------------------------
                    IF ( GetLogical( BC, 'Heat Gap', Found ) ) THEN
                        IF ( GetLogical( BC, 'Heat Gap Implicit', Found ) ) &
                            CALL AddHeatGap( Solver, Element, LocalSTIFF, TempPerm)
                    END IF
                    !------------------------------------------------------------------------------
                    ! Update global matrices from local matrices
                    !------------------------------------------------------------------------------         
                    CALL UpdateGlobalEquations( StiffMatrix, LocalSTIFF, &
                        forcevector, LocalFORCE, n, TDOFs, TempPerm(Element % NodeIndexes) )
                !------------------------------------------------------------------------------
                ELSE
                !------------------------------------------------------------------------------
                ! Maybe a flux BC has to be a pplied as a modification of Diriclet BC:
                !------------------------------------------------------------------------------
                    IF (ModifDBC .AND. ModifDBCToFluxBC) THEN   
                    !------------------------------------------------------------------------------
                    ! If a dirichlet BC is locally modified to flux BC, check if element selected:
                    !------------------------------------------------------------------------------
                            ModifDBC_Select_Element = .FALSE.
                            !------------------------------------------------------------------------------
                            !   Selection test for locally modified boundary condition:
                            !------------------------------------------------------------------------------
                            ! Get the nodes characteristics
                            !------------------------------------------------------------------------------
                            CALL GetElementNodes( ElementNodes )
                            !------------------------------------------------------------------------------
                            !   Compute the maximum distance from the nodes to the target:
                            !------------------------------------------------------------------------------
                            ModifDBC_Dist = sqrt( (ElementNodes % x(1)-ModifDBC_xc)**2 + &
                                (ElementNodes % y(1)-ModifDBC_yc)**2 + &
                                (ElementNodes % z(1)-ModifDBC_zc)**2 )
                
                            DO i=2,n
                                ModifDBC_Dist = Max (ModifDBC_Dist, sqrt( (ElementNodes % x(i)-ModifDBC_xc)**2 + &
                                    (ElementNodes % y(i)-ModifDBC_yc)**2 + &
                                    (ElementNodes % z(i)-ModifDBC_zc)**2 ) )
                            END DO
                            !------------------------------------------------------------------------------
                            !   Check the condition on distance from the target and time:
                            !------------------------------------------------------------------------------
                            IF ( ((ModifDBC_Dist<ModifDBC_DistMax) .OR. &
                                   (ModifDBC_DistMax<0.0)) .AND. (Time>=ModifDBC_TimeStart) ) THEN
                                !------------------------------------------------------------------------------
                                !   Compute the diameter of the element:
                                !------------------------------------------------------------------------------                             
                                Diameter = ElementDiameter( Element, ElementNodes )
                                !------------------------------------------------------------------------------
                                !   Check the condition on the diameter of the element:
                                !------------------------------------------------------------------------------
                                IF ( (Diameter<ModifDBC_DiamMax) .OR. (ModifDBC_DiamMax<0.0) ) THEN
                                !------------------------------------------------------------------------------
                                    ModifDBC_Select_Element = .TRUE.
                                !------------------------------------------------------------------------------
                                END IF
                                !------------------------------------------------------------------------------
                            END IF
                            !------------------------------------------------------------------------------
                            !   If selected element than apply Flux BC:
                            !------------------------------------------------------------------------------
                            IF (ModifDBC_Select_Element) THEN
                                !------------------------------------------------------------------------------
                                ! Check that a flux condition is defined:
                                !------------------------------------------------------------------------------
                                IF ( GetLogical( Model % BCs(ModifDBC_Index) % Values, 'Heat Flux BC',Found) ) THEN
                                    !------------------------------------------------------------------------------
                                    ! Get physical parameters from input file
                                    !------------------------------------------------------------------------------
                                    HeatTransferCoeff = 0.0D0
                                    LOAD  = 0.0D0
                                    Work = 0.0D0
                                    AText = 0.0D0
                                    !------------------------------------------------------------------------------
                                    ! Convective transfer coefficient (BC)
                                    !------------------------------------------------------------------------------
                                    HeatTransferArray => ListGetConstRealArray( Model % BCs(ModifDBC_Index) % Values, &
                                        'Heat Transfer Coefficient', Found )
                                        IF (Found) THEN 
                                            DO k_dof=1,TDOFS
                                                Work(k_dof,1:n) =  HeatTransferArray(k_dof,1)
                                            END DO ! k_dof
                                        ELSE
                                            DO k_dof=1,TDOFS
                                                Work(k_dof,1:n) =  0.0
                                            END DO ! k_dof
                                        END IF
                                    !------------------------------------------------------------------------------
                                    ! Heat gap for convective transfer (BC)
                                    !------------------------------------------------------------------------------             
                                    IF ( GetLogical( Model % BCs(ModifDBC_Index) % Values, 'Heat Gap', Found ) ) THEN
                                            IF ( .NOT. GetLogical( Model % BCs(ModifDBC_Index) % Values, &
                                                'Heat Gap Implicit', Found ) ) THEN
                                                DO k_dof=1,TDOFS
                                                    AText(k_dof,1:n) = GapTemperature( Solver, Element, Temperature, TempPerm, n)
                                                END DO
                                            END IF
                                    ELSE
                                        ATextArray => ListGetConstRealArray( Model % BCs(ModifDBC_Index) % Values, &
                                            'External Temperature', Found )
                                        IF (Found) THEN
                                            DO k_dof=1,TDOFS
                                                IF ( ANY(Work(k_dof,1:n) /= 0.0d0) ) THEN
                                                    AText(k_dof,1:n) = ATextArray(k_dof,1)
                                                END IF
                                            END DO
                                        ELSE 
                                            DO k_dof=1,TDOFS
                                                IF ( ANY(Work(k_dof,1:n) /= 0.0d0) ) THEN
                                                    AText(k_dof,1:n) = 310.0
                                                END IF
                                            END DO
                                        END IF
                                    END IF  
                                    !------------------------------------------------------------------------------
                                    ! Transfer BC: -k@T/@n = \alpha(T - Text)
                                    !------------------------------------------------------------------------------
                                    DO k_dof=1,TDOFS    
                                        DO j=1,n
                                            LOAD(k_dof,j) = LOAD(k_dof,j) + Work(k_dof,j) * AText(k_dof,j)
                                            HeatTransferCoeff(k_dof,j) = HeatTransferCoeff(k_dof,j) + Work(k_dof,j)
                                        END DO
                                    END DO
                                    !------------------------------------------------------------------------------
                                    ! Flux BC: k@T/@n = g
                                    !------------------------------------------------------------------------------ 
                                    HeatFluxArray => ListGetConstRealArray( Model % BCs(ModifDBC_Index) % Values, &
                                        'Heat Flux', Found )
                                    IF (Found) THEN
                                        DO k_dof=1,TDOFS
                                            LOAD(k_dof,1:n) = LOAD(k_dof,1:n) +  HeatFluxArray(k_dof,1)
                                        END DO ! k_dof
                                    END IF
                                    !------------------------------------------------------------------------------
                                    ! Get element matrix and rhs due to boundary conditions 
                                    !------------------------------------------------------------------------------
                                    CALL TemperatureBoundary( LocalSTIFF,LocalFORCE, &
                                        LOAD,HeatTransferCoeff,Element,n,ElementNodes,TDOFs )               
                                    !------------------------------------------------------------------------------
                                    ! If time dependent simulation, add the time derivative coefficients 
                                    ! terms to stiff matrix
                                    !------------------------------------------------------------------------------
                                    IF ( TransientSimulation ) THEN
                                        LocalMASS = 0.d0
                                        CALL Add1stOrderTime( LocalMASS, LocalSTIFF, LocalFORCE, dt, n, TDOFs, &
                                            TempPerm(Element % NodeIndexes(1:n)), Solver )  
                                    END IF
                                    !------------------------------------------------------------------------------
                                    ! Add heat gap in the stiffmatrix (if implicit treatment)
                                    !------------------------------------------------------------------------------
                                    IF ( GetLogical( Model % BCs(ModifDBC_Index) % Values, 'Heat Gap', Found ) ) THEN
                                        IF ( GetLogical( Model % BCs(ModifDBC_Index) % Values, 'Heat Gap Implicit', Found ) ) &
                                            CALL AddHeatGap( Solver, Element, LocalSTIFF, TempPerm)
                                    END IF
                                    !------------------------------------------------------------------------------
                                    ! Update global matrices from local matrices
                                    !------------------------------------------------------------------------------         
                                    CALL UpdateGlobalEquations( StiffMatrix, LocalSTIFF, &
                                        forcevector, LocalFORCE, n, TDOFs, TempPerm(Element % NodeIndexes) )
                                !------------------------------------------------------------------------------ 
                                END IF ! Heat flux BC
                            !------------------------------------------------------------------------------
                            END IF ! ModifDBC_Select_Element
                    !------------------------------------------------------------------------------
                    END IF ! ModifDBCToFluxBC
                !------------------------------------------------------------------------------
                END IF ! of heat-flux bc
            !------------------------------------------------------------------------------     
            END DO   ! Boundary elements    
            !------------------------------------------------------------------------------
            CALL DefaultFinishAssembly()
            !------------------------------------------------------------------------------
            !  Dirichlet boundary conditions
            !------------------------------------------------------------------------------
            CALL NumaDefaultDirichletConditions(Solver)
            !------------------------------------------------------------------------------
            CALL Info( 'NumaHeatSolve', 'Assembly done', Level=4 )
            !------------------------------------------------------------------------------
            ! Compute assembly CPU time, save current CPU time for solving CPU time below, 
            ! and save current solution norm
            !------------------------------------------------------------------------------
            at = CPUTime() - at
            arealt = RealTime() -arealt
            st = CPUTime()
            srealt = RealTime()
            PrevNorm = Norm    
            !------------------------------------------------------------------------------
            !     Solve the system 
            !------------------------------------------------------------------------------
            CALL SolveSystem( StiffMatrix, ParMatrix, ForceVector, &
                Temperature, Solver % Variable % Norm, TDOFs, Solver )
            !------------------------------------------------------------------------------
            !   Enforce the computation of the norm
            !   (This computation is processor-local in parallel execution)
            !------------------------------------------------------------------------------
            Solver % Variable % Norm = SQRT( SUM( Temperature**2 ) / (TDOFs*LocalNodes) )
            Norm = Solver % Variable % Norm
            !------------------------------------------------------------------------------
            IF(ControlError) THEN
            !------------------------------------------------------------------------------         
            !   Compute L2 and H1 Errors between numerical and analytical temperature 
            !------------------------------------------------------------------------------   
                CALL ErrorCompute( Model, Temperature, TempPerm,TDOFs )
                CALL Info( 'NumaHeatSolve', 'Errors between numerical and analytical solutions:', Level=4 )
                PRINT *,'ErrorL2= ',ErrorL2
                PRINT *,'ErrorH1= ',ErrorH1
            !------------------------------------------------------------------------------         
            !   Compute L-infinite Error between numerical and analytical temperature 
            !------------------------------------------------------------------------------
                ErrorL0 = 0.0D0
                DO i=1,LocalNodes
                    DeltaTemperature(i)=ABS(Temperature(TempPerm(i))- &
                        AnalyticalSolution( model % Nodes % x(i), model % Nodes % y(i), model % Nodes % z(i)))  
                    IF (DeltaTemperature(i)>ErrorL0) THEN
                        ErrorL0 = DeltaTemperature(i)
                    END IF
                END DO
                PRINT *,'ErrorL0= ',ErrorL0
            !------------------------------------------------------------------------------         
            !   Compute L2 Error with Elmer method:
            !------------------------------------------------------------------------------
                ErrorL2 = 0.0D0
                DO i=1,LocalNodes
                    DeltaTemperature(i)=Temperature(TempPerm(i))- &
                        AnalyticalSolution( model % Nodes % x(i), model % Nodes % y(i), model % Nodes % z(i))
                    ErrorL2 = ErrorL2 + DeltaTemperature(i)**2
                END DO
                ErrorL2 = sqrt(ErrorL2 / LocalNodes)
                PRINT *,'ErrorL2 (Monte-Carlo method)= ',ErrorL2
            !------------------------------------------------------------------------------
            END IF
            !------------------------------------------------------------------------------         
            ! Compute Max temperature 
            !------------------------------------------------------------------------------         
            IF(ControlMaxTemperature) THEN
                MaxTemperature = 0.0d0
                DO i=1,LocalNodes
                    DO k_dof=1,TDOFs
                        IF (MaxTemperature(k_dof) < Temperature((i-1)*TDOFs+k_dof)) THEN
                            MaxTemperature(k_dof) = Temperature((i-1)*TDOFs+k_dof)
                        END IF
                    END DO  
                END DO
            END IF
            !------------------------------------------------------------------------------         
            ! Compute variables for temperature-controlled power:
            !------------------------------------------------------------------------------         
            IF(TemperatureControlledPower) THEN
            !------------------------------------------------------------------------------ 
                ! Save error between target and current temperature and update history:
                !------------------------------------------------------------------------------ 
                DO j=1,PowerControl_Integ_Length-1
                        HistoryErrorToTargetTemperature(PowerControl_Integ_Length-j+1,:) = &
                            HistoryErrorToTargetTemperature(PowerControl_Integ_Length-j,:)
                END DO
                HistoryErrorToTargetTemperature(1,:) = TargetTemperature-Temperature

                HistoryErrorToTargetTemperatureVariable => HistoryErrorToTargetTemperature(1,:)

                CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
                Solver, 'Error to target Temperature', 1, &
                HistoryErrorToTargetTemperatureVariable, TempPerm ) 
                !------------------------------------------------------------------------------ 
                ! Integral of error between target and current temperature:
                !------------------------------------------------------------------------------ 
                IF(PowerControl_Integ_Length/=0) THEN
                !------------------------------------------------------------------------------
                !If specified length of integral, use history variable
                !------------------------------------------------------------------------------
                    IntegErrorToTargetTemperature = 0.0D0
                    DO j=1,PowerControl_Integ_Length
                        IntegErrorToTargetTemperature(:) = IntegErrorToTargetTemperature(:) + &
                            HistoryErrorToTargetTemperature(j,:)*dt
                    END DO
                !------------------------------------------------------------------------------
                ELSE
                !------------------------------------------------------------------------------
                !Else, make the integral over all time-steps
                !------------------------------------------------------------------------------             
                    IntegErrorToTargetTemperature(:) = IntegErrorToTargetTemperature(:) + &
                            HistoryErrorToTargetTemperature(1,:)*dt
                !------------------------------------------------------------------------------
                END IF
                !------------------------------------------------------------------------------ 
                ! Derivative of error between target and current temperature:
                !------------------------------------------------------------------------------ 
                DerivErrorToTargetTemperature(:) = (HistoryErrorToTargetTemperature(1,:)- &
                    HistoryErrorToTargetTemperature(2,:))/dt
            !------------------------------------------------------------------------------ 
            END IF !TemperatureControlledPower
        !------------------------------------------------------------------------------ 
        !   Add the heat source as a variable for visualization
        !------------------------------------------------------------------------------ 
        VisualizeHeatSource = GetLogical( SolverParams,'Heat Source Visualization',Found )
        IF ( .NOT.Found ) VisualizeHeatSource = .FALSE.
        !------------------------------------------------------------------------------
        IF(VisualizeHeatSource) THEN
        !------------------------------------------------------------------------------
                HeatSource = 0.0D0
        !------------------------------------------------------------------------------ 
        !   Go through bulk elements and get heat source:
        !------------------------------------------------------------------------------
                DO t=1,Solver % NumberOfActiveElements
        !------------------------------------------------------------------------------
                    Element => GetActiveElement(t)
                    Material => GetMaterial()
                    n = GetElementNOFNodes()
                    CALL GetElementNodes( ElementNodes )    

                    BodyForce => GetBodyForce()
        !------------------------------------------------------------------------------
                    IF ( ASSOCIATED( BodyForce ) ) THEN
        !------------------------------------------------------------------------------ 
                        LocalHeatSource = 0.0D0
        !------------------------------------------------------------------------------
                        DO k_dof=1,TDOFs
                            !------------------------------------------------------------------------------
                            ! Read the body force value in the input file: 
                            !------------------------------------------------------------------------------
                            WRITE(char_k_dof,*) k_dof
                            char_k_dof = ADJUSTL(char_k_dof)    

                            LocalHeatSource(k_dof,1:n) = LocalHeatSource(k_dof,1:n) + &
                                    GetReal( BodyForce, 'Heat Source '//TRIM(char_k_dof), Found )
        !------------------------------------------------------------------------------
                            DO i=1,n
        !------------------------------------------------------------------------------
                                k = TempPerm(Element % NodeIndexes(i))
                                HeatSource((k-1)*TDOFs + k_dof) = LocalHeatSource(k_dof,i)
        !------------------------------------------------------------------------------
                            END DO ! i
        !------------------------------------------------------------------------------ 
                        END DO ! k_dof
        !------------------------------------------------------------------------------ 
                    END IF
        !------------------------------------------------------------------------------ 
                END DO ! t
        !------------------------------------------------------------------------------ 
                CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
                Solver, 'Heat source', 1, &
                HeatSource, TempPerm ) 
        !------------------------------------------------------------------------------ 
        END IF

        !------------------------------------------------------------------------------ 
        !   Add the heat source as a variable for visualization
        !------------------------------------------------------------------------------ 
        VisualizePerfusion = GetLogical( SolverParams,'Perfusion Visualization',Found )
        IF ( .NOT.Found ) VisualizePerfusion = .FALSE.
        !------------------------------------------------------------------------------
        IF(VisualizePerfusion) THEN
        !------------------------------------------------------------------------------
            Perfusion = 0.0D0
            !------------------------------------------------------------------------------ 
            ! Go through bulk elements and get Perfusion:
            !------------------------------------------------------------------------------
            DO t=1,Solver % NumberOfActiveElements
            !------------------------------------------------------------------------------
                Element => GetActiveElement(t)
                Material => GetMaterial()
                n = GetElementNOFNodes()
                CALL GetElementNodes( ElementNodes )    
                !------------------------------------------------------------------------------
                PerfusionCoeff(1:n) = GetReal( Material, &
                'Tissue Perfusion Coefficient', Found )
                IF ( .NOT.Found ) PerfusionCoeff=0.0D0
                !------------------------------------------------------------------------------
                DO i=1,n
                !------------------------------------------------------------------------------
                    Perfusion(TempPerm(Element % NodeIndexes(i))) = PerfusionCoeff(i)
                !------------------------------------------------------------------------------
                END DO ! i
            !------------------------------------------------------------------------------ 
            END DO ! t
            !------------------------------------------------------------------------------ 
            CALL VariableAdd( Solver % Mesh % Variables, Solver % Mesh, &
            Solver, 'Perfusion', 1, Perfusion, TempPerm ) 
        !------------------------------------------------------------------------------ 
        END IF !VisualizePerfusion
        !------------------------------------------------------------------------------      
        ! Write some information messages
        !------------------------------------------------------------------------------      
       !------------------------------------------------------------------------------
        ! Compute solving CPU time, and total assembly and solving CPU time in the 
        ! coupled system iteration (may contain several nonlinear iterations) 
        !------------------------------------------------------------------------------
        st = CPUTIme()-st
        srealt = RealTime()-srealt
        totat = totat + at
        totst = totst + st
        WRITE(Message,'(a,i4,a,F8.2,F8.2)') 'iter: ',iter,', Assembly CPU Time (nonlinear it., coupled it.): (s)', at, totat
        CALL Info( 'NumaHeatSolve', Message, Level=4 )
        WRITE(Message,'(a,i4,a,F8.2)') 'iter: ',iter,', Assembly Real Time (nonlinear it): (s)', arealt
        CALL Info( 'NumaHeatSolve', Message, Level=4 )
        WRITE(Message,'(a,i4,a,F8.2,F8.2)') 'iter: ',iter,', Solve CPU Time (nonlinear it., coupled it.):    (s)', st, totst
        CALL Info( 'NumaHeatSolve', Message, Level=4 )
        WRITE(Message,'(a,i4,a,F8.2)') 'iter: ',iter,', Solve Real Time (nonlinear it): (s)', srealt
        CALL Info( 'NumaHeatSolve', Message, Level=4 )
        !------------------------------------------------------------------------------
        ! Compute norm of the solution and check convergence criteria for nonlinear iterations
        !------------------------------------------------------------------------------
        IF ( PrevNorm + Norm /= 0.0d0 ) THEN
            RelativeChange = 2.0d0 * ABS( PrevNorm-Norm ) / (PrevNorm + Norm)
        ELSE
            RelativeChange = 0.0d0
        END IF

        WRITE( Message, * ) 'Result Norm   : ',Norm
        CALL Info( 'NumaHeatSolve', Message, Level=4 )
        WRITE( Message, * ) 'Relative Change : ',RelativeChange
        CALL Info( 'NumaHeatSolve', Message, Level=4 )
        !------------------------------------------------------------------------------
        ! If NonlinearTol is reached, stop the nonlinear iterations before maxiter: 
        ! In case of a parallel execution, communication with the other processus and
        ! stop of iterations only if for all processus convergence is reached
        !------------------------------------------------------------------------------
        ConvergenceDone = .FALSE.
        IF ( RelativeChange < NonlinearTol ) THEN
            ConvergenceDone = .TRUE.
        END IF
        !PRINT *,'proc= ',ParEnv % MyPe,'ConvergenceDone= ',ConvergenceDone
        IF(ParEnv % PEs>1) THEN
            CALL ParallelAllReduceAnd( ConvergenceDone )
            !PRINT *,'aftercomm, proc= ',ParEnv % MyPe,'ConvergenceDone= ',ConvergenceDone
        END IF
        IF( ConvergenceDone ) EXIT
        !------------------------------------------------------------------------------
        END DO ! of the nonlinear iteration
        !------------------------------------------------------------------------------ 
        ! Save the maximum number of non-linear iterations (for control)
        !------------------------------------------------------------------------------ 
        IF (ControlIterations) THEN
        
            IF (iter > NonLinearIter)  THEN
                CALL Info( 'NumaHeatSolve', 'Nonlinear Iterations done this timestep= ', Level=4 )
                PRINT *,iter-1
                IF (iter-1 > MaxNonLinearIterDone) THEN
                    MaxNonLinearIterDone = iter-1
                END IF
            ELSE 
                CALL Info( 'NumaHeatSolve', 'Nonlinear Iterations done this timestep= ', Level=4 )
                PRINT *,iter
                IF (iter > MaxNonLinearIterDone) THEN
                    MaxNonLinearIterDone = iter
                END IF
            END IF
            
            CALL Info( 'NumaHeatSolve', 'Max number of Nonlinear Iterations= ', Level=4 )
            PRINT *,MaxNonLinearIterDone
            
        END IF
        !------------------------------------------------------------------------------
        !  Check if convergence is reached towards specified tolerance >0
        !  If not, stop the simulation
        !------------------------------------------------------------------------------ 
        IF ( (NonlinearIterAbort) .AND. (NonlinearTol/=0) .AND. ( RelativeChange > NonlinearTol ) &
            ) THEN
            WRITE( Message, * ) 'No convergence reached during the ', NonlinearIter,' non-linear iterations '
            CALL Error( 'NumaHeatSolve', Message )
            CALL Fatal( 'NumaHeatSolve', ' ' )
        END IF
        !------------------------------------------------------------------------------
        !  Save the maximum number of coupling iterations (for control)
        !------------------------------------------------------------------------------ 
        IF (ControlIterations) THEN
            IF (PrevTime < Time) THEN 
                !------------------------------------------------------------------------------
                ! We are in the first coupled system sub-iteration:
                !------------------------------------------------------------------------------
                CouplingIter = 1
            ELSE
                CouplingIter = CouplingIter + 1
            END IF
            IF (CouplingIter > MaxCouplingIterDone) THEN
                MaxCouplingIterDone = CouplingIter
            END IF
            CALL Info( 'NumaHeatSolve', 'Coupling Iterations done this timestep= ', Level=4 )
            PRINT *, CouplingIter
            CALL Info( 'NumaHeatSolve', 'Max number of Coupling Iterations= ', Level=4 )
            PRINT *, MaxCouplingIterDone
        END IF
        !------------------------------------------------------------------------------         
        ! Save Time as Previous Time for next iteration
        !------------------------------------------------------------------------------
        PrevTime = TimeVar % Values(1)
        !------------------------------------------------------------------------------
        ! Compute cumulative time done by now and time remaining
        !------------------------------------------------------------------------------
        IF ( .NOT. TransientSimulation ) EXIT
        CumulativeTime = CumulativeTime + dt
        dt = Timestep - CumulativeTime
    !------------------------------------------------------------------------------   
    END DO ! time interval
   Solver % dt = Timestep
    !------------------------------------------------------------------------------
   CALL  ListAddConstReal( Solver % Values,  &
        'Nonlinear System Relaxation Factor', SaveRelax )
    !------------------------------------------------------------------------------
    ! Compute mesh average size
    !------------------------------------------------------------------------------        
   MeshSize = ComputeMeshSize(Solver)
   !------------------------------------------------------------------------------
    ! Refine mesh if required
    !------------------------------------------------------------------------------ 
    IF ( ListGetLogical( Solver % Values, 'Adaptive Mesh Refinement', Found ) ) &
        CALL NumaRefineMesh( Model,Solver,Temperature,TempPerm, &
            TemperatureInsideResidual, TemperatureEdgeResidual, TemperatureBoundaryResidual, TDOFs )
!------------------------------------------------------------------------------
CONTAINS


    
!------------------------------------------------------------------------------
    FUNCTION NumaDistanceToElectricTip (x, y, z, xtip, ytip, ztip, nbpoints, xproj, yproj, zproj ) RESULT( Distance )
!------------------------------------------------------------------------------ 
!******************************************************************************
!
!   Compute the distance of the point (x,y,z) from a given Tip defined by using
!     linear interpolation between given points + parametric representation
!
!   ARGUMENTS:
!
!       REAL(KIND=dp) :: x,y,z
!           INPUT: Coordinates of the point at which we compute the distance
!
!       REAL(KIND=dp) :: xtip(nbpoints),ytip(nbpoints),ztip(nbpoints)
!           INPUT: Coordinates of the points used for the linear interpolation 
!
!   INTEGER :: nbpoints
!           INPUT: Number of interpolation points
!
!******************************************************************************
        REAL(KIND=dp) :: Distance, x, y, z, xtip(nbpoints), ytip(nbpoints), ztip(nbpoints), &
            xproj, yproj, zproj
        INTEGER :: nbpoints 
        !------------------------------------------------------------------------------ 
        !   Local variables
        !------------------------------------------------------------------------------ 
        INTEGER :: i,j, S_MAX 
        REAL(KIND=dp) :: s, d, x_s, y_s, z_s
        !------------------------------------------------------------------------------   
        Distance = 100000.0D0   
        !------------------------------------------------------------------------------ 
        !   Case of point source
        !------------------------------------------------------------------------------ 
        IF (nbpoints==1) THEN
        !------------------------------------------------------------------------------ 
            Distance = sqrt( (x-xtip(1))**2 + (y-ytip(1))**2 + (z-ztip(1))**2 )
            xproj =xtip(1)
            yproj =ytip(1)
            zproj =ztip(1)
        !------------------------------------------------------------------------------ 
        ELSE
            !------------------------------------------------------------------------------ 
            !   For each linear part, compute the minimal distance in function of parameter s
            !------------------------------------------------------------------------------  
            DO i=1,nbpoints-1
            !------------------------------------------------------------------------------
                s = 0.0
                S_MAX = 100
                !------------------------------------------------------------------------------
                DO j = 1,S_MAX-1
                !------------------------------------------------------------------------------
                    x_s = (xtip(i+1)-xtip(i))*s + xtip(i)
                    y_s = (ytip(i+1)-ytip(i))*s + ytip(i)
                    z_s = (ztip(i+1)-ztip(i))*s + ztip(i)

                    d = sqrt( (x-x_s)**2 + (y-y_s)**2 + (z-z_s)**2 )
                    IF (d<Distance) THEN 
                        Distance = d
                        xproj =x_s
                        yproj =y_s
                        zproj =z_s
                    END IF
                    s = j*1.0/(S_MAX-1)
                !------------------------------------------------------------------------------
                END DO
            !------------------------------------------------------------------------------
            END DO
        !------------------------------------------------------------------------------
        END IF
!------------------------------------------------------------------------------ 
    END FUNCTION NumaDistanceToElectricTip
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
    SUBROUTINE ErrorCompute( Model, Temperature, Reorder,NDOFs )
!------------------------------------------------------------------------------
!******************************************************************************
!
!     Compute error at model nodes between numerical and analytical solution
!
!     ARGUMENTS:
!
!       TYPE(Model_t) :: Model
!           INPUT: All model information (mesh, materials, BCs, etc...)
!
!       REAL(KIND=dp) :: Temperature(:)
!           INPUT: Temperature, solution of the linear system 
!
!       INTEGER :: Reorder
!           INPUT: Element local numbering
!
!       INTEGER :: NDOFs
!           INPUT: Number of degres of freedom of the solution
!
!
!******************************************************************************
        TYPE(Model_t) :: Model
        REAL(KIND=dp) :: Temperature(:)
        INTEGER :: Reorder(:),NDOFs
!------------------------------------------------------------------------------
!     Local variables
!------------------------------------------------------------------------------
        TYPE(Element_t), POINTER :: Element
        TYPE(Nodes_t) :: Nodes 
        TYPE(GaussIntegrationPoints_t), TARGET :: IntegStuff

        REAL(KIND=dp), POINTER :: U_Integ(:), V_Integ(:), W_Integ(:), S_Integ(:)
        
        REAL(KIND=dp) :: &
            Basis(Model % MaxElementNodes), &
            dBasisdx(Model % MaxElementNodes,3), &
            SqrtElementMetric, &
            ElementTemperature(NDOFs,Model % MaxElementNodes), &
            ElementAnalyticalSol(NDOFs,Model % MaxElementNodes), &
            s, ug, vg, wg, Error(NDOFs), GradError(NDOFs),&
            SqrtMetric, Metric(3,3), Symb(3,3,3), dSymb(3,3,3,3), &
            x, y, z
            
        INTEGER, POINTER :: NodeIndexes(:)
        INTEGER :: N_Integ, t, tg, i, j, k_dof, dim
        LOGICAL :: Stat         
!------------------------------------------------------------------------------
!       Allocate the nodes coordinates vectors
!------------------------------------------------------------------------------
        ALLOCATE( Nodes % x( Model % MaxElementNodes ) )
        ALLOCATE( Nodes % y( Model % MaxElementNodes ) )
        ALLOCATE( Nodes % z( Model % MaxElementNodes ) )
!------------------------------------------------------------------------------
!       Get the model dimension
!------------------------------------------------------------------------------     
        dim = CoordinateSystemDimension()
!------------------------------------------------------------------------------
!       Initialize the error
!------------------------------------------------------------------------------         
        ErrorL2 = 0.0D0
        ErrorH1 = 0.0D0
        !------------------------------------------------------------------------------
        !     Go through the elements, we will compute on average of elementwise
        !     fluxes to nodes of the model
        !------------------------------------------------------------------------------
        DO t = 1,Solver % NumberOfActiveElements
            !------------------------------------------------------------------------------
            ! Check if this element belongs to a body where error
            ! should be calculated
            !------------------------------------------------------------------------------
            Element => Solver % Mesh % Elements( Solver % ActiveElements( t ) )
            Material => GetMaterial()
            !------------------------------------------------------------------------------
            ! Global indexes of the nodes of the element
            !------------------------------------------------------------------------------
            NodeIndexes => Element % NodeIndexes
            !------------------------------------------------------------------------------
            ! Number of nodes of the element
            !------------------------------------------------------------------------------
            n = Element % TYPE % NumberOfNodes
            !------------------------------------------------------------------------------
            ! Check if the element is active
            !------------------------------------------------------------------------------
            IF ( ANY(Reorder(NodeIndexes) == 0) ) CYCLE
            !------------------------------------------------------------------------------
            ! Get coordinates of nodes:
            !------------------------------------------------------------------------------
            Nodes % x(1:n) = Model % Nodes % x( NodeIndexes )
            Nodes % y(1:n) = Model % Nodes % y( NodeIndexes )
            Nodes % z(1:n) = Model % Nodes % z( NodeIndexes )
            !------------------------------------------------------------------------------
            ! Get (element local) numerical solution:
            !------------------------------------------------------------------------------
            ElementTemperature = 0.0D0
            DO i=1,n
                k = TempPerm(NodeIndexes(i))
                DO k_dof=1,NDOFs
                    ElementTemperature(k_dof,i) = Temperature(NDOFs*(k-1)+k_dof)
                END DO
            END DO
            !------------------------------------------------------------------------------
            ! Get (element local) analytical solution:
            !------------------------------------------------------------------------------
            ElementAnalyticalSol = 0.0D0
            DO i=1,n
                DO k_dof=1,NDOFs
                    ElementAnalyticalSol(k_dof,i) = AnalyticalSolution(Nodes % x(i),Nodes % y(i),Nodes % z(i))
                END DO
            END DO
            !------------------------------------------------------------------------------
            ! Gauss integration stuff
            !------------------------------------------------------------------------------
            IntegStuff = GaussPoints( Element )
            U_Integ => IntegStuff % u
            V_Integ => IntegStuff % v
            W_Integ => IntegStuff % w
            S_Integ => IntegStuff % s
            N_Integ =  IntegStuff % n
            !------------------------------------------------------------------------------
            ! Loop over Gauss integration points
            !------------------------------------------------------------------------------
            DO tg=1,N_Integ
            !------------------------------------------------------------------------------
                ug = U_Integ(tg)
                vg = V_Integ(tg)
                wg = W_Integ(tg)
                !------------------------------------------------------------------------------
                ! Need SqrtElementMetric and Basis at the integration point
                !------------------------------------------------------------------------------
                stat = ElementInfo( Element, Nodes,ug,vg,wg, &
                    SqrtElementMetric,Basis,dBasisdx )
                !------------------------------------------------------------------------------
                ! Coordinatesystem dependent info
                !------------------------------------------------------------------------------
                s = 1
                CALL CoordinateSystemInfo( Metric,SqrtMetric,Symb,dSymb,x,y,z )
                s = s * SqrtMetric * SqrtElementMetric * S_Integ(tg)
                !------------------------------------------------------------------------------
                ! Approximation of the L2 and H1 error by using the basis functions
                !------------------------------------------------------------------------------
                Error = 0.0d0
                GradError = 0.0d0
                
                DO k_dof =1,NDOFs
                    !------------------------------------------------------------------------------
                    ! L^2-part of error norm 
                    !------------------------------------------------------------------------------
                    Error(k_dof) = SUM( Basis(1:n) * &
                        abs(ElementTemperature(k_dof,1:n)-ElementAnalyticalSol(k_dof,1:n)) )
                    
                    ErrorL2 = ErrorL2 + s * Error(k_dof) * Error(k_dof)  
                    ErrorH1 = ErrorH1 + s * Error(k_dof) * Error(k_dof)
                    !------------------------------------------------------------------------------
                    ! H^1_0-part of error norm 
                    !------------------------------------------------------------------------------
                    DO j = 1, DIM
                        GradError(k_dof) =  SUM( dBasisdx(1:n,j) * &
                        abs(ElementTemperature(k_dof,1:n)-ElementAnalyticalSol(k_dof,1:n)) )
                        ErrorH1 = ErrorH1 + s * GradError(k_dof) * GradError(k_dof)
                    END DO      
                END DO !k_dof
            !------------------------------------------------------------------------------
        END DO ! of the Gauss integration points
    !------------------------------------------------------------------------------
    END DO ! of the bulk elements
    !------------------------------------------------------------------------------
    ErrorL2 = sqrt(ErrorL2) 
    !------------------------------------------------------------------------------
    DEALLOCATE( Nodes % x, Nodes % y, Nodes % z)
    !------------------------------------------------------------------------------
   END SUBROUTINE ErrorCompute
!------------------------------------------------------------------------------








!------------------------------------------------------------------------------
    SUBROUTINE TemperatureBoundary( BoundaryMatrix,BoundaryVector, &
               LoadVector,NodalAlpha,Element,n,Nodes,NDOFs )
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Return element local matrices and RHS vector for boundary conditions
!  of diffusion convection equation: 
!
!  ARGUMENTS:
!
!  REAL(KIND=dp) :: BoundaryMatrix(:,:)
!     OUTPUT: coefficient matrix if equations
!
!  REAL(KIND=dp) :: BoundaryVector(:)
!     OUTPUT: RHS vector
!
!  REAL(KIND=dp) :: LoadVector(:,:)
!     INPUT: coefficient of the force term
!
!  REAL(KIND=dp) :: NodalAlpha(:,:)
!     INPUT: coefficient for temperature dependent term
!
!  TYPE(Element_t) :: Element
!       INPUT: Structure describing the element (dimension,nof nodes,
!               interpolation degree, etc...)
!
!   INTEGER :: n
!       INPUT: Number  of element nodes
!
!  TYPE(Nodes_t) :: Nodes
!       INPUT: Element node coordinates
!
!******************************************************************************
        REAL(KIND=dp) :: BoundaryMatrix(:,:),BoundaryVector(:), &
            LoadVector(:,:),NodalAlpha(:,:), &
            ddBasisddx(n,3,3), Basis(n), dBasisdx(n,3), SqrtElementMetric, &
            U,V,W,S, &
            Force(NDOFs),Alpha(NDOFs)
        
        REAL(KIND=dp), POINTER :: U_Integ(:),V_Integ(:),W_Integ(:),S_Integ(:)
        
        INTEGER :: n, NDOFS, k_dof, i, t, q, p, N_Integ

        LOGICAL :: stat
        
        TYPE(Nodes_t)   :: Nodes
        TYPE(Element_t) :: Element
        TYPE(GaussIntegrationPoints_t), TARGET :: IntegStuff
!------------------------------------------------------------------------------
        BoundaryVector = 0.0D0
        BoundaryMatrix = 0.0D0
!------------------------------------------------------------------------------
!     Integration stuff
!------------------------------------------------------------------------------
        IntegStuff = GaussPoints( Element )
        U_Integ => IntegStuff % u
        V_Integ => IntegStuff % v
        W_Integ => IntegStuff % w
        S_Integ => IntegStuff % s
        N_Integ =  IntegStuff % n
!------------------------------------------------------------------------------
!     Loop over Gauss integration points
!------------------------------------------------------------------------------
        DO t=1,N_Integ
            U = U_Integ(t)
            V = V_Integ(t)
            W = W_Integ(t)
!------------------------------------------------------------------------------
!        Basis function values & derivatives at the integration point
!------------------------------------------------------------------------------
            stat = ElementInfo( Element, Nodes, u, v, w, SqrtElementMetric, Basis, dBasisdx )
            S = SqrtElementMetric * S_Integ(t)
!------------------------------------------------------------------------------
            DO k_dof=1,NDOFs
!------------------------------------------------------------------------------         
                Force(k_dof) = SUM( LoadVector(k_dof,1:n)*Basis )
                Alpha(k_dof) = SUM( NodalAlpha(k_dof,1:n)*Basis )
!------------------------------------------------------------------------------         
!               Update local boundary matrix
!------------------------------------------------------------------------------         
                DO p=1,N
                    DO q=1,N
                        BoundaryMatrix(NDOFs*(p-1)+k_dof,NDOFs*(q-1)+k_dof) = &
                        BoundaryMatrix(NDOFs*(p-1)+k_dof,NDOFs*(q-1)+k_dof) + &
                        s * Alpha(k_dof) * Basis(q) * Basis(p)
                    END DO
                END DO
!------------------------------------------------------------------------------         
!               Update local boundary vector
!------------------------------------------------------------------------------ 
                DO q=1,N
                    BoundaryVector(NDOFs*(q-1)+k_dof) = BoundaryVector(NDOFs*(q-1)+k_dof) &
                    + s * Basis(q) * Force(k_dof)
!------------------------------------------------------------------------------                 
                END DO
!------------------------------------------------------------------------------         
            END DO ! k_dof
!------------------------------------------------------------------------------
        END DO ! Gauss integration points
!------------------------------------------------------------------------------     
    END SUBROUTINE TemperatureBoundary
!------------------------------------------------------------------------------













!------------------------------------------------------------------------------
    SUBROUTINE TemperatureCompose( MassMatrix,StiffMatrix,ForceVector,  &
      LoadVector, NodalInterMaterialCoeff, &
      NodalCT,NodalC0,NodalC1,NodalC2, &
      UX,UY,UZ,NodalDensity, NodalPerfusionCoeff, &
        Stabilize, UseBubbles,Element,n,Nodes, NDOFs )
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
!  REAL(KIND=dp) :: NodalInterMaterialCoeff(:,:)
!     INPUT: Nodal values of the transfer coeff. if convection transfer between materials
!
!  REAL(KIND=dp) :: NodalCT(:,:),NodalC0,NodalC1(:,:)
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
!  INTEGER :: NDOFs
!       INPUT: Number of degrees of freedom of the solution
!******************************************************************************
        REAL(KIND=dp) :: ForceVector(:), &
            MassMatrix(:,:),StiffMatrix(:,:),LoadVector(:,:),UX(:,:),UY(:,:),UZ(:,:), &
            NodalDensity(:,:), NodalC2(:,:,:,:), dT, &
            NodalInterMaterialCoeff(:,:), NodalC0(:), NodalC1(:,:), NodalCT(:,:), &
            NodalPerfusionCoeff(:)
            
        LOGICAL :: Stabilize, UseBubbles

        INTEGER :: n, NDOFs

        TYPE(Nodes_t) :: Nodes
        TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
!    Local variables
!------------------------------------------------------------------------------
        REAL(KIND=dp) :: &
            Basis(2*n), dBasisdx(2*n,3), ddBasisddx(n,3,3), &
            SqrtElementMetric, s, u, v, w, &
            Velo(NDOFs,3), Force(NDOFs), dVelodx(3,3),&
            A(NDOFs),B(NDOFs),M(NDOFs), &
            Load, &
            VNorm, hK, mK, SU(NDOFs*n), SW(NDOFs*n), &
            Pe,Tau(NDOFs),x,y,z, Density(NDOFs), &
            C00(NDOFs),C0(NDOFs),C1(NDOFs),CT(NDOFs),C2(NDOFs,3,3),dC2dx(NDOFs,3,3,3)
            
        REAL(KIND=dp), DIMENSION(:), POINTER :: U_Integ,V_Integ,W_Integ,S_Integ
            
        INTEGER :: i,j,k,p,q,t,dim,N_Integ,NBasis,k_dof
     
        LOGICAL :: stat, Convection, ConvectAndStabilize, Bubbles
        
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
        B = 0.0D0
        M = 0.0D0
!------------------------------------------------------------------------------
!       Check if any convection
!------------------------------------------------------------------------------     
        Convection =  ANY( NodalC1 /= 0.0d0 )
!------------------------------------------------------------------------------
!       Get numbre of basis functions       
!------------------------------------------------------------------------------ 
        NBasis = n
        !BUBBLES:
        IF(Convection .AND. UseBubbles) THEN
            NBasis = 2*n        
            Bubbles = .TRUE.
        END IF
!------------------------------------------------------------------------------
!     Integration stuff
!------------------------------------------------------------------------------
        !BUBBLES:       
        IF(Convection .AND. UseBubbles) THEN
            IntegStuff = GaussPoints( element, Element % Type % GaussPoints2 )
        ELSE
            IntegStuff = GaussPoints( element )
        END IF

        U_Integ => IntegStuff % u
        V_Integ => IntegStuff % v
        W_Integ => IntegStuff % w
        S_Integ => IntegStuff % s
        N_Integ =  IntegStuff % n
!------------------------------------------------------------------------------
!    Stabilization parameters: hK, mK (take a look at Franca et.al.)
!    If there is no convection term we don t need stabilization.
!------------------------------------------------------------------------------
        ConvectAndStabilize = .FALSE.
        IF ( Stabilize .AND. Convection ) THEN
            ConvectAndStabilize = .TRUE.
            hK = element % hK
            mK = element % StabilizationMK
        END IF
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
            IF(Convection .AND. UseBubbles) THEN
                stat = ElementInfo( Element,Nodes,u,v,w,SqrtElementMetric, Basis, &
                    dBasisdx,ddBasisddx, Bubbles=Bubbles )
            ELSE
                stat = ElementInfo( Element,Nodes,u,v,w,SqrtElementMetric, &
                     Basis,dBasisdx,ddBasisddx,ConvectAndStabilize )    
            END IF
                 
            s = SqrtElementMetric * S_Integ(t)      
!------------------------------------------------------------------------------
!        Coefficient of the convection (C1), time derivative (CT), and O-order  
!           derivative (C0) terms at the integration point
!------------------------------------------------------------------------------         
            DO k_dof=1,NDOFs
                C0(k_dof) = SUM( NodalC0(1:n) * Basis(1:n) )   
                C1(k_dof) = SUM( NodalC1(k_dof,1:n) * Basis(1:n) )
                CT(k_dof) = SUM( NodalCT(k_dof,1:n) * Basis(1:n) )  
            END DO  
!------------------------------------------------------------------------------
!        For a given DOF, C00 is the coefficient relative to the other DOF in 
!           the convective  transfer term
!------------------------------------------------------------------------------                 
            C00 = 0.0D0
            
            DO k_dof=1,NDOFs-1
                C0(k_dof) = C0(k_dof) + SUM( NodalInterMaterialCoeff(k_dof,1:n) * Basis(1:n) )    
                C00(k_dof+1) = C00(k_dof+1) - SUM( NodalInterMaterialCoeff(k_dof,1:n) * Basis(1:n) )
            END DO
            C0(NDOFs) = C0(NDOFs) + SUM( NodalInterMaterialCoeff(NDOFs,1:n) * Basis(1:n) )    
            C00(1) = C00(1) - SUM( NodalInterMaterialCoeff(NDOFs,1:n) * Basis(1:n) )
!------------------------------------------------------------------------------
!           Part of the perfusion term containing temperature variable in tissue equation:
!------------------------------------------------------------------------------                 
            C0(1) = C0(1) + SUM( NodalPerfusionCoeff(1:n) * Basis(1:n) )    
!------------------------------------------------------------------------------
!        Approximation of density and coefficient of the diffusion term 
!------------------------------------------------------------------------------
            DO k_dof=1,NDOFs
            
                Density(k_dof) = SUM( NodalDensity(k_dof,1:n) * Basis(1:n) ) 
               
                DO i=1,dim
                    DO j=1,dim
                        C2(k_dof,i,j) = SUM( NodalC2(k_dof,i,j,1:n) * Basis(1:n) )
                    END DO
                END DO
                                
            END DO ! k_dof  
!------------------------------------------------------------------------------
!        If there's no convection term we don't need the velocities, and
!        also no need for stabilization
!------------------------------------------------------------------------------
            Convection = .FALSE.
            IF (ANY( C1 /= 0.0D0 )) THEN
            
                Convection = .TRUE.
    
                Velo = 0.0D0
                dVelodx=0.0D0
!------------------------------------------------------------------------------             
                DO k_dof=1,NDOFs
!------------------------------------------------------------------------------
!              Approximation of velocity
!----------------------------------------------------------------------
                    Velo(k_dof,1) = SUM( UX(k_dof,1:n)*Basis(1:n) )
                    Velo(k_dof,2) = SUM( UY(k_dof,1:n)*Basis(1:n) )
                    IF ( dim > 2 ) Velo(k_dof,3) = SUM( UZ(k_dof,1:n)*Basis(1:n) )
!------------------------------------------------------------------------------
!              Stabilization parameter Tau
!------------------------------------------------------------------------------
                    IF ( Stabilize ) THEN
                        
                        VNorm = SQRT( SUM(Velo(k_dof,1:dim)**2) )
                        Pe  = MIN( 1.0D0, mK*hK*C1(k_dof)*VNorm/(2*ABS(C2(k_dof,1,1))) )
                        Tau(k_dof) = 0.0D0
                        IF ( VNorm /= 0.0 ) THEN
                            Tau(k_dof) = hK * Pe / (2 * C1(k_dof) * VNorm)
                        END IF
!------------------------------------------------------------------------------
                        DO i=1,dim
                            DO j=1,dim
                                DO k=1,dim
                                    dC2dx(k_dof,i,j,k) = SUM( NodalC2(k_dof,i,j,1:n)*dBasisdx(1:n,k) )
                                END DO
                            END DO
                        END DO      
!------------------------------------------------------------------------------
!                   Compute residual & stablization vectors
!------------------------------------------------------------------------------
                        DO p=1,N
                            !TEST: removes convective transfer from stabilization
                            !SU(NDOFs*(p-1)+k_dof) = C0(k_dof) * Basis(p)
                            !SU(NDOFs*(p-1)+k_dof) = SU(NDOFs*(p-1)+k_dof) + C00(k_dof) * Basis(p)
                            SU(NDOFs*(p-1)+k_dof) = 0.0

                            DO i = 1,dim
                                SU(NDOFs*(p-1)+k_dof) = SU(NDOFs*(p-1)+k_dof) + C1(k_dof) * dBasisdx(p,i) * Velo(k_dof,i)
                                DO k=1,dim
                                    SU(NDOFs*(p-1)+k_dof) = SU(NDOFs*(p-1)+k_dof) - C2(k_dof,i,k) * ddBasisddx(p,i,k)
                                    SU(NDOFs*(p-1)+k_dof) = SU(NDOFs*(p-1)+k_dof) - dC2dx(k_dof,i,k,k) * dBasisdx(p,i)
                                END DO
                            END DO
                            
                            SW(NDOFs*(p-1)+k_dof) = 0.0

                            DO i = 1,dim
                                SW(NDOFs*(p-1)+k_dof) = SW(NDOFs*(p-1)+k_dof) + C1(k_dof) * dBasisdx(p,i) * Velo(k_dof,i)
                                DO k=1,dim
                                    SW(NDOFs*(p-1)+k_dof) = SW(NDOFs*(p-1)+k_dof) - C2(k_dof,i,k) * ddBasisddx(p,i,k)
                                    SW(NDOFs*(p-1)+k_dof) = SW(NDOFs*(p-1)+k_dof) - dC2dx(k_dof,i,k,k) * dBasisdx(p,i)
                                END DO
                            END DO
                            
                        END DO
!------------------------------------------------------------------------------                     
                    END IF ! stabilize
!------------------------------------------------------------------------------ 
                 END DO ! k_dof
!------------------------------------------------------------------------------
            END IF ! convection         
!------------------------------------------------------------------------------
!        Loop over basis functions of both unknowns and weights
!------------------------------------------------------------------------------
            DO k_dof=1,NDOFs
            
                DO p=1,NBasis
                    DO q=1,NBasis
        !------------------------------------------------------------------------------
        !           The diffusive-convective equation without stabilization
        !------------------------------------------------------------------------------
                        M(k_dof) = CT(k_dof) * Basis(q) * Basis(p)
                        A(k_dof) = C0(k_dof) * Basis(q) * Basis(p)
                        B(k_dof) = C00(k_dof) * Basis(q) * Basis(p)
        !------------------------------------------------------------------------------
        !           The diffusion term
        !------------------------------------------------------------------------------
                        DO i=1,dim
                            DO j=1,dim
                                A(k_dof) = A(k_dof) + C2(k_dof,i,j) * dBasisdx(q,i) * dBasisdx(p,j)
                            END DO
                        END DO

                        IF ( Convection ) THEN
        !------------------------------------------------------------------------------
        !              The convection term
        !------------------------------------------------------------------------------
                            DO i=1,dim
                                A(k_dof) = A(k_dof) + C1(k_dof) * Velo(k_dof,i) * dBasisdx(q,i) * Basis(p)
                            END DO
        !------------------------------------------------------------------------------
        !              Next we add the stabilization
        !------------------------------------------------------------------------------
                            IF ( Stabilize ) THEN
                                A(k_dof) = A(k_dof) + Tau(k_dof) * SU(NDOFs*(q-1)+k_dof) * SW(NDOFs*(p-1)+k_dof)    
                                M(k_dof) = M(k_dof) + Tau(k_dof) * CT(k_dof) * Basis(q) * SW(NDOFs*(p-1)+k_dof)

                            END IF ! stabilize
                        END IF ! convection
        !------------------------------------------------------------------------------
        !           Stiff matrix: diffusion, convection, 0-degree term
        !------------------------------------------------------------------------------
                        StiffMatrix(NDOFs*(p-1)+k_dof,NDOFs*(q-1)+k_dof) = &
                            StiffMatrix(NDOFs*(p-1)+ k_dof,NDOFs*(q-1)+k_dof) + s * A(k_dof)
        !------------------------------------------------------------------------------
        !           Stiff matrix: convective transfer
        !------------------------------------------------------------------------------             
                        IF (k_dof<NDOFs) THEN 
                            StiffMatrix(NDOFs*(p-1)+k_dof+1,NDOFs*(q-1)+k_dof) = &
                                StiffMatrix(NDOFs*(p-1)+ k_dof+1,NDOFs*(q-1)+k_dof) + s * B(k_dof)
                        ELSE    
                            StiffMatrix(NDOFs*(p-1)+1,NDOFs*(q-1)+k_dof) = &
                                StiffMatrix(NDOFs*(p-1)+1,NDOFs*(q-1)+k_dof) + s * B(k_dof) 
                        END IF
        !------------------------------------------------------------------------------
        !           Mass matrix: time derivative
        !------------------------------------------------------------------------------             
                        MassMatrix(NDOFs*(p-1)+k_dof,NDOFs*(q-1)+k_dof)  = &
                            MassMatrix(NDOFs*(p-1)+k_dof,NDOFs*(q-1)+k_dof)  + s * M(k_dof)
                        
                    END DO ! q
                END DO ! p
                
            END DO ! k_dof
            
!------------------------------------------------------------------------------
!        The righthand side
!------------------------------------------------------------------------------
!        Force at the integration point
!------------------------------------------------------------------------------
            Force = 0.0D0
            DO k_dof = 1,NDOFs
                !------------------------------------------------------------------------------
                Force(k_dof) = SUM( LoadVector(k_dof,1:n)*Basis(1:n) )
                !------------------------------------------------------------------------------
                IF ( Convection ) THEN
                !------------------------------------------------------------------------------
                    IF ( C2(k_dof,1,1) > 0.0d0 ) THEN
                        DO i=1,3
                            dVelodx(1,i) = SUM( UX(k_dof,1:n)*dBasisdx(1:n,i) )
                            dVelodx(2,i) = SUM( UY(k_dof,1:n)*dBasisdx(1:n,i) )
                            IF ( dim > 2 ) dVelodx(3,i) = SUM( UZ(k_dof,1:n)*dBasisdx(1:n,i) )
                        END DO
                        Force(k_dof) = Force(k_dof) + 0.5d0 * C2(k_dof,1,1)*SecondInvariant(Velo(k_dof,:),dVelodx)
                    END IF
                !------------------------------------------------------------------------------
                END IF
                !------------------------------------------------------------------------------
        END DO
!------------------------------------------------------------------------------
            DO k_dof = 1,NDOFs
                DO q=1,NBasis
                    Load = Basis(q)
                    IF ( ConvectAndStabilize ) Load = Load + Tau(k_dof) * SW(NDOFs*(q-1)+k_dof)
!------------------------------------------------------------------------------
!               Force vector
!------------------------------------------------------------------------------                     
                    ForceVector(NDOFs*(q-1)+k_dof) = ForceVector(NDOFs*(q-1)+k_dof) + s * Force(k_dof) * Load
                END DO
                Integ_Force(k_dof) =  Integ_Force(k_dof) + Force(k_dof) * s 
            END DO
!------------------------------------------------------------------------------
        END DO ! Gauss integration points
!------------------------------------------------------------------------------
   END SUBROUTINE TemperatureCompose
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
    SUBROUTINE TotalPowerCompose( LoadVector,Element,n,Nodes, NDOFs )
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
!
!  INTEGER :: NDOFs
!       INPUT: Number of degrees of freedom of the solution
!******************************************************************************
        REAL(KIND=dp) :: LoadVector(:,:)
        INTEGER :: n, NDOFs
        TYPE(Nodes_t) :: Nodes
        TYPE(Element_t), POINTER :: Element
!------------------------------------------------------------------------------
!    Local variables
!------------------------------------------------------------------------------
        REAL(KIND=dp) :: Basis(2*n), dBasisdx(2*n,3), ddBasisddx(n,3,3), &
            SqrtElementMetric, s, u, v, w, Force(NDOFs)

        REAL(KIND=dp), DIMENSION(:), POINTER :: U_Integ,V_Integ,W_Integ,S_Integ
            
        INTEGER :: i,j,k,p,q,t,N_Integ,NBasis,k_dof
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
!------------------------------------------------------------------------------
!        Loop over basis functions
!------------------------------------------------------------------------------
            Force = 0.0D0
            DO k_dof = 1,NDOFs
                Force(k_dof) = SUM( LoadVector(k_dof,1:n)*Basis(1:n) )
                TotalPower(k_dof) =  TotalPower(k_dof) + Force(k_dof) * s 
            END DO
!------------------------------------------------------------------------------
        END DO ! Gauss integration points
!------------------------------------------------------------------------------
   END SUBROUTINE TotalPowerCompose
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
    FUNCTION AnalyticalSolution( x, y, z ) RESULT(T)
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Return the analytical temperature for some simple problems (used in error computation) 
!
!  ARGUMENTS:
!
!  REAL(KIND=dp) :: x, y, z
!     INPUT: coordinates
!
!******************************************************************************
        REAL(KIND=dp) :: x, y, z, T
!------------------------------------------------------------------------------
        ! Diffusion on a square with dirichlet conditions only:
        !T = sinh(pi*y)*sin(pi*x)/sinh(pi)
        
        ! Diffusion on a square with dirichlet and Neumann conditions:
        T = -2*cosh(pi)*cos(2*pi*(x-0.5))*sinh(2*pi*(y-0.5))/sinh(2*pi)
        
        ! Advection-diffusion on a square with dirichlet and Neumann conditions:
        !T = (exp(20.0)-2+exp(20.0*y))/(exp(20.0)-1)
!------------------------------------------------------------------------------
    END FUNCTION AnalyticalSolution
!------------------------------------------------------------------------------




!------------------------------------------------------------------------------
    FUNCTION GapTemperature( Solver, Element, Temperature, TempPerm, n ) RESULT(T)
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Return element's neighbourhood temperature 
!
!  ARGUMENTS:
!
!  TYPE(Solver_t) :: Solver
!     INPUT: Linear equation solver options
!
!  TYPE(Element_t) :: Element
!   INPUT: Structure describing the element (dimension,nof nodes,
!               interpolation degree, etc...)
!
!  REAL(KIND=dp) :: Temperature(:)
!     INPUT: Nodal values of temperature from previous iteration
!
!   INTEGER :: TempPerm(:)
!       INPUT: Element local numbering
!
!  INTEGER :: n
!   INPUT: Number of element nodes
!
!******************************************************************************
        TYPE(Solver_t) :: Solver
        TYPE(Element_t), POINTER :: Element
        INTEGER :: TempPerm(:), n
        REAL(KIND=dp) :: Temperature(:)
!------------------------------------------------------------------------------
!     Local variables
!------------------------------------------------------------------------------
        TYPE(Element_t), POINTER :: Parent, Left, Right
        INTEGER :: i,j,k
        REAL(KIND=dp) :: x0,y0,z0,x,y,z,T(n)
!------------------------------------------------------------------------------
!       Initialization
!------------------------------------------------------------------------------
        T = 0.0d0
!------------------------------------------------------------------------------
!       Get the neighbourood information of the boundary element
!------------------------------------------------------------------------------     
        Left  => Element % BoundaryInfo % Left
        Right => Element % BoundaryInfo % Right

        IF ( .NOT. ASSOCIATED(Left) .OR. .NOT.ASSOCIATED(Right) ) RETURN
!------------------------------------------------------------------------------
!       Go through the nodes of the boundary element
!------------------------------------------------------------------------------     
        DO i=1,n
!------------------------------------------------------------------------------
!           Get the neighbour of the boundary element which is in the other side of
!           the boundary (may not be defined)
!------------------------------------------------------------------------------     
            Parent => Left
            k = Element % NodeIndexes(i)
            IF ( ANY( Parent % NodeIndexes == k ) ) Parent => Right
!------------------------------------------------------------------------------
!           Get the coordinates of the nodes of the boundary element
!------------------------------------------------------------------------------
            x0 = ElementNodes % x(i)
            y0 = ElementNodes % y(i)
            z0 = ElementNodes % z(i)
            
            DO j=1,Parent % TYPE % NumberOfNodes
!------------------------------------------------------------------------------
!               Go through the nodes of the Parent element
!------------------------------------------------------------------------------             
                k = Parent % NodeIndexes(j)
!------------------------------------------------------------------------------
!               Compute the distance between the boundary element node and the parent 
!               element node, and check this distance is lower than a fixed threshold
!------------------------------------------------------------------------------                     
                x = Solver % Mesh % Nodes % x(k) - x0
                y = Solver % Mesh % Nodes % y(k) - y0
                z = Solver % Mesh % Nodes % z(k) - z0
                IF ( x**2 + y**2 + z**2 < AEPS ) EXIT
!------------------------------------------------------------------------------                 
            END DO
!------------------------------------------------------------------------------
!           Get the parent element node temperature 
!------------------------------------------------------------------------------ 
            T(i) = Temperature( TempPerm( k ) )
!------------------------------------------------------------------------------             
        END DO
!------------------------------------------------------------------------------
    END FUNCTION GapTemperature
!------------------------------------------------------------------------------









!------------------------------------------------------------------------------
    SUBROUTINE AddHeatGap( Solver, Element, STIFF, TempPerm )
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Add heat gap coefficient to stiff matrix (if implicit treatment of the BC)
!
!  ARGUMENTS:
!
!  TYPE(Solver_t) :: Solver
!     INPUT: Linear equation solver options
!
!  TYPE(Element_t) :: Element
!   INPUT: Structure describing the element (dimension,nof nodes,
!               interpolation degree, etc...)
!
!  REAL(KIND=dp) :: STIFF(:,:)
!     INPUT/OUTPUT: Stiff matrix
!
!   INTEGER :: TempPerm(:)
!       INPUT: Element local numbering
!
!
!******************************************************************************
        TYPE(Solver_t) :: Solver
        REAL(KIND=dp) :: STIFF(:,:)
        INTEGER :: TempPerm(:)
        TYPE(Element_t) :: Element
!------------------------------------------------------------------------------
!     Local variables
!------------------------------------------------------------------------------
        TYPE(Element_t), POINTER :: Parent,Left,Right
        INTEGER :: i,j,k,l, Ind(n)
        REAL(KIND=dp) :: x0,y0,z0,x,y,z
!------------------------------------------------------------------------------
!       Get the neighbourood information of the boundary element
!------------------------------------------------------------------------------     
        Left  => Element % BoundaryInfo % Left
        Right => Element % BoundaryInfo % Right

        IF ( .NOT.ASSOCIATED(Left) .OR. .NOT.ASSOCIATED(Right) ) RETURN
!------------------------------------------------------------------------------
!       Initialization
!------------------------------------------------------------------------------
        l = 0
!------------------------------------------------------------------------------
!       Go through the nodes of the boundary element
!------------------------------------------------------------------------------     
        DO i=1,n
!------------------------------------------------------------------------------
!           Get the neighbour of the boundary element which is in the other side of
!           the boundary (may not be defined)
!------------------------------------------------------------------------------             
            Parent => Left
            k = Element % NodeIndexes(i)

            IF ( ANY( Parent % NodeIndexes == k ) ) Parent => Right
!------------------------------------------------------------------------------
!           Get the coordinates of the nodes of the boundary element
!------------------------------------------------------------------------------
            x0 = ElementNodes % x(i)
            y0 = ElementNodes % y(i)
            z0 = ElementNodes % z(i)
            
            DO j=1,Parent % TYPE % NumberOfNodes

                k = Parent % NodeIndexes(j)
!------------------------------------------------------------------------------
!               Compute the distance between the boundary element node and the parent 
!               element node, and check this distance is lower than a fixed threshold
!------------------------------------------------------------------------------     
                x = Solver % Mesh % Nodes % x(k) - x0
                y = Solver % Mesh % Nodes % y(k) - y0
                z = Solver % Mesh % Nodes % z(k) - z0
                IF ( x**2 + y**2 + z**2 < AEPS ) EXIT
!------------------------------------------------------------------------------             
            END DO
!------------------------------------------------------------------------------
            Ind(i) = k
!------------------------------------------------------------------------------
        END DO
!------------------------------------------------------------------------------
!       Modify the matrix
!------------------------------------------------------------------------------     
        DO i=1,n
            DO j=1,n
!------------------------------------------------------------------------------
!               Get the entries of the matrix
!------------------------------------------------------------------------------                 
                k = TempPerm( Element % NodeIndexes(i) )
                l = TempPerm( Ind(j) )
                IF ( k > 0 .AND. l > 0 ) &
                    CALL AddToMatrixElement( Solver % Matrix,k,l,-STIFF(i,j) )
            END DO
        END DO
!------------------------------------------------------------------------------
    END SUBROUTINE AddHeatGap
!------------------------------------------------------------------------------





!------------------------------------------------------------------------------
    FUNCTION ComputeMeshSize( Solver ) RESULT(MeshSize)
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Return mesh average size, defined as the average distance between two nodes
!
!  ARGUMENTS:
!
!  TYPE(Solver_t) :: Solver
!     INPUT: 
!
!******************************************************************************
        TYPE(Solver_t) :: Solver
        REAL(KIND=dp) :: MeshSize
!------------------------------------------------------------------------------
        INTEGER :: t,i,cpt
        INTEGER, POINTER :: NodeIndexes(:)  
        REAL(KIND=dp) :: tmp
        TYPE(Nodes_t) :: Nodes 
!------------------------------------------------------------------------------
!       Allocations and initializations
!------------------------------------------------------------------------------
        ALLOCATE( Nodes % x( Model % MaxElementNodes ) )
        ALLOCATE( Nodes % y( Model % MaxElementNodes ) )
        ALLOCATE( Nodes % z( Model % MaxElementNodes ) )

        MeshSize = 0.0
!------------------------------------------------------------------------------
!       Two times the number of edges
!------------------------------------------------------------------------------         
        cpt = 0
!------------------------------------------------------------------------------                         
!       Go through the elements of the model (bulk and boundary ones)       
!------------------------------------------------------------------------------                 
        DO t=1, Solver % Mesh % NumberOfBulkElements + Solver % Mesh % NumberOfBoundaryElements
!------------------------------------------------------------------------------                     
            Element =>  Solver % Mesh % Elements(t)
            !------------------------------------------------------------------------------
            ! Get the indexes of the element nodes:
            !------------------------------------------------------------------------------
            NodeIndexes => Element % NodeIndexes
            !------------------------------------------------------------------------------
            ! Get the number of element nodes:
            !------------------------------------------------------------------------------
            n = Element % TYPE % NumberOfNodes
            !------------------------------------------------------------------------------
            ! Get the coordinates of the element nodes:
            !------------------------------------------------------------------------------
            Nodes % x(1:n) = Model % Nodes % x( NodeIndexes )
            Nodes % y(1:n) = Model % Nodes % y( NodeIndexes )
            Nodes % z(1:n) = Model % Nodes % z( NodeIndexes )
            !------------------------------------------------------------------------------
            ! All elements (no condition on n)
            !------------------------------------------------------------------------------
            DO i =1,n-1
                !------------------------------------------------------------------------------
                ! Square of the distance between nodes
                !------------------------------------------------------------------------------
                tmp =  &
                    (Nodes % x(i+1)-Nodes % x(i))*(Nodes % x(i+1)-Nodes % x(i)) + &
                    (Nodes % y(i+1)-Nodes % y(i))*(Nodes % y(i+1)-Nodes % y(i)) + &
                    (Nodes % z(i+1)-Nodes % z(i))*(Nodes % z(i+1)-Nodes % z(i)) 
                !------------------------------------------------------------------------------
                ! Update sum of distances
                !------------------------------------------------------------------------------ 
                MeshSize = MeshSize +sqrt(tmp)
                !------------------------------------------------------------------------------
                ! Increment the number of edges
                !------------------------------------------------------------------------------ 
                cpt = cpt + 1
            END DO
            !------------------------------------------------------------------------------
            ! Trianglular and tetrahedral elements
            !------------------------------------------------------------------------------
            IF (n>=3) THEN
                !------------------------------------------------------------------------------
                ! Square of the distance between nodes
                !------------------------------------------------------------------------------
                tmp =  &
                    (Nodes % x(1)-Nodes % x(n))*(Nodes % x(1)-Nodes % x(n)) + &
                    (Nodes % y(1)-Nodes % y(n))*(Nodes % y(1)-Nodes % y(n)) + &
                    (Nodes % z(1)-Nodes % z(n))*(Nodes % z(1)-Nodes % z(n)) 
                !------------------------------------------------------------------------------
                ! Update sum of distances
                !------------------------------------------------------------------------------ 
                MeshSize = MeshSize +sqrt(tmp)
                !------------------------------------------------------------------------------
                ! Increment the number of edges
                !------------------------------------------------------------------------------ 
                cpt = cpt + 1
            END IF
            !------------------------------------------------------------------------------
            ! Tetrahedral elements
            !------------------------------------------------------------------------------
            IF (n==4) THEN
                !------------------------------------------------------------------------------
                ! Square of the distance between nodes
                !------------------------------------------------------------------------------
                DO i =1,n-2
                    tmp =  &
                        (Nodes % x(i+2)-Nodes % x(i))*(Nodes % x(i+2)-Nodes % x(i)) + &
                        (Nodes % y(i+2)-Nodes % y(i))*(Nodes % y(i+2)-Nodes % y(i)) + &
                        (Nodes % z(i+2)-Nodes % z(i))*(Nodes % z(i+2)-Nodes % z(i)) 
                    !------------------------------------------------------------------------------
                    ! Update sum of distances
                    !------------------------------------------------------------------------------ 
                    MeshSize = MeshSize +sqrt(tmp)
                    !------------------------------------------------------------------------------
                    ! Increment the number of edges
                    !------------------------------------------------------------------------------ 
                    cpt = cpt + 1
                END DO
            END IF
!------------------------------------------------------------------------------                     
        END DO ! t
!------------------------------------------------------------------------------
!       Compute the average distance between nodes
!------------------------------------------------------------------------------             
        MeshSize =MeshSize / cpt
        !PRINT *,'meshsize= ',MeshSize,'cpt= ',cpt
!------------------------------------------------------------------------------                     
        DEALLOCATE( Nodes % x, Nodes % y, Nodes % z)
!------------------------------------------------------------------------------
    END FUNCTION ComputeMeshSize
!------------------------------------------------------------------------------
















!------------------------------------------------------------------------------
    SUBROUTINE NumaDefaultDirichletConditions( Solver )
!------------------------------------------------------------------------------
!******************************************************************************
!
!  Set dirichlet condition on boundary and interior nodes 
!
!   ARGUMENT:
!
!  TYPE(Solver_t) :: Solver
!     INPUT: Linear equation solver options
!
!******************************************************************************
        TYPE(Solver_t), TARGET :: Solver
!------------------------------------------------------------------------------
        TYPE(Matrix_t), POINTER   :: A
        TYPE(Variable_t), POINTER :: x
        TYPE(ValueList_t), POINTER :: BC, SolverParams
        TYPE(Element_t), POINTER :: Element, Parent, Edge, Face, SaveElement
        
        REAL(KIND=dp), POINTER    :: b(:)
        REAL(KIND=dp) :: xx
        REAL(KIND=dp), ALLOCATABLE :: Work(:), STIFF(:,:)
        
        INTEGER, ALLOCATABLE :: lInd(:), gInd(:)
        INTEGER :: i,j, k, kk, l, m, n,nd, nb, mb, nn, DOF, local, numEdgeDofs,istat

        LOGICAL :: Flag,Found
        
        CHARACTER(LEN=MAX_NAME_LEN) :: name

        TYPE(Nodes_t) :: Nodes
!------------------------------------------------------------------------------
        SAVE gInd, lInd, STIFF, Work
!------------------------------------------------------------------------------
!       Get the linear system components
!------------------------------------------------------------------------------
        A => Solver % Matrix
        x => Solver % Variable
        b => A % RHS
!------------------------------------------------------------------------------
!       Get the maximal number of DOF 
!------------------------------------------------------------------------------
        n = Solver % Mesh % MaxElementDOFs
!------------------------------------------------------------------------------
!       Allocations
!------------------------------------------------------------------------------     
        IF ( .NOT. ALLOCATED( gInd ) ) THEN
            ALLOCATE( gInd(n), lInd(n), STIFF(n,n), Work(n), stat=istat )
            IF ( istat /= 0 ) &
                CALL Fatal('DefUtils::DefaultDirichletBCs','Memory allocation failed.' )
        ELSE IF ( SIZE(gInd) < n ) THEN
            DEALLOCATE( gInd, lInd, STIFF, Work )
            ALLOCATE( gInd(n), lInd(n), STIFF(n,n), Work(n), stat=istat )
            IF ( istat /= 0 ) &
                CALL Fatal('DefUtils::DefaultDirichletBCs','Memory allocation failed.' )
        END IF
!------------------------------------------------------------------------------
!       Special treatment if several DOFs ?
!------------------------------------------------------------------------------
        IF ( x % DOFs > 1 ) THEN
            ! TEMP!!!
            CALL NumaSetDirichletBoundaries( CurrentModel,A, b, x % Name,-1,x % DOFs,x % Perm )
            CALL NumaSetInteriorDirichletConditions( CurrentModel, A, b, x % &
                Name, -1, x % DOFs, x % Perm )
        END IF
!------------------------------------------------------------------------------
!       Clear dirichlet BCs for face & edge DOFs:
!------------------------------------------------------------------------------
        DO DOF=1,x % DOFs
!------------------------------------------------------------------------------     
!           Get the name of the DOF:
!------------------------------------------------------------------------------     
            name = x % name
            IF ( x % DOFs > 1 ) name = ComponentName(name,DOF)
!------------------------------------------------------------------------------
!           Go through the boundary elements:
!------------------------------------------------------------------------------
            DO i=1,Solver % Mesh % NumberOfBoundaryElements
!------------------------------------------------------------------------------
                Element => GetBoundaryElement(i)
!------------------------------------------------------------------------------
!               Check if the element is active:
!------------------------------------------------------------------------------
                IF ( .NOT. ActiveBoundaryElement() ) CYCLE
!------------------------------------------------------------------------------
!               Get the BC associated to this element
!               Check if the BC exists
!------------------------------------------------------------------------------
                BC => GetBC()
                IF ( .NOT. ASSOCIATED( BC ) ) CYCLE
                IF ( .NOT. ListCheckPresent(BC, Name) ) CYCLE 
!------------------------------------------------------------------------------
!               Get the parent element: bulk element with common face or edge.
!               Left or right
!------------------------------------------------------------------------------
                Parent => Element % BoundaryInfo % Left
                IF ( .NOT. ASSOCIATED( Parent ) ) THEN
                    Parent => Element % BoundaryInfo % Right
                END IF
                IF ( .NOT. ASSOCIATED( Parent ) ) CYCLE
!------------------------------------------------------------------------------
!               Clear dofs associated with element edges:
!------------------------------------------------------------------------------
                IF ( ASSOCIATED( Solver % Mesh % Edges ) ) THEN
!------------------------------------------------------------------------------             
                    DO j=1,Parent % TYPE % NumberOfEdges
!------------------------------------------------------------------------------                 
                        Edge => Solver % Mesh % Edges( Parent % EdgeIndexes(j) )
                        IF ( Edge % BDOFs == 0 ) CYCLE

                        n = 0
                        DO k=1,Element % TYPE % NumberOfNodes
                            DO l=1,Edge % TYPE % NumberOfNodes
                                IF ( Edge % NodeIndexes(l) == Element % NodeIndexes(k) ) n=n+1
                            END DO
                        END DO

                        IF ( n ==  Edge % Type % NumberOfNodes ) THEN
                            DO k=1,Edge % BDOFs
                                n = Solver % Mesh % NumberofNodes + &
                                    (Parent % EdgeIndexes(j)-1)*Solver % Mesh % MaxEdgeDOFs+k
                                n = x % Perm( n )
                                IF ( n <= 0 ) CYCLE
                                n = x % DOFs*(n-1) + DOF
                                CALL CRS_ZeroRow( A, n )
                                A % RHS(n) = 0.0d0
                            END DO
                        END IF
                    END DO
                END IF
!------------------------------------------------------------------------------
!              Clear dofs associated with element faces:
!------------------------------------------------------------------------------
                IF ( ASSOCIATED( Solver % Mesh % Faces ) ) THEN
!------------------------------------------------------------------------------             
                    DO j=1,Parent % Type % NumberOfFaces
!------------------------------------------------------------------------------
                        Face => Solver % Mesh % Faces( Parent % FaceIndexes(j) )
                        IF ( Face % BDOFs == 0 ) CYCLE

                        n = 0
                        DO k=1,Element % TYPE % NumberOfNodes
                            DO l=1,Face % TYPE % NumberOfNodes
                                IF ( Face % NodeIndexes(l) == Element % NodeIndexes(k) ) n=n+1
                            END DO
                        END DO
                        IF ( n /= Face % TYPE % NumberOfNodes ) CYCLE

                        DO k=1,Face % BDOFs
                            n = Solver % Mesh % NumberofNodes + &
                            Solver % Mesh % MaxEdgeDOFs * Solver % Mesh % NumberOfEdges + &
                            (Parent % FaceIndexes(j)-1) * Solver % Mesh % MaxFaceDOFs + k
                            n = x % Perm( n )
                            IF ( n <= 0 ) CYCLE
                            n = x % DOFs*(n-1) + DOF
                            CALL CRS_ZeroRow( A, n )
                            A % RHS(n) = 0.0d0
                        END DO
                    END DO
                END IF
!------------------------------------------------------------------------------          
            END DO ! i
!------------------------------------------------------------------------------
        END DO ! DOF
!------------------------------------------------------------------------------
        CALL Info('NumaHeatSolve: ', &
            'Setting Dirichlet boundary conditions', Level=5)
!------------------------------------------------------------------------------
!     Set Dirichlet dofs for edges and faces
!------------------------------------------------------------------------------
        DO DOF=1,x % DOFs
!------------------------------------------------------------------------------     
!           Get the name of the DOF:
!------------------------------------------------------------------------------     
            name = x % name
            IF ( x % DOFs > 1 ) name = ComponentName(name,DOF)
!------------------------------------------------------------------------------     
!           Set nodal loads:
!------------------------------------------------------------------------------     
            !CALL SetNodalLoads( CurrentModel,A, b, &
            !   Name,DOF,x % DOFs,x % Perm )
!------------------------------------------------------------------------------     
!           Set Dirichlet Conditions for Boundaries:
!------------------------------------------------------------------------------     
            CALL NumaSetDirichletBoundaries( CurrentModel, A, b, &
                Name, DOF, x % DOFs, x % Perm )
!------------------------------------------------------------------------------
!       Set Dirichlet conditions for interior zones
!------------------------------------------------------------------------------ 
            CALL NumaSetInteriorDirichletConditions( CurrentModel, A, b, &
                Name, DOF, x % DOFs, x % Perm )
            SaveElement => CurrentModel % CurrentElement
!------------------------------------------------------------------------------
!        Dirichlet BCs for face & edge DOFs:
!-------------------------------------------------------------------------------
            DO i=1,Solver % Mesh % NumberOfBoundaryElements
!------------------------------------------------------------------------------         
                Element => GetBoundaryElement(i)
                IF ( .NOT. ActiveBoundaryElement() ) CYCLE
!------------------------------------------------------------------------------
!               Get the BC associated to this element
!               Check if the BC exists
!------------------------------------------------------------------------------
                BC => GetBC()
                IF ( .NOT. ASSOCIATED( BC ) ) CYCLE
                IF ( .NOT. ListCheckPresent(BC, Name) ) CYCLE
!------------------------------------------------------------------------------
!           Get parent element:
!------------------------------------------------------------------------------
                Parent => Element % BoundaryInfo % Left
                IF ( .NOT. ASSOCIATED( Parent ) ) THEN
                    Parent => Element % BoundaryInfo % Right
                END IF
                IF ( .NOT. ASSOCIATED( Parent ) )   CYCLE
                IF ( .NOT. ASSOCIATED( Parent % pDefs ) ) CYCLE
!------------------------------------------------------------------------------
!               
!------------------------------------------------------------------------------
                n = Element % Type % NumberOfNodes
                DO j=1,n
                    l = Element % NodeIndexes(j)
                    Work(j)  = ListGetConstReal( BC, Name, Found, &
                        CurrentModel % Mesh % Nodes % x(l), &
                        CurrentModel % Mesh % Nodes % y(l), &
                        CurrentModel % Mesh % Nodes % z(l) )
                END DO
!------------------------------------------------------------------------------
                SELECT CASE(Parent % Type % Dimension)
!------------------------------------------------------------------------------
                    CASE(2)
!------------------------------------------------------------------------------
                    ! If no edges do not try to set boundary conditions
                    IF ( .NOT. ASSOCIATED( Solver % Mesh % Edges ) ) CYCLE

                    ! If boundary edge has no dofs move on to next edge
                    IF (Element % BDOFs <= 0) CYCLE

                    ! Number of nodes for this element
                    n = Element % TYPE % NumberOfNodes

                    ! Get indexes for boundary and values for dofs associated to them
                    CALL getBoundaryIndexes( Solver % Mesh, Element, Parent, gInd, numEdgeDofs )
                    CALL LocalBcBDOFs( BC, Element, numEdgeDofs, Name, STIFF, Work )

                    ! Contribute this boundary to global system
                    ! (i.e solve global boundary problem)
                    DO k=n+1,numEdgeDofs
                        nb = x % Perm( gInd(k) )
                        IF ( nb <= 0 ) CYCLE
                        nb = x % DOFs * (nb-1) + DOF
                        A % RHS(nb) = A % RHS(nb) + Work(k)
                        DO l=1,numEdgeDofs
                            mb = x % Perm( gInd(l) )
                            IF ( mb <= 0 ) CYCLE
                            mb = x % DOFs * (mb-1) + DOF
                            DO kk=A % Rows(nb)+DOF-1,A % Rows(nb+1)-1,x % DOFs
                                IF ( A % Cols(kk) == mb ) THEN
                                    A % Values(kk) = A % Values(kk) + STIFF(k,l)
                                    EXIT
                                END IF
                            END DO
                        END DO
                    END DO
!------------------------------------------------------------------------------                 
                    CASE(3)
!------------------------------------------------------------------------------
                    ! If no faces present do not try to set boundary conditions
                    ! @todo This should be changed to EXIT
                    IF ( .NOT. ASSOCIATED( Solver % Mesh % Faces ) ) CYCLE

                    ! Parameters of element
                    n = Element % TYPE % NumberOfNodes

                    ! Get global boundary indexes and solve dofs associated to them
                    CALL getBoundaryIndexes( Solver % Mesh, Element,  &
                        Parent, gInd, numEdgeDofs )
                    ! If boundary face has no dofs skip to next boundary element
                    IF (numEdgeDOFs == n) CYCLE

                    ! Get local solution
                    CALL LocalBcBDofs( BC, Element, numEdgeDofs, Name, STIFF, Work )

                    ! Contribute this entry to global boundary problem
                    DO k=n+1, numEdgeDOFs
                        nb = x % Perm( gInd(k) )
                        IF ( nb <= 0 ) CYCLE
                        nb = x % DOFs * (nb-1) + DOF
                        A % RHS(nb) = A % RHS(nb) + Work(k)
                        DO l=1, numEdgeDOFs
                            mb = x % Perm( gInd(l) )
                            IF ( mb <= 0 ) CYCLE
                            mb = x % DOFs * (mb-1) + DOF
                            DO kk=A % Rows(nb)+DOF-1,A % Rows(nb+1)-1,x % DOFs
                                IF ( A % Cols(kk) == mb ) THEN
                                    A % Values(kk) = A % Values(kk) + STIFF(k,l)
                                    EXIT
                                END IF
                            END DO
                        END DO
                    END DO
!------------------------------------------------------------------------------
                END SELECT
!------------------------------------------------------------------------------
        END DO ! elements
!------------------------------------------------------------------------------
        CurrentModel % CurrentElement => SaveElement
!------------------------------------------------------------------------------        
        END DO ! DOFs
!------------------------------------------------------------------------------
        CALL Info('NumaHeatSolve: ', &
            'Dirichlet boundary conditions set', Level=5)
!------------------------------------------------------------------------------
    END SUBROUTINE NumaDefaultDirichletConditions
!------------------------------------------------------------------------------




!------------------------------------------------------------------------------
    SUBROUTINE NumaSetInteriorDirichletConditions( Model, A, b, Name, DOF, NDOFs, Perm )
!------------------------------------------------------------------------------
!******************************************************************************
!
! Set dirichlet boundary condition for non boundary elements
!
! TYPE(Model_t) :: Model
!   INPUT: the current model structure
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
! INTEGER :: Perm(:)
!   INPUT: The node reordering info, this has been generated at the
!          beginning of the simulation for bandwidth optimization
!******************************************************************************
!------------------------------------------------------------------------------
        TYPE(Model_t) :: Model
        TYPE(Matrix_t), POINTER :: A
        REAL(KIND=dp) :: b(:)
        CHARACTER(LEN=*) :: Name 
        INTEGER :: DOF, NDOFs, Perm(:)
!------------------------------------------------------------------------------
!       Local variables
!------------------------------------------------------------------------------
        TYPE(ValueList_t), POINTER :: ValueList
        
        INTEGER, POINTER :: NodeIndexes(:), IndNodes(:)
        INTEGER :: BC,i,j,n, NoNodes, NOFNodesFound, dim, Nb_Target_Spherical
        
        LOGICAL :: GotIt, NodesFound, Interior, Target_spherical, Interior1, Interior2
        REAL(KIND=dp) ::  s, min_x, min_y, max_x, max_y,min_z,max_z, dist
        REAL(KIND=dp), POINTER :: c_x_Array(:,:), c_y_Array(:,:), c_z_Array(:,:), &
            radius_Array(:,:)
!------------------------------------------------------------------------------
!       Dimension of the model
!------------------------------------------------------------------------------     
        dim=CoordinateSystemDimension()
!------------------------------------------------------------------------------
!     Go through the BCs 
!------------------------------------------------------------------------------
        DO BC=1,Model % NumberOfBCs
!------------------------------------------------------------------------------ 
!           Check if the variable is concerned by this BC
!------------------------------------------------------------------------------      
            IF( .NOT. ListCheckPresent( Model % BCs(BC) % Values,Name )) CYCLE
!------------------------------------------------------------------------------         
            NodesFound = .FALSE.
!------------------------------------------------------------------------------
!           The areas in which Dirichlet conditions have to be applied are defined either 
!           in terms of coordinates or in terms of nodes.
!           At the first calling the list of coordinates is transformed to a list of nodes 
!------------------------------------------------------------------------------
            IF(.NOT. NodesFound) THEN
!------------------------------------------------------------------------------            
                ALLOCATE( IndNodes(Model % NumberOfNodes) )
                IndNodes = -1
                NoNodes=0
                GotIt = .FALSE.
!------------------------------------------------------------------------------ 
!               Check if the target zone is spherical 
!------------------------------------------------------------------------------                 
                Target_spherical = GetLogical( Model % BCs(BC) % Values,'Target Spherical',GotIt )      
                IF ( .NOT. GotIt ) Target_spherical = .FALSE.
                IF(Target_spherical) THEN
!------------------------------------------------------------------------------ 
!                   Check if several spherical zones are treated:
!------------------------------------------------------------------------------ 
                    Nb_Target_Spherical = GetInteger( Model % BCs(BC) % Values,'Nb Target Spherical',GotIt )        
                    IF ( .NOT. GotIt ) THEN 
                        Nb_Target_Spherical = 1
                    ELSE
                        IF ( Nb_Target_Spherical < 1 ) THEN 
                            PRINT*,'Nb Target Spherical = ',Nb_Target_Spherical
                            CALL Fatal( 'NumaSetInteriorDirichletConditions', 'Check Nb Target Spherical!' )
                        END IF
                    END IF
                    ALLOCATE( c_x_Array(Nb_Target_Spherical,1), c_y_Array(Nb_Target_Spherical,1), &
                        c_z_Array(Nb_Target_Spherical,1), radius_Array(Nb_Target_Spherical,1) )
!------------------------------------------------------------------------------ 
!                   Read the centre coordinates and radius for spherical areas in which a 
!                   Dirichlet BC has to be set
!                   These areas are then defined by
!                   (x-x_c)^2 + (y-y_c)^2 + (z-z_c)^2 < radius^2
!------------------------------------------------------------------------------                     
                    min_x=1
                    max_x=0
                    min_y=1
                    max_y=0
                    min_z=1
                    max_z=0
!------------------------------------------------------------------------------                                 
                    c_x_Array => ListGetConstRealArray( Model % BCs(BC) % Values, 'Target CentreX', GotIt )
                    IF (.NOT. GotIt) CYCLE
                    
                    c_y_Array => ListGetConstRealArray( Model % BCs(BC) % Values, 'Target CentreY', GotIt )
                    IF (.NOT. GotIt) CYCLE

                    radius_Array => ListGetConstRealArray( Model % BCs(BC) % Values, 'Target Radius', GotIt )
                    IF (.NOT. GotIt) CYCLE

                    IF (dim>2) THEN
                        c_z_Array => ListGetConstRealArray( Model % BCs(BC) % Values, 'Target CentreZ', GotIt )
                        IF (.NOT. GotIt) CYCLE
                    END IF
!------------------------------------------------------------------------------                     
                ELSE
!------------------------------------------------------------------------------ 
!                   Read the upper/lower bounds of coordinates for areas in which a 
!                   Dirichlet BC has to be set
!                   These areas are then defined by
!                   min_x < x < max_x, min_y < y < max_y, min_z < z < max_z
!------------------------------------------------------------------------------                                             
                    min_x =  ListGetConstReal(  Model % BCs(BC) % Values, 'Target min x',GotIt)
                    IF (.NOT. GotIt) CYCLE
                    min_y =  ListGetConstReal(  Model % BCs(BC) % Values, 'Target min y',GotIt)
                    IF (.NOT. GotIt) CYCLE
                    max_x =  ListGetConstReal(  Model % BCs(BC) % Values, 'Target max x',GotIt)
                    IF (.NOT. GotIt) CYCLE
                    max_y =  ListGetConstReal(  Model % BCs(BC) % Values, 'Target max y',GotIt)
                    IF (.NOT. GotIt) CYCLE
                    IF (dim>2) THEN
                        min_z =  ListGetConstReal(  Model % BCs(BC) % Values, 'Target min z',GotIt)
                        IF (.NOT. GotIt) CYCLE
                        max_z =  ListGetConstReal(  Model % BCs(BC) % Values, 'Target max z',GotIt)
                        IF (.NOT. GotIt) CYCLE
                    END IF
!------------------------------------------------------------------------------
                END IF
!------------------------------------------------------------------------------ 
!               Go through the nodes of the model and check for each if it belongs 
!               to an area with Dirichlet BC
!------------------------------------------------------------------------------             
                DO i=1,Model % NumberOfNodes
!------------------------------------------------------------------------------ 
!                       Check if the element is active 
!------------------------------------------------------------------------------    
                    IF( Perm(i) == 0) CYCLE
                    Interior = .FALSE.
!------------------------------------------------------------------------------ 
!                   Comparisons between node coordinates and area bounds in spherical case
!------------------------------------------------------------------------------     
                    Interior1 = .FALSE.
!------------------------------------------------------------------------------
                    IF(Target_spherical) THEN
!------------------------------------------------------------------------------
                        DO j=1,Nb_Target_Spherical
                            dist = (Model % Mesh % Nodes % x(i)-c_x_Array(j,1))**2 + &
                                (Model % Mesh % Nodes % y(i)-c_y_Array(j,1))**2 
                            IF (dim>2) THEN 
                                dist = dist + (Model % Mesh % Nodes % z(i)-c_z_Array(j,1))**2 
                            END IF
                            Interior1= Interior1 .OR. (dist<=radius_Array(j,1)**2)
                        END DO
!------------------------------------------------------------------------------
                    END IF
!------------------------------------------------------------------------------ 
!                   Comparisons between node coordinates and area bounds in non-spherical case
!------------------------------------------------------------------------------    
                    Interior2= (Model % Mesh % Nodes % x(i)>min_x) .AND. &
                        (Model % Mesh % Nodes % x(i)<max_x)
                    Interior2= Interior2 .AND. (Model % Mesh % Nodes % y(i)>min_y) .AND. &
                        (Model % Mesh % Nodes % y(i)<max_y)
                    IF (dim>2) THEN
                        Interior2= Interior2 .AND. (Model % Mesh % Nodes % z(i)>min_z) .AND. &
                        (Model % Mesh % Nodes % z(i)<max_z)
                    END IF
!------------------------------------------------------------------------------ 
                    Interior = Interior1 .OR. Interior2                 
!------------------------------------------------------------------------------ 
!                   Update the number of nodes for the BC, and the vector of these nodes
!------------------------------------------------------------------------------
                    IF( Interior ) THEN
                        NoNodes = NoNodes+1
                        IndNodes(NoNodes) = i
                    END IF
!------------------------------------------------------------------------------
                END DO ! Model % NumberOfNodes
!------------------------------------------------------------------------------         
!               Deallocate local arrays:
!------------------------------------------------------------------------------
                IF(Target_spherical) DEALLOCATE( c_x_Array, c_y_Array, c_z_Array, radius_Array )
!--------------------------------------------------------------------------     
!               Check if all the selected nodes are active
!------------------------------------------------------------------------------         
                NOFNodesFound = 0
                DO j=1,NoNodes
                    IF ( IndNodes(j)>0 ) THEN
                        NOFNodesFound=NOFNodesFound+1
                        IndNodes(NOFNodesFound) = IndNodes(j)
                    END IF
                END DO
!------------------------------------------------------------------------------ 
!               In the first time add the found nodes to the list structure
!------------------------------------------------------------------------------ 
                IF ( NOFNodesFound > 0 ) THEN
                    CALL ListAddIntegerArray( Model % BCs(BC) % Values,'Target Nodes', &
                        NOFNodesFound, IndNodes) 
                    DEALLOCATE(IndNodes)
                    NodesFound = .TRUE.               
                END IF  
!------------------------------------------------------------------------------
            END IF ! NOT NodesFound
!------------------------------------------------------------------------------
!           If the nodes are specified in the input file, or if the coordinates 
!        have been transformed in nodes, we apply the conditions:           
!------------------------------------------------------------------------------
            IF(NodesFound) THEN          
!------------------------------------------------------------------------------     
!               Read the nodes in the input file
!------------------------------------------------------------------------------             
                NodeIndexes => ListGetIntegerArray( Model % BCs(BC) % Values,'Target Nodes')
!------------------------------------------------------------------------------     
!               Get the number of nodes concerned be Dirichlet BC
!------------------------------------------------------------------------------                     
                n = SIZE(NodeIndexes)
!------------------------------------------------------------------------------     
!               Get the fixed values of the Dirichlet BC
!------------------------------------------------------------------------------     
                ValueList => Model % BCs(BC) % Values
!------------------------------------------------------------------------------     
!               Modify the system with the Dirichlet BC
!------------------------------------------------------------------------------     
                CALL NumaSetPointValues(A, b,Name, DOF,NDOFs,ValueList,n,NodeIndexes,Perm)
!------------------------------------------------------------------------------
            END IF ! NodesFound
!------------------------------------------------------------------------------
        END DO ! BC
!------------------------------------------------------------------------------
  END SUBROUTINE NumaSetInteriorDirichletConditions
!------------------------------------------------------------------------------




!------------------------------------------------------------------------------
    SUBROUTINE NumaSetDirichletBoundaries( Model, A, b, Name, DOF, NDOFs, Perm )
!------------------------------------------------------------------------------
!******************************************************************************
!
! Set dirichlet boundary condition for given dof
!
! TYPE(Model_t) :: Model
!   INPUT: the current model structure
!
! TYPE(Matrix_t), POINTER :: A
!   INOUT: The global matrix
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
! INTEGER :: Perm(:)
!   INPUT: The node reordering info, this has been generated at the
!          beginning of the simulation for bandwidth optimization
!******************************************************************************
!------------------------------------------------------------------------------
        TYPE(Model_t) :: Model
        TYPE(Matrix_t), POINTER :: A

        REAL(KIND=dp) :: b(:)

        CHARACTER(LEN=*) :: Name 
        INTEGER :: DOF, NDOFs, Perm(:)
!------------------------------------------------------------------------------
    TYPE(Element_t), POINTER :: Element
    INTEGER, POINTER :: NodeIndexes(:), IndNodes(:)
    INTEGER, ALLOCATABLE :: Indexes(:)
    INTEGER :: BC,i,j,k,l,n,t,k1,k2, ModifDBC_Index
    LOGICAL :: GotIt, periodic, OrderByBCNumbering
    REAL(KIND=dp), POINTER :: WorkA(:,:,:) => NULL()
    REAL(KIND=dp) ::  s, ModifDBC_DistMax, ModifDBC_xc, ModifDBC_yc, ModifDBC_zc, &
            ModifDBC_DiamMax, Diameter, ModifDBC_TimeStart

    LOGICAL :: Conditional, ModifDBC, ModifDBCToFluxBC, SelectElement
    LOGICAL, ALLOCATABLE :: DonePeriodic(:)
    CHARACTER(LEN=MAX_NAME_LEN) :: CondName, PassName

    INTEGER :: NoNodes,NoDims,bf_id,nlen, NOFNodesFound
    REAL(KIND=dp), POINTER :: CoordNodes(:,:), Condition(:), Work(:)
    REAL(KIND=dp) :: MinDist,Dist, Eps
    LOGICAL, ALLOCATABLE :: ActivePart(:), ActiveCond(:), ActivePartAll(:)
    LOGICAL :: NodesFound, Passive
    TYPE(ValueList_t), POINTER :: ValueList, SolverParams

        TYPE(Nodes_t) :: Nodes      


!------------------------------------------------------------------------------
!   These logical vectors are used to minimize extra effort in setting up different BCs
!------------------------------------------------------------------------------
    nlen = LEN_TRIM(Name)

    n = MAX( Model % NumberOfBodyForces,Model % NumberOfBCs)
    ALLOCATE( ActivePart(n), ActivePartAll(n), ActiveCond(n))
    CondName = Name(1:nlen) // ' Condition'
    PassName = Name(1:nlen) // ' Passive'
 
    ALLOCATE( Indexes(Model % Mesh % MaxElementDOFs) )
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!   Check if the BC will have to be modified at some elements
!------------------------------------------------------------------------------
        ModifDBC = .FALSE.
        ModifDBCToFluxBC = .FALSE.

        SolverParams => GetSolverParams()
        ModifDBC = ListGetLogical( SolverParams, 'Dirichlet Modified BC ', GotIt )
!------------------------------------------------------------------------------
        IF (ModifDBC) THEN
!------------------------------------------------------------------------------
            ModifDBCToFluxBC = ListGetLogical( SolverParams, &
                'Dirichlet Modified BC To Flux BC', GotIt )
            !------------------------------------------------------------------------------
            IF (.NOT. ModifDBCToFluxBC) THEN
                CALL Info( 'NumaSetDirichletBoundaries', 'Setting Modified Dirichlet BC' )
            END IF
            ALLOCATE(Nodes % x(n), Nodes % y(n), Nodes % z(n))
            !------------------------------------------------------------------------------
            ! Index of the boundary condition where is specified the modification:
            !------------------------------------------------------------------------------
            ModifDBC_Index = GetInteger( SolverParams, 'Dirichlet Modified BC Index', GotIt )
            IF (ModifDBC_Index>Model % NumberOfBCs) THEN
                CALL Fatal( 'NumaSetDirichletBoundaries', 'Check Dirichlet Modified BC Index!' )
            END IF
            !------------------------------------------------------------------------------
            ! Distance from target under which is applied the modification:
            !------------------------------------------------------------------------------
            ModifDBC_DistMax = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                'Dirichlet Modified BC Distance Max', GotIt )
            IF (.NOT. GotIt) THEN
                CALL Error( 'NumaSetDirichletBoundaries', 'Check Dirichlet Modified BC Distance Max!' )
                ModifDBC_DistMax = 0.0 
            END IF
            !------------------------------------------------------------------------------
            ! x-coord of the target around which is applied the modification:
            !------------------------------------------------------------------------------
            ModifDBC_xc = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                'Dirichlet Modified BC xc', GotIt )
            IF (.NOT. GotIt) THEN
                CALL Error( 'NumaSetDirichletBoundaries', 'Check Dirichlet Modified BC xc!' )
            END IF
            !------------------------------------------------------------------------------
            ! y-coord of the target around which is applied the modification:
            !------------------------------------------------------------------------------
            ModifDBC_yc = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                'Dirichlet Modified BC yc', GotIt )
            IF (.NOT. GotIt) THEN
                CALL Error( 'NumaSetDirichletBoundaries', 'Check Dirichlet Modified BC yc!' )
            END IF
            !------------------------------------------------------------------------------
            ! z-coord of the target around which is applied the modification:
            !------------------------------------------------------------------------------
            ModifDBC_zc = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                'Dirichlet Modified BC zc', GotIt )
            IF (.NOT. GotIt) THEN
                CALL Error( 'NumaSetDirichletBoundaries', 'Check Dirichlet Modified BC zc!' )
            END IF
            !------------------------------------------------------------------------------
            ! Diameter max of the elements on which is applied the modification:
            !------------------------------------------------------------------------------
            ModifDBC_DiamMax = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                'Dirichlet Modified BC Diameter Max', GotIt )
            IF (.NOT. GotIt) THEN
                CALL Info( 'NumaSetDirichletBoundaries', 'No Dirichlet Modified BC Diameter Max specified' )
                ModifDBC_DiamMax = -1.0 
            END IF
            !------------------------------------------------------------------------------
            ! Start time at which is applied the modification:
            !------------------------------------------------------------------------------
            ModifDBC_TimeStart = GetConstReal( Model % BCs(ModifDBC_Index) % Values, &
                'Dirichlet Modified BC Time Start', GotIt )
            IF (.NOT. GotIt) THEN
                ModifDBC_TimeStart = 0.0
            END IF
!------------------------------------------------------------------------------
        END IF
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!   Go through the periodic BCs and set the linear dependence
!------------------------------------------------------------------------------
        ActivePart = .FALSE.
!------------------------------------------------------------------------------
        DO BC=1,Model % NumberOfBCs
         IF ( .NOT. ListGetLogical( Model % BCs(BC) % Values, &
                 'Periodic BC ' // Name(1:nlen), GotIt ) ) ActivePart(BC) = .TRUE.
         IF ( .NOT. ListGetLogical( Model % BCs(BC) % Values, &
                 'Anti Periodic BC ' // Name(1:nlen), GotIt ) ) ActivePart(BC) = .TRUE.
        END DO
!------------------------------------------------------------------------------
        IF( ANY(ActivePart) ) THEN    
            ALLOCATE( DonePeriodic( Model % Mesh % NumberOFNodes ) )
            DonePeriodic = .FALSE.
            DO BC=1,Model % NumberOfBCs
                CALL NumaSetPeriodicBoundariesPass1( Model, A, b, Name, DOF, NDOFs, Perm, BC, DonePeriodic )
            END DO

            DonePeriodic = .FALSE.
            DO BC=1,Model % NumberOfBCs
                CALL NumaSetPeriodicBoundariesPass2( Model, A, b, Name, DOF, NDOFs, Perm, BC, DonePeriodic )
            END DO
            DEALLOCATE( DonePeriodic ) 
        END IF
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
!   Go through the normal Dirichlet BCs applied on the boundaries
!------------------------------------------------------------------------------
    ActivePart = .FALSE.
    ActiveCond = .FALSE.
    ActivePartAll = .FALSE.
!------------------------------------------------------------------------------
    DO BC=1,Model % NumberOfBCs
      ActivePartAll(BC) = ListCheckPresent( &
            Model % BCs(bc) % Values, Name(1:nlen) // ' DOFs' )
      ActivePart(BC) = ListCheckPresent( Model % BCs(bc) % Values, Name ) 
      ActiveCond(BC) = ListCheckPresent( Model % BCs(bc) % Values, CondName )      
    END DO
!------------------------------------------------------------------------------
    IF( ANY(ActivePart) .OR. ANY(ActivePartAll) ) THEN    
      OrderByBCNumbering = ListGetLogical( Model % Simulation, &
          'Set Dirichlet BCs by BC Numbering', gotIt)
!------------------------------------------------------------------------------
      IF ( OrderByBCNumbering ) THEN
!------------------------------------------------------------------------------
!               Go through Boundaries
!------------------------------------------------------------------------------
        DO BC=1,Model % NumberOfBCs
!------------------------------------------------------------------------------
          IF(.NOT. ActivePart(BC) .AND. .NOT. ActivePartAll(BC) ) CYCLE
          Conditional = ActiveCond(BC)
!------------------------------------------------------------------------------
!                   Go through Boundary elements
!------------------------------------------------------------------------------
          DO t = Model % NumberOfBulkElements + 1, &
              Model % NumberOfBulkElements + Model % NumberOfBoundaryElements
!------------------------------------------------------------------------------            
            Element => Model % Elements(t)
!------------------------------------------------------------------------------
!                       Check that this is the boundary associated to the element:
!------------------------------------------------------------------------------
            IF ( Element % BoundaryInfo % Constraint /= Model % BCs(BC) % Tag ) CYCLE
           
            Model % CurrentElement => Element
!------------------------------------------------------------------------------
!                       Get element description:
!------------------------------------------------------------------------------
            IF ( ActivePart(BC) ) THEN
              n = Element % Type % NumberOfNodes
              Indexes(1:n) = Element % NodeIndexes
            ELSE
              n = SgetElementDOFs( Indexes )
            END IF
!------------------------------------------------------------------------------
!                       Get the values to apply on the boundary:
!------------------------------------------------------------------------------
            ValueList => Model % BCs(BC) % Values

!------------------------------------------------------------------------------
!                       If locally modified boundary condition:
!------------------------------------------------------------------------------
                        IF (ModifDBC) THEN
!------------------------------------------------------------------------------
                            SelectElement = .FALSE.
!------------------------------------------------------------------------------
!                           Selection test for locally modified boundary condition:
!------------------------------------------------------------------------------
                            Nodes % x(1:n) = Solver % Mesh % Nodes % x(Element % NodeIndexes)
                            Nodes % y(1:n) = Solver % Mesh % Nodes % y(Element % NodeIndexes)
                            Nodes % z(1:n) = Solver % Mesh % Nodes % z(Element % NodeIndexes)
!------------------------------------------------------------------------------
!                           Compute the maximum distance from the nodes to the target:
!------------------------------------------------------------------------------
                            Dist = sqrt( (Nodes % x(1)-ModifDBC_xc)**2 + &
                                (Nodes % y(1)-ModifDBC_yc)**2 + &
                                (Nodes % z(1)-ModifDBC_zc)**2 )
                
                            DO i=2,n
                                Dist = Max (Dist, sqrt( (Nodes % x(i)-ModifDBC_xc)**2 + &
                                    (Nodes % y(i)-ModifDBC_yc)**2 + &
                                    (Nodes % z(i)-ModifDBC_zc)**2 ) )
                            END DO
!------------------------------------------------------------------------------
!                           Check the condition on distance from the target and time:
!------------------------------------------------------------------------------
                            IF ( ((Dist<ModifDBC_DistMax) .OR. (ModifDBC_DistMax<0.0)) .AND. (Time>=ModifDBC_TimeStart) ) THEN
!------------------------------------------------------------------------------
!                               Compute the diameter of the element:
!------------------------------------------------------------------------------                             
                                Diameter = ElementDiameter( Element, Nodes )
!------------------------------------------------------------------------------
!                               Check the condition on the diameter of the element:
!------------------------------------------------------------------------------
                                IF ( (Diameter<ModifDBC_DiamMax) .OR. (ModifDBC_DiamMax<0.0) ) THEN
!------------------------------------------------------------------------------
                                    SelectElement = .TRUE.
!------------------------------------------------------------------------------
                                END IF ! diameter
!------------------------------------------------------------------------------
                            END IF ! distance and time
!------------------------------------------------------------------------------
!                           Element is selected and Dirichlet BC is modified into Dirichlet BC:
!------------------------------------------------------------------------------
                            IF (SelectElement .AND. .NOT. ModifDBCToFluxBC) THEN
!------------------------------------------------------------------------------
!                               Get the modified value in input file:
!------------------------------------------------------------------------------
                                ValueList => Model % BCs(ModifDBC_Index) % Values
!------------------------------------------------------------------------------
                            END IF
!------------------------------------------------------------------------------
!                           Dirichlet BC is modified into Dirichlet BC, or DBC is modified into 
!                           NBC but element not selected:
!------------------------------------------------------------------------------
                            IF (.NOT. ModifDBCToFluxBC .OR. (ModifDBCToFluxBC .AND. .NOT.SelectElement)) THEN
!------------------------------------------------------------------------------
!                               Use the dirichlet boundary values (modified) to update the system
!------------------------------------------------------------------------------
                                CALL NumaSetElementValues(A, b,Name, DOF,NDOFs,ValueList,n,Indexes,Perm)
!------------------------------------------------------------------------------
                            END IF
!------------------------------------------------------------------------------
                        ELSE
!------------------------------------------------------------------------------
!                           Use the dirichlet boundary values (not modified) to update the system
!------------------------------------------------------------------------------
                CALL NumaSetElementValues(A, b,Name, DOF,NDOFs,ValueList,n,Indexes,Perm)
!------------------------------------------------------------------------------
                        END IF ! ModifDBC
!------------------------------------------------------------------------------
          END DO !Boundary elements
!------------------------------------------------------------------------------
        END DO !BC
!------------------------------------------------------------------------------
      ELSE
!------------------------------------------------------------------------------
!               Go through Boundary elements
!------------------------------------------------------------------------------
        DO t = Model % NumberOfBulkElements + 1, &
            Model % NumberOfBulkElements + Model % NumberOfBoundaryElements
!------------------------------------------------------------------------------
!                   Go through Boundaries
!------------------------------------------------------------------------------
          DO BC=1,Model % NumberOfBCs
!------------------------------------------------------------------------------
            IF(.NOT. ActivePart(BC) .AND. .NOT. ActivePartAll(BC) ) CYCLE
            Conditional = ActiveCond(BC)   
            Element => Model % Elements(t)
!------------------------------------------------------------------------------
!                       Check that this is the boundary associated to the element:
!------------------------------------------------------------------------------
            IF ( Element % BoundaryInfo % Constraint /= Model % BCs(BC) % Tag ) CYCLE
            
            Model % CurrentElement => Element
!------------------------------------------------------------------------------
!                       Get element description:
!------------------------------------------------------------------------------
            IF ( ActivePart(BC) ) THEN
              n = Element % Type % NumberOfNodes
              Indexes(1:n) = Element % NodeIndexes
            ELSE
              n = SgetElementDOFs( Indexes )
            END IF
!------------------------------------------------------------------------------
!                       Get the values to apply on the boundary:
!------------------------------------------------------------------------------
            ValueList => Model % BCs(BC) % Values

!------------------------------------------------------------------------------
!                       If locally modified boundary condition:
!------------------------------------------------------------------------------
                        IF (ModifDBC) THEN
!------------------------------------------------------------------------------
                            SelectElement = .FALSE.
!------------------------------------------------------------------------------
!                           Selection test for locally modified boundary condition:
!------------------------------------------------------------------------------
                            Nodes % x(1:n) = Solver % Mesh % Nodes % x(Element % NodeIndexes)
                            Nodes % y(1:n) = Solver % Mesh % Nodes % y(Element % NodeIndexes)
                            Nodes % z(1:n) = Solver % Mesh % Nodes % z(Element % NodeIndexes)
!------------------------------------------------------------------------------
!                           Compute the maximum distance from the nodes to the target:
!------------------------------------------------------------------------------
                            Dist = sqrt( (Nodes % x(1)-ModifDBC_xc)**2 + &
                                (Nodes % y(1)-ModifDBC_yc)**2 + &
                                (Nodes % z(1)-ModifDBC_zc)**2 )
                
                            DO i=2,n
                                Dist = Max (Dist, sqrt( (Nodes % x(i)-ModifDBC_xc)**2 + &
                                    (Nodes % y(i)-ModifDBC_yc)**2 + &
                                    (Nodes % z(i)-ModifDBC_zc)**2 ) )
                            END DO
!------------------------------------------------------------------------------
!                           Check the condition on distance from the target and time:
!------------------------------------------------------------------------------
                            IF ( ((Dist<ModifDBC_DistMax) .OR. (ModifDBC_DistMax<0.0)) .AND. (Time>=ModifDBC_TimeStart) ) THEN
!------------------------------------------------------------------------------
!                               Compute the diameter of the element:
!------------------------------------------------------------------------------                             
                                Diameter = ElementDiameter( Element, Nodes )
!------------------------------------------------------------------------------
!                               Check the condition on the diameter of the element:
!------------------------------------------------------------------------------
                                IF ( (Diameter<ModifDBC_DiamMax) .OR. (ModifDBC_DiamMax<0.0) ) THEN
!------------------------------------------------------------------------------
                                    SelectElement = .TRUE.
!------------------------------------------------------------------------------
                                END IF ! diameter
!------------------------------------------------------------------------------
                            END IF ! distance and time
!------------------------------------------------------------------------------
!                           Element is selected and Dirichlet BC is modified into Dirichlet BC:
!------------------------------------------------------------------------------
                            IF (SelectElement .AND. .NOT. ModifDBCToFluxBC) THEN
!------------------------------------------------------------------------------
!                               Get the modified value in input file:
!------------------------------------------------------------------------------
                                ValueList => Model % BCs(ModifDBC_Index) % Values
!------------------------------------------------------------------------------
                            END IF
!------------------------------------------------------------------------------
!                           Dirichlet BC is modified into Dirichlet BC, or DBC is modified into 
!                           NBC but element not selected:
!------------------------------------------------------------------------------
                            IF (.NOT. ModifDBCToFluxBC .OR. (ModifDBCToFluxBC .AND. .NOT.SelectElement)) THEN
!------------------------------------------------------------------------------
!                               Use the dirichlet boundary values (modified) to update the system
!------------------------------------------------------------------------------
                                CALL NumaSetElementValues(A, b,Name, DOF,NDOFs,ValueList,n,Indexes,Perm)
!------------------------------------------------------------------------------
                            END IF
!------------------------------------------------------------------------------
                        ELSE
!------------------------------------------------------------------------------
!                           Use the dirichlet boundary values (not modified) to update the system
!------------------------------------------------------------------------------
                CALL NumaSetElementValues(A, b,Name, DOF,NDOFs,ValueList,n,Indexes,Perm)
!------------------------------------------------------------------------------
                        END IF ! ModifDBC
!------------------------------------------------------------------------------
          END DO !BC
!------------------------------------------------------------------------------
        END DO !Boundary elements
!------------------------------------------------------------------------------
      END IF !OrderByBCNumbering
!------------------------------------------------------------------------------
    END IF !ActivePart or ActivePartAll
!------------------------------------------------------------------------------
        IF (ModifDBC) DEALLOCATE(Nodes % x, Nodes % y, Nodes % z)
!------------------------------------------------------------------------------




!------------------------------------------------------------------------------
!   Go through the Dirichlet conditions in the body force lists
!------------------------------------------------------------------------------ 
    ActivePart = .FALSE.
    ActiveCond = .FALSE.
    ActivePartAll = .FALSE.
    Passive = .FALSE.
!------------------------------------------------------------------------------
    DO bf_id=1,Model % NumberOFBodyForces
      ActivePart(bf_id) = ListCheckPresent( Model % BodyForces(bf_id) % Values, Name ) 
      ActivePartAll(bf_id) = ListCheckPresent( &
           Model % BodyForces(bf_id) % Values, Name(1:nlen) // ' DOFs' ) 
      ActiveCond(bf_id) = ListCheckPresent( Model % BodyForces(bf_id) % Values,CondName )      

      Passive = Passive .OR. ListCheckPresent( Model % BodyForces(bf_id) % Values, &
           PassName )
    END DO !bf_id 
!------------------------------------------------------------------------------  
    IF ( ANY( ActivePart ) .OR. ANY(ActivePartAll) ) THEN
!------------------------------------------------------------------------------
!           Go through Bulk elements
!------------------------------------------------------------------------------
      DO t = 1, Model % NumberOfBulkElements 
!------------------------------------------------------------------------------
        Element => Model % Elements(t)
        bf_id = ListGetInteger( Model % Bodies(Element % BodyId) % Values,'Body Force', GotIt)
        
        IF(.NOT. GotIt) CYCLE
        IF(.NOT. ActivePart(bf_id) .AND. .NOT. ActivePartAll(bf_id)) CYCLE
        Conditional = ActiveCond(bf_id)
        
        Model % CurrentElement => Element
        IF ( ActivePart(bf_id) ) THEN
          n = Element % Type % NumberOfNodes
          Indexes(1:n) = Element % NodeIndexes
        ELSE
          n = SgetElementDOFs( Indexes )
        END IF
        ValueList => Model % BodyForces(bf_id) % Values
!------------------------------------------------------------------------------
!               Use the boundary values to update the system
!------------------------------------------------------------------------------
        CALL NumaSetElementValues(A, b,Name, DOF,NDOFs,ValueList,n,Indexes,Perm)
!------------------------------------------------------------------------------
      END DO !Bulk Elements
!------------------------------------------------------------------------------
    END IF
!------------------------------------------------------------------------------
    DEALLOCATE(ActivePart, ActivePartAll, ActiveCond, Indexes)
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
!   Go through the pointwise Dirichlet BCs that are created on-the-fly
!   Note that it is best that the coordinates are transformed to nodes using 
!   the right variable. Otherwise it could point to nodes that are not active.
!------------------------------------------------------------------------------
    DO BC=1,Model % NumberOfBCs
!------------------------------------------------------------------------------      
      IF( .NOT. ListCheckPresent( Model % BCs(BC) % Values,Name )) CYCLE
      NodesFound = ListCheckPresent( Model % BCs(BC) % Values,'Target Nodes' )
!------------------------------------------------------------------------------      
!       The coordinates are only requested for a body that has no list of nodes. 
!       At the first calling the list of coorinates is transformed to list of nodes. 
!------------------------------------------------------------------------------
      IF(.NOT. NodesFound) THEN
!------------------------------------------------------------------------------
        CoordNodes => ListGetConstRealArray(Model % BCs(BC) % Values, &
                    'Target Coordinates',GotIt)
        IF(GotIt) THEN
          Eps = ListGetConstReal( Model % BCs(BC) % Values, &
                      'Target Coordinates Eps', Gotit )
          IF ( .NOT. GotIt ) THEN
            Eps = HUGE(Eps)
          ELSE
            Eps = Eps**2
          END IF

          NoNodes = SIZE(CoordNodes,1)
          NoDims = SIZE(CoordNodes,2)
          
          IF(NoNodes > 0) THEN               
            ALLOCATE( IndNodes(NoNodes) )
            IndNodes = -1
            DO j=1,NoNodes
              MinDist = HUGE(Dist)
              
              DO i=1,Model % NumberOfNodes
                IF( Perm(i) == 0) CYCLE
                
                Dist = (Model % Mesh % Nodes % x(i) - CoordNodes(j,1))**2.0 
                IF(NoDims >= 2) Dist = Dist + (Model % Mesh % Nodes % y(i) - CoordNodes(j,2))**2.0 
                IF(NoDims == 3) Dist = Dist + (Model % Mesh % Nodes % z(i) - CoordNodes(j,3))**2.0 
                Dist = SQRT(Dist)
                
                IF(Dist < MinDist .AND. Dist <= Eps ) THEN
                  MinDist = Dist
                  IndNodes(j) = i
                END IF
              END DO
            END DO

            NOFNodesFound = 0
            DO j=1,NoNodes
               IF ( IndNodes(j)>0 ) THEN
                 NOFNodesFound=NOFNodesFound+1
                 IndNodes(NOFNodesFound) = IndNodes(j)
               END IF
            END DO
            
            ! In the first time add the found nodes to the list structure
            IF ( NOFNodesFound > 0 ) THEN
              CALL ListAddIntegerArray( Model % BCs(BC) % Values,'Target Nodes', &
                  NOFNodesFound, IndNodes) 
              DEALLOCATE(IndNodes)
              NodesFound = .TRUE.               
            END IF
          END IF
        END IF
!------------------------------------------------------------------------------
      END IF !not NodesFound
!------------------------------------------------------------------------------      
      IF(NodesFound) THEN       
!------------------------------------------------------------------------------  
        Conditional = ListCheckPresent( Model % BCs(bc) % Values, CondName )      
        NodeIndexes => ListGetIntegerArray( Model % BCs(BC) % Values,'Target Nodes')
        n = SIZE(NodeIndexes)
        ValueList => Model % BCs(BC) % Values
!------------------------------------------------------------------------------
!               Use the point values to update the system
!------------------------------------------------------------------------------
        CALL NumaSetPointValues(A, b,Name, DOF,NDOFs,ValueList,n,NodeIndexes,Perm)
!------------------------------------------------------------------------------  
      END IF !NodesFound
!------------------------------------------------------------------------------
    END DO !BC
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
!   Take care of the matrix entries of passive elements
!------------------------------------------------------------------------------
    IF ( Passive ) THEN
       DO i = 1, A % NumberOfRows
          IF ( ABS(A % Values( A % Diag(i) ) ) < 1.0e-14 ) THEN
             A % Values( A % Diag(i) ) = 1.0d0
             b(i) = Model % Solver % Variable % Values(i)
          END IF
       END DO
    END IF !Passive
!------------------------------------------------------------------------------
  END SUBROUTINE NumaSetDirichletBoundaries
!------------------------------------------------------------------------------






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








!------------------------------------------------------------------------------  
    SUBROUTINE NumaSetPointValues(A,b,Name,DOF,NDOFs,ValueList,n,NodeIndexes,Perm)
!------------------------------------------------------------------------------ 
!******************************************************************************
!
! Set values related to individual points
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
!                   DOF>0
!------------------------------------------------------------------------------                 
                    IF ( DOF>0 ) THEN
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
                    ELSE
!------------------------------------------------------------------------------
!                       DOF<=0
!------------------------------------------------------------------------------     
                        DO l=1,MIN( NDOFs, SIZE(Worka,1) )
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
                        END DO ! l
!------------------------------------------------------------------------------                     
                    END IF ! DOF>0
!------------------------------------------------------------------------------
                END IF ! k>0
!------------------------------------------------------------------------------             
            END DO ! j
!------------------------------------------------------------------------------         
        END IF ! gotIt
!------------------------------------------------------------------------------
    END SUBROUTINE NumaSetPointValues
!------------------------------------------------------------------------------




!------------------------------------------------------------------------------
END SUBROUTINE NumaHeatSolver
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
FUNCTION TemperatureBoundaryResidual( Model, Edge, Mesh, Quant, Perm,Gnorm, NDOFs ) RESULT( Indicator )
!------------------------------------------------------------------------------
!******************************************************************************
!
! Compute an indicator concerning boundary edges for remeshing process
!
! ARGUMENTS:
!
!  TYPE(Model_t) :: Model,  
!     INPUT: All model information (mesh,materials,BCs,etc...)
!
!  TYPE(Element_t), POINTER :: Edge
!       INPUT: Structure describing the element (dimension,nof nodes,
!               interpolation degree, etc...)
!
!  TYPE( Mesh_t ), POINTER :: Mesh
!     INPUT: Current mesh
!
!   REAL(KIND=dp) :: Quant(:)
!       INPUT: Quantity computed on the mesh
!
!   INTEGER :: Perm(:)
!       INPUT: Element local numbering
!
!   REAL(KIND=dp) :: Gnorm
!       OUTPUT: volumic force norm
!
!   REAL(KIND=dp) :: Indicator(2)
!       OUTPUT: Remeshing criterion
!
!  INTEGER :: NDOFs
!   INPUT: Degrees of freedom of the solution Quant
!******************************************************************************
    USE ElementDescription
    IMPLICIT NONE
    !------------------------------------------------------------------------------
    TYPE(Model_t) :: Model
    INTEGER :: Perm(:), NDOFs
    REAL(KIND=dp) :: Quant(:), Indicator(2), Gnorm
    TYPE( Mesh_t ), POINTER    :: Mesh
    TYPE( Element_t ), POINTER :: Edge
    !------------------------------------------------------------------------------
    Gnorm = 0.0
    Indicator = 0.0
!------------------------------------------------------------------------------
END FUNCTION TemperatureBoundaryResidual
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
FUNCTION TemperatureEdgeResidual( Model, Edge, Mesh, Quant, Perm, NDOFs ) RESULT( Indicator )
!------------------------------------------------------------------------------
!******************************************************************************
!
! Compute an indicator concerning interior edges for remeshing process
!
! ARGUMENTS:
!
!  TYPE(Model_t) :: Model,  
!     INPUT: All model information (mesh,materials,BCs,etc...)
!
!  TYPE(Element_t), POINTER :: Edge
!       INPUT: Structure describing the element (dimension,nof nodes,
!               interpolation degree, etc...)
!
!  TYPE( Mesh_t ), POINTER :: Mesh
!     INPUT: Current mesh
!
!   REAL(KIND=dp) :: Quant(:)
!       INPUT: Quantity computed on the mesh
!
!   INTEGER :: Perm(:)
!       INPUT: Element local numbering
!
!   REAL(KIND=dp) :: Indicator(2)
!       OUTPUT: Remeshing criterion
!
!  INTEGER :: NDOFs
!   INPUT: Degrees of freedom of the solution Quant
!******************************************************************************
    USE ElementDescription
    IMPLICIT NONE
    !------------------------------------------------------------------------------
    TYPE(Model_t) :: Model
    INTEGER :: Perm(:), NDOFs
    REAL(KIND=dp) :: Quant(:), Indicator(2)
    TYPE( Mesh_t ), POINTER    :: Mesh
    TYPE( Element_t ), POINTER :: Edge
    !------------------------------------------------------------------------------
    Indicator = 0.0
!------------------------------------------------------------------------------
END FUNCTION TemperatureEdgeResidual
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
FUNCTION TemperatureInsideResidual( Model, Element, Mesh, &
    Quant, Perm, Fnorm, NDOFs ) RESULT( Indicator )
!------------------------------------------------------------------------------
!******************************************************************************
!
! Compute an indicator for remeshing process
!
! ARGUMENTS:
!
!  TYPE(Model_t) :: Model,  
!     INPUT: All model information (mesh,materials,BCs,etc...)
!
!  TYPE(Element_t), POINTER :: Element
!       INPUT: Structure describing the element (dimension,nof nodes,
!               interpolation degree, etc...)
!
!  TYPE( Mesh_t ), POINTER :: Mesh
!     INPUT: Current mesh
!
!   REAL(KIND=dp) :: Quant(:)
!       INPUT: Quantity computed on the mesh
!
!   INTEGER :: Perm(:)
!       INPUT: Element local numbering
!
!   REAL(KIND=dp) :: Fnorm
!       OUTPUT: volumic force norm
!
!   REAL(KIND=dp) :: Indicator(2)
!       OUTPUT: Remeshing criterion
!
!  INTEGER :: NDOFs
!   INPUT: Degrees of freedom of the solution Quant
!******************************************************************************
    USE CoordinateSystems
    USE ElementDescription
    !------------------------------------------------------------------------------
    IMPLICIT NONE
    !------------------------------------------------------------------------------
    TYPE(Model_t) :: Model
    INTEGER :: Perm(:), NDOFs
    REAL(KIND=dp) :: Quant(:), Indicator(2), Fnorm
    TYPE( Mesh_t ), POINTER    :: Mesh
    TYPE( Element_t ), POINTER :: Element
    !------------------------------------------------------------------------------
    ! Local variables
    !------------------------------------------------------------------------------
    TYPE(Nodes_t) :: Nodes
    TYPE(GaussIntegrationPoints_t), TARGET :: IntegStuff
    
    INTEGER :: i,j,n,t,DIM,k_dof

    LOGICAL :: stat

    REAL(KIND=dp), ALLOCATABLE :: x(:), y(:), z(:), Temperature(:,:), &
        Basis(:), dBasisdx(:,:), Grad(:,:)

    REAL(KIND=dp) :: u, v, w, s, detJ, GradNorm(NDOFs)
    !------------------------------------------------------------------------------
    ! Initialization:
    !------------------------------------------------------------------------------
    Indicator = 0.0d0
    Fnorm = 0.0
    GradNorm = 0.0d0
    
    DIM = CoordinateSystemDimension()
    !------------------------------------------------------------------------------
    ! Check if this eq. computed in this element:
    !------------------------------------------------------------------------------
    IF ( ANY( Perm( Element % NodeIndexes ) <= 0 ) ) RETURN
    !------------------------------------------------------------------------------
    ! Element nodal points:
    !------------------------------------------------------------------------------
    n = Element % TYPE % NumberOfNodes

    ALLOCATE( Nodes % x(n), Nodes % y(n), Nodes % z(n) )
    Nodes % x = Mesh % Nodes % x(Element % NodeIndexes)
    Nodes % y = Mesh % Nodes % y(Element % NodeIndexes)
    Nodes % z = Mesh % Nodes % z(Element % NodeIndexes)
    !------------------------------------------------------------------------------
    ALLOCATE( Temperature(NDOFs,n), Basis(n), dBasisdx(n,3), Grad(NDOFs,3) )
    !------------------------------------------------------------------------------
    ! Elementwise nodal solution:
    !------------------------------------------------------------------------------
    DO k_dof =1,NDOFs
        DO i=1,n
            j = Perm(Element % NodeIndexes(i))
            Temperature(k_dof,i) = Quant((j-1)*NDOFs + k_dof )  
        END DO
    END DO
    !------------------------------------------------------------------------------
    ! Integrate square of residual over element:
    !------------------------------------------------------------------------------
    IntegStuff = GaussPoints( Element )
    !------------------------------------------------------------------------------
    DO t=1,IntegStuff % n
    !------------------------------------------------------------------------------
        u = IntegStuff % u(t)
        v = IntegStuff % v(t)
        w = IntegStuff % w(t)
        
        Basis = 0.0d0
        dBasisdx = 0.0d0
        s=1
        
        stat = ElementInfo( Element, Nodes, u, v, w, detJ, Basis, dBasisdx )
        s = IntegStuff % s(t) * detJ

        Grad = 0.0d0
        
        DO k_dof =1,NDOFs   
            DO i = 1, DIM
                Grad(k_dof,i) = SUM( dBasisdx(1:n,i) * Temperature(k_dof,1:n) )
            END DO
            GradNorm(k_dof) = GradNorm(k_dof) + s * SUM( Grad(k_dof,1:DIM) * Grad(k_dof,1:DIM) ) 
        END DO !k_dof
    !------------------------------------------------------------------------------
    END DO
    !------------------------------------------------------------------------------
    Indicator = Element % hK**2 * MAXVAL(GradNorm)
    !------------------------------------------------------------------------------
    DEALLOCATE( Temperature, Basis, dBasisdx, Grad  )
!------------------------------------------------------------------------------
END FUNCTION TemperatureInsideResidual
!------------------------------------------------------------------------------




