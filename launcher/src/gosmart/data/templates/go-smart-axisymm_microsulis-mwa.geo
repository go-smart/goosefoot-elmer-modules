L = 1e-3;
M = 1e-3;
eps = 1e-6;
vdelta = 0.0;

// R1 = CONSTANT_MW_DIELECTRIC_RADIUS_INNER / 1000;
// R2 = CONSTANT_MW_DIELECTRIC_RADIUS_OUTER / 1000;
// R3 = CONSTANT_MW_CATHETER_RADIUS_INNER / 1000;
// R4 = CONSTANT_MW_CATHETER_RADIUS_OUTER / 1000;
// slotelevation = CONSTANT_MW_SLOT_ELEVATION / 1000;
// slotheight = CONSTANT_MW_SLOT_HEIGHT / 1000;
// dielectricextension = CONSTANT_MW_DIELECTRIC_EXTENSION  / 1000;
// 
height = $INNERHEIGHT / 1000;
width = $INNERWIDTH / 1000;
R1 = 0.135 / 1000;
R2 = 0.470 / 1000;
R3 = 0.595 / 1000;
R4 = 0.895 / 1000;
slotelevation = 6 / 1000;
slotheight = 1 / 1000;
dielectricextension = 0.135 / 1000;

bottomclearance = 10 / 1000;

// ncl = $NEARFIELD / 1000 / L;
// fcl = $FARFIELD / 1000 / L;
// dcl = $NEARFIELD / 1000 / L;
// ccl = $NEARFIELD / 1000 / L;
// scl = $NEARFIELD / 1000 / L;
NEARFIELD = 0.05;
FARFIELD = 3.0;
ncl = NEARFIELD / 1000 / L;
fcl = FARFIELD / 1000 / L;
dcl = 2 * NEARFIELD / 1000 / L;
ccl = 2 * NEARFIELD / 1000 / L;
scl = 2 * NEARFIELD / 1000 / L;

REF = 30;

dy_tip = - 7 / 1000;
dy_tip_2 = dy_tip + 3.25 / 1000;
l_ferrule_3 = (REF - 23.8) / 1000;
l_ferrule_1 = (REF - 19.8) / 1000;
l_dielectric_coax = (REF - 18.3) / 1000;
l_shaft = (REF - 16.8) / 1000;
l_air_shaft = (REF - 15.3) / 1000;
r_outer_cond = 0.6 / 1000;
r_cyl_inner = 0.2 / 1000;
r_cyl_outer = 0.61 / 1000;
r_centre_cond = 0.16 / 1000;
r_washer_inner = 0.2 / 1000;
r_washer_outer = 0.6 / 1000;
r_dielectric_coax = 0.47 / 1000;
r_shaft_inner = 0.805 / 1000;
r_shaft = 0.915 / 1000;
r_tip = r_shaft;
r_tip_inner = 0.685 / 1000;
r_ferrule_3_outer = 0.61 / 1000;
r_ferrule_3_inner = r_dielectric_coax;
r_ferrule_2_outer = r_shaft;
r_ferrule_1_outer = r_shaft_inner;
dy_washer = 0.5 / 1000;
dy_solder = dy_washer;

// Liver Tissue
Point(1) = { (0.+eps), (- bottomclearance + dy_tip - vdelta) / M, 0., ncl };
Point(2) = { (0.+eps), (dy_tip - vdelta) / M, 0., ncl };
Point(3) = { r_tip / L, (dy_tip_2 - vdelta)/ M, 0., ncl };
//Point(4) = { r_tip / L, (l_ferrule_1 - vdelta) / M, 0., ncl };
//Point(5) = { r_tip / L, (l_shaft - vdelta) / M, 0., ncl };
Point(6) = { r_tip / L, (height - vdelta) / M, 0., ncl };
Point(7) = { width / L, (height - vdelta) / M, 0., fcl };
Point(8) = { width / L, (- bottomclearance + dy_tip - vdelta) / M, 0., fcl };

// Ferrule
Point(11) = { r_ferrule_3_inner / L, (l_ferrule_3 - vdelta) / M, 0., scl };
Point(12) = { r_ferrule_3_outer / L, (l_ferrule_3 - vdelta) / M, 0., scl };
Point(13) = { r_ferrule_3_outer / L, (l_ferrule_1 - vdelta) / M, 0., scl };
Point(14) = { r_ferrule_2_outer / L, (l_ferrule_1 - vdelta) / M, 0., scl };
Point(15) = { r_ferrule_2_outer / L, (l_shaft - vdelta) / M, 0., scl };
//Point(16) = { r_ferrule_1_outer / L, (l_shaft- vdelta) / M, 0., scl };
Point(17) = { r_ferrule_1_outer / L, (l_air_shaft - vdelta) / M, 0., scl };
Point(18) = { r_outer_cond / L, (l_air_shaft - vdelta) / M, 0., scl };
//Point(19) = { r_outer_cond / L, (l_dielectric_coax - vdelta) / M, 0., scl };
Point(110) = { r_ferrule_3_inner / L, (l_dielectric_coax - vdelta) / M, 0., scl };

// Dielectric cylinder
Point(21) = { r_cyl_inner / L, (dy_washer - vdelta) / M, 0., dcl };
Point(22) = { r_cyl_outer / L, (dy_washer - vdelta) / M, 0., dcl };
Point(23) = { r_cyl_outer / L, (l_ferrule_3 - vdelta) / M, 0., dcl };
Point(24) = { r_cyl_inner / L, (l_ferrule_3 - vdelta) / M, 0., dcl };

// Tip
Point(31) = { (0.0+eps) / L, (dy_tip - vdelta) / M, 0., ccl };
Point(32) = { r_tip / L, (dy_tip_2 - vdelta) / M, 0., ccl };
Point(33) = { r_tip / L, (l_ferrule_1 - vdelta) / M, 0., ccl };
Point(34) = { r_tip_inner / L, (l_ferrule_1 - vdelta) / M, 0., ccl };
Point(35) = { r_tip_inner / L, (0.0 - vdelta) / M, 0., ccl };
Point(36) = { (0.0+eps) / L, (0.0 - vdelta) / M, 0., ccl };

// Central conductor
Point(41) = { (0.0+eps) / L, (0.0 - vdelta) / M, 0., ccl };
Point(42) = { r_centre_cond / L, (0.0 - vdelta) / M, 0., ccl };
Point(43) = { r_centre_cond / L, (height - vdelta) / M, 0., ccl };
Point(44) = { (0.0+eps) / L, (height - vdelta) / M, 0., ccl };

// Washer
Point(51) = { r_washer_inner / L, (0.0 - vdelta) / M, 0., ccl };
Point(52) = { r_washer_outer / L, (0.0 - vdelta) / M, 0., ccl };
Point(53) = { r_washer_outer / L, (dy_washer - vdelta) / M, 0., ccl };
Point(54) = { r_washer_inner / L, (dy_washer - vdelta) / M, 0., ccl };

// Coax dielectric
Point(61) = { r_centre_cond / L, (l_dielectric_coax - vdelta) / M, 0., ccl };
Point(62) = { r_dielectric_coax / L, (l_dielectric_coax - vdelta) / M, 0., ccl };
Point(63) = { r_dielectric_coax / L, (height - vdelta) / M, 0., ccl };
Point(64) = { r_centre_cond / L, (height - vdelta) / M, 0., ccl };

// Outer conductor
Point(71) = { r_dielectric_coax / L, (l_dielectric_coax - vdelta) / M, 0., ccl };
Point(72) = { r_outer_cond / L, (l_dielectric_coax - vdelta) / M, 0., ccl };
Point(73) = { r_outer_cond / L, (height - vdelta) / M, 0., ccl };
Point(74) = { r_dielectric_coax / L, (height - vdelta) / M, 0., ccl };

// Air gap
Point(81) = { r_outer_cond / L, (l_air_shaft - vdelta) / M, 0., ccl };
Point(82) = { r_shaft_inner / L, (l_air_shaft - vdelta) / M, 0., ccl };
Point(83) = { r_shaft_inner / L, (height - vdelta) / M, 0., ccl };
Point(84) = { r_outer_cond / L, (height - vdelta) / M, 0., ccl };

// Shaft
Point(91) = { r_shaft_inner / L, (l_shaft - vdelta) / M, 0., ccl };
Point(92) = { r_shaft / L, (l_shaft - vdelta) / M, 0., ccl };
Point(93) = { r_shaft / L, (height - vdelta) / M, 0., ccl };
Point(94) = { r_shaft_inner / L, (height - vdelta) / M, 0., ccl };

// Air gap #2
Point(1001) = { r_centre_cond / L, (l_ferrule_3 - vdelta) / M, 0., ccl };
Point(1002) = { r_ferrule_3_inner / L, (l_ferrule_3 - vdelta) / M, 0., ccl };
Point(1003) = { r_ferrule_3_inner / L, (l_dielectric_coax - vdelta) / M, 0., ccl };
Point(1004) = { r_centre_cond / L, (l_dielectric_coax - vdelta) / M, 0., ccl };
//Point(1003) = Point(62) = { r_ferrule_3_inner / L, (l_dielectric_coax - vdelta) / M, 0., ccl };
//Point(1004) = Point(61) = { r_centre_cond / L, (l_dielectric_coax - vdelta) / M, 0., ccl };

// Adhesive #1
Point(1011) = { r_centre_cond / L, (dy_solder - vdelta) / M, 0., ccl };
Point(1012) = { r_cyl_inner / L, (dy_solder - vdelta) / M, 0., ccl };
Point(1013) = { r_cyl_inner / L, (l_ferrule_3 - vdelta) / M, 0., ccl };
Point(1014) = { r_centre_cond / L, (l_ferrule_3 - vdelta) / M, 0., ccl };

// Solder
Point(1021) = { r_centre_cond / L, (0.0 - vdelta) / M, 0., ccl };
Point(1022) = { r_washer_inner / L, (0.0 - vdelta) / M, 0., ccl };
Point(1023) = { r_washer_inner / L, (dy_solder - vdelta) / M, 0., ccl };
Point(1024) = { r_centre_cond / L, (dy_solder - vdelta) / M, 0., ccl };

// Adhesive #2
Point(1031) = { r_washer_outer / L, (0.0 - vdelta) / M, 0., ccl };
Point(1032) = { r_tip_inner / L, (0.0 - vdelta) / M, 0., ccl };
Point(1033) = { r_tip_inner / L, (l_ferrule_1 - vdelta) / M, 0., ccl };
Point(1034) = { r_cyl_outer / L, (l_ferrule_1 - vdelta) / M, 0., ccl };
Point(1035) = { r_cyl_outer / L, (dy_washer - vdelta) / M, 0., ccl };
Point(1036) = { r_washer_outer / L, (dy_washer - vdelta) / M, 0., ccl };

// Liver Tissue
Line(1) = { 1, 2 };
Line(2) = { 2, 3 };
Line(3) = { 3, 33 };
Line(4) = { 33, 15 };
Line(5) = { 15, 6 };
Line(6) = { 6, 7 };
Line(7) = { 7, 8 };
Line(8) = { 8, 1 };

// Ferrule
//Line(11) = { 11, 12 };
//Line(12) = { 12, 13 };
//Line(13000) = { 13, 34 };
//Line(13001) = { 34, 14 };
//Line(14) = { 14, 15 };
//Line(15) = { 15, 16 };
//Line(16) = { 16, 17 };
//Line(17) = { 17, 18 };
//Line(18) = { 18, 19 };
//Line(19) = { 19, 110 };
//Line(110) = { 110, 11 };

// Dielectric cylinder
Line(21000) = { 21, 1036 };
Line(21001) = { 1036, 22 };
Line(22) = { 22, 23 };
Line(23000) = { 23, 1002 };
Line(23001) = { 1002, 24 };
Line(24) = { 24, 21 };

// Tip
Line(31) = { 31, 32 };
Line(32) = { 32, 33 };
Line(33) = { 33, 34 };
Line(34) = { 34, 35 };
Line(35000) = { 35, 52 };
Line(35001) = { 52, 51 };
Line(35002) = { 51, 1021 };
Line(35003) = { 1021, 36 };
Line(36) = { 36, 31 };

// Central conductor
//Line(41) = { 41, 42 };
//Line(42000) = { 42, 1011 };
//Line(42001) = { 1011, 1001 };
//Line(42002) = { 1001, 61 };
//Line(42003) = { 61, 43 };
//Line(43) = { 43, 44 };
//Line(44) = { 44, 41 };

// Washer
//Line(51) = { 51, 52 };
//Line(52) = { 52, 53 };
//Line(53) = { 53, 54 };
//Line(54) = { 54, 51 };

// Coax dielectric
Line(61) = { 61, 62 };
Line(62) = { 62, 63 };
Line(63) = { 63, 64 };
Line(64) = { 64, 61 };

// Outer conductor
//Line(71) = { 71, 72 };
//Line(72000) = { 72, 18 };
//Line(72001) = { 18, 73 };
//Line(73) = { 73, 74 };
//Line(74) = { 74, 71 };

// Air gap
Line(81) = { 81, 82 };
Line(82) = { 82, 83 };
Line(83) = { 83, 84 };
Line(84) = { 84, 81 };

// Shaft
//Line(91) = { 91, 92 };
//Line(92) = { 92, 93 };
//Line(93) = { 93, 94 };
//Line(94000) = { 94, 17 };
//Line(94001) = { 17, 91 };

// Air gap #2
Line(10010) = { 1001, 1013 };
Line(10011) = { 1013, 1002 };
//Line(1002) = { 1002, 62 };
Line(1002) = { 1002, 1003 };
//Line(1003) = -Line(61) = { 1003, 1004 };
Line(1003) = { 1003, 1004 };
//Line(1004) = { 61, 1001 };
Line(1004) = { 1004, 1001 };

// Adhesive #1
Line(1011) = { 1011, 1012 };
Line(1012) = { 1012, 1013 };
Line(1013) = { 1013, 1014 };
Line(1014) = { 1014, 1011 };

// Solder
//Line(1021) = { 1021, 1022 };
//Line(1022) = { 1022, 1023 };
//Line(1023) = { 1023, 1024 };
//Line(1024) = { 1024, 1021 };

// Adhesive #2
Line(1031) = { 1031, 1032 };
Line(1032) = { 1032, 1033 };
Line(1033) = { 1033, 1034 };
Line(10340) = { 1034, 23 };
Line(10341) = { 23, 1035 };
Line(1035) = { 1035, 1036 };
Line(1036) = { 1036, 1031 };

// Liver Tissue
Line Loop(1) = { 1, 2, 3, 4, 5, 6, 7, 8 };

// Ferrule
//Line Loop(11) = { 11, 12, 13000, 13001, 14, 15, 16, 17, 18, 19, 110 };

// Dielectric
Line Loop(21) = { 21000, 21001, 22, 23000, 23001, 24 };

// Tip
Line Loop(31) = { 31, 32, 33, 34, 35000, 35001, 35002, 35003, 36 };

// Central conductor
//Line Loop(41) = { 41, 42000, 42001, 42002, 42003, 43, 44 };

// Washer
//Line Loop(51) = { 51, 52, 53, 54 };

// Coax dielectric
Line Loop(61) = { 61, 62, 63, 64 };

// Outer conductor
//Line Loop(71) = { 71, 72000, 72001, 73, 74 };

// Air gap
Line Loop(81) = { 81, 82, 83, 84 };

// Shaft
//Line Loop(91) = { 91, 92, 93, 94000, 94001 };

// Air gap #2
Line Loop(1001) = { 10010, 10011, 1002, 1003, 1004 };

// Adhesive #1
Line Loop(1011) = { 1011, 1012, 1013, 1014 };

// Solder
//Line Loop(1021) = { 1021, 1022, 1023, 1024 };

// Adhesive #2
Line Loop(1031) = { 1031, 1032, 1033, 10340, 10341, 1035, 1036 };

/* Subdomains */

// Liver Tissue
Plane Surface(1) = {1};

// Ferrule
//Plane Surface(2) = {11};

// Dielectric
Plane Surface(3) = {21};

// Tip
Plane Surface(4) = {31};

// Central conductor
//Plane Surface(5) = {41};

// Washer
//Plane Surface(6) = {51};

// Coax dielectric
Plane Surface(7) = {61};

// Outer conductor
//Plane Surface(8) = {71};

// Air gap
Plane Surface(9) = {81};

// Shaft
//Plane Surface(10) = {91};

// Air gap #2
Plane Surface(11) = {1001};

// Adhesive #1
Plane Surface(12) = {1011};

// Solder
//Plane Surface(13) = {1021};

// Adhesive #2
Plane Surface(14) = {1031};

Coherence;

// Tissue
//  Physical Surface(1) = {1};

// Ferrule
//Physical Surface(2) = {2};

// Dielectric cylinder
//  Physical Surface(3) = {3};

// Tip
//  Physical Surface(4) = {4};

// Central conductor
//Physical Surface(5) = {5};

// Washer
//Physical Surface(6) = {6};

// Coax dielectric
//  Physical Surface(7) = {7};

// Outer conductor
//Physical Surface(8) = {8};

// Air gap
//  Physical Surface(9) = {9};

// Shaft
//Physical Surface(10) = {10};

// Air gap #2
//  Physical Surface(11) = {11};

// Adhesive #1
//  Physical Surface(12) = {12};

// Solder
//Physical Surface(13) = {13};

// Adhesive #2
//  Physical Surface(14) = {14};

Physical Surface($REGION_ORGAN_0) = {1};
Physical Surface($REGION_SLOT) = {9, 11};
Physical Surface($REGION_DIELECTRIC_CABLE) = {3, 7};
Physical Surface($REGION_CATHETER) = {4, 12, 14};

/* Boundaries */

// Scattering Boundary Condition
Physical Line(2) = { 83, 93, 6, 7, 8 };

// Conductor Boundary Condition (Upper + Lower)
Physical Line(3) = { 41, 42000, 42001, 42002, 42003, 43, 44,
                     71, 72000, 72001, 73, 74,
                     51, 52, 53, 54,
                     1021, 1022, 1023, 1024,
                     11, 12, 13000, 13001, 14, 15, 16, 17, 18, 19, 110 };

// Axisymmetric Boundary Condition
Physical Line(1) = { 1, -36, -44 };

// Port Boundary Condition
Physical Line(4) = { 63 };

// Temperature Boundary Condition
Physical Line(5) = { 6, 7, 8 };

Coherence;
