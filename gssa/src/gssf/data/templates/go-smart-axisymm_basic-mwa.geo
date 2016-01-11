L = 1e-3;
M = 1e-3;
eps = 1e-6;
vdelta = 0.0;

R1 = $CONSTANT_MW_DIELECTRIC_RADIUS_INNER / 1000;
R2 = $CONSTANT_MW_DIELECTRIC_RADIUS_OUTER / 1000;
R3 = $CONSTANT_MW_CATHETER_RADIUS_INNER / 1000;
R4 = $CONSTANT_MW_CATHETER_RADIUS_OUTER / 1000;
slotelevation = $CONSTANT_MW_SLOT_ELEVATION / 1000;
slotheight = $CONSTANT_MW_SLOT_HEIGHT / 1000;
dielectricextension = $CONSTANT_MW_DIELECTRIC_EXTENSION  / 1000;

height = $INNERHEIGHT / 1000;
width = $INNERWIDTH / 1000;

bottomclearance = 1.6 * (slotheight + R4);

ncl = $NEARFIELD / 1000 / L;
fcl = $FARFIELD / 1000 / L;
dcl = $NEARFIELD / 1000 / L;
ccl = $NEARFIELD / 1000 / L;
scl = $NEARFIELD / 1000 / L;

// Liver Tissue
Point(1) = { (0.+eps), (- bottomclearance - R4 - dielectricextension - vdelta) / M, 0., ncl };
Point(2) = { (0.+eps), (- R4 - dielectricextension - vdelta) / M, 0., ncl };
Point(3) = { R4 / L, (- dielectricextension - vdelta)/ M, 0., ncl };
Point(4) = { R4 / L, (slotelevation - vdelta) / M, 0., ncl };
Point(5) = { R4 / L, (slotelevation + slotheight - vdelta) / M, 0., ncl };
Point(6) = { R4 / L, (height - vdelta) / M, 0., ncl };
Point(7) = { width / L, (height - vdelta) / M, 0., fcl };
Point(8) = { width / L, (- bottomclearance - R4 - dielectricextension - vdelta) / M, 0., fcl };

// Slot
Point(11) = { R2 / L, (slotelevation - vdelta) / M, 0., scl };
Point(12) = { R2 / L, (slotelevation + slotheight - vdelta) / M, 0., scl };
Point(13) = { R3 / L, (slotelevation + slotheight - vdelta) / M, 0., scl };
Point(14) = { R3 / L, (slotelevation - vdelta) / M, 0., scl };

// Dielectric
Point(21) = { R2 / L, (height - vdelta) / M, 0., dcl };
Point(22) = { R2 / L, (- vdelta) / M, 0., dcl };
Point(23) = { R1 / L, (- vdelta) / M, 0., dcl };
Point(24) = { R1 / L, (height - vdelta) / M, 0., dcl };

// Catheter
Point(31) = { R3 / L, (height - vdelta) / M, 0., ccl };
Point(32) = { R3 / L, (-dielectricextension - vdelta) / M, 0., ccl };
Point(33) = { (0.0+eps) / L, (-dielectricextension - vdelta) / M, 0., ccl };

// Liver Tissue
Line(1) = { 1, 2 };
Line(2) = { 2, 3 };
Line(3) = { 3, 4 };
Line(4) = { 4, 5 };
Line(5) = { 5, 6 };
Line(6) = { 6, 7 };
Line(7) = { 7, 8 };
Line(8) = { 8, 1 };

// Slot
Line(11) = { 11, 12 };
Line(12) = { 12, 13 };
Line(13) = { 13, 14 };
Line(14) = { 14, 11 };

// Dielectric
Line(21) = { 21, 12 };
Line(22) = { 11, 22 };
Line(23) = { 22, 23 };
Line(24) = { 23, 24 };
Line(25) = { 24, 21 };

// Catheter
Line(31) = { 31, 13 };
Line(32) = { 14, 32 };
Line(33) = { 32, 33 };
Line(34) = { 33, 2 };
Line(35) = { 6, 31 };

// Liver Tissue
Line Loop(1) = { 1, 2, 3, 4, 5, 6, 7, 8 };

// Slot
Line Loop(11) = { 11, 12, 13, 14 };

// Dielectric
Line Loop(21) = { 21, -11, 22, 23, 24, 25 };

// Catheter
Line Loop(31) = { 31, 13, 32, 33, 34, 2, 3, 4, 5, 35 };

/* Subdomains */

// Liver Tissue
Plane Surface(1) = {1};

// Slot
Plane Surface(2) = {11};

// Dielectric
Plane Surface(3) = {21};

// Catheter
Plane Surface(4) = {31};

Physical Surface($REGION_ORGAN_0) = {1};
Physical Surface($REGION_SLOT) = {2};
Physical Surface($REGION_DIELECTRIC_CABLE) = {3};
Physical Surface($REGION_CATHETER) = {4};

/* Boundaries */

// Scattering Boundary Condition
Physical Line(11) = { -35, 6, 7, 8 };

// Conductor Boundary Condition (Upper + Lower)
Physical Line(12) = { 31, -12, -21,
                     -33, -32, 14, 22, 23, 24 };

// Axisymmetric Boundary Condition
Physical Line(13) = { 1, -34 };

// Port Boundary Condition
Physical Line(14) = { 25 };
