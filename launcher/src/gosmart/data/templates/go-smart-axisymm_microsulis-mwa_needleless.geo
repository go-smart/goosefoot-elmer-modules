L = 1e-3;
M = 1e-3;
eps = 1e-6;
vdelta = 0.0;

slotelevation = 6 / 1000;
slotheight = 1 / 1000;
dielectricextension = 0.135 / 1000;

// height = $INNERHEIGHT / 1000;
// width = $INNERWIDTH / 1000;
height = $INNERHEIGHT / 1000;
width = $INNERWIDTH / 1000;

bottomclearance = 10 / 1000;

ncl = $NEARFIELD / 1000 / L;
fcl = $FARFIELD / 1000 / L;
ccl = 2 * $NEARFIELD / 1000 / L;
scl = 2 * $NEARFIELD / 1000 / L;

REF = 30;

dy_tip = - 7 / 1000;
dy_tip_2 = dy_tip + 3.25 / 1000;
l_ferrule_1 = (REF - 19.8) / 1000;
l_shaft = (REF - 16.8) / 1000;
r_shaft = 0.915 / 1000;
r_tip = r_shaft;
r_ferrule_2_outer = r_shaft;

// Liver Tissue
Point(1) = { (0.+eps), (- bottomclearance + dy_tip - vdelta) / M, 0., ncl };
Point(2) = { (0.+eps), (dy_tip - vdelta) / M, 0., ncl };
Point(3) = { r_tip / L, (dy_tip_2 - vdelta)/ M, 0., ncl };
Point(4) = { r_tip / L, (l_ferrule_1 - vdelta) / M, 0., ccl };
Point(5) = { r_ferrule_2_outer / L, (l_shaft - vdelta) / M, 0., scl };
Point(6) = { r_tip / L, (height - vdelta) / M, 0., ncl };
Point(7) = { width / L, (height - vdelta) / M, 0., fcl };
Point(8) = { width / L, (- bottomclearance + dy_tip - vdelta) / M, 0., fcl };

// Liver Tissue
Line(1) = { 1, 2 };
Line(2) = { 2, 3 };
Line(3) = { 3, 4 };
Line(4) = { 4, 5 };
Line(5) = { 5, 6 };
Line(6) = { 6, 7 };
Line(7) = { 7, 8 };
Line(8) = { 8, 1 };

// Liver Tissue
Line Loop(1) = { 1, 2, 3, 4, 5, 6, 7, 8 };

/* Subdomains */

// Liver Tissue
Plane Surface($REGION_ORGAN_0) = {1};

Physical Surface($REGION_ORGAN_0) = {$REGION_ORGAN_0};
