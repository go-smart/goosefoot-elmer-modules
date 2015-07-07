L = 1e-3;
M = 1e-3;
eps = 1e-6;
vdelta = 0.0;

R4 = $CONSTANT_MW_CATHETER_RADIUS_OUTER / 1000 + L / 5;
slotelevation = $CONSTANT_MW_SLOT_ELEVATION / 1000;
slotheight = $CONSTANT_MW_SLOT_HEIGHT / 1000;
dielectricextension = $CONSTANT_MW_DIELECTRIC_EXTENSION / 1000;

height = $INNERHEIGHT / 1000;
width = $INNERWIDTH / 1000;

bottomclearance = 1.6 * (slotheight + R4);

ncl = $NEARFIELD / 1000 / L;
fcl = $FARFIELD / 1000 / L;

// Liver Tissue
Point(1) = { (0.+eps), (- bottomclearance - R4 - dielectricextension - vdelta) / M, 0., ncl };
Point(2) = { (0.+eps), (- R4 - dielectricextension - vdelta) / M, 0., ncl };
Point(3) = { R4 / L, (- dielectricextension - vdelta)/ M, 0., ncl };
Point(4) = { R4 / L, (slotelevation - vdelta) / M, 0., ncl };
Point(5) = { R4 / L, (slotelevation + slotheight - vdelta) / M, 0., ncl };
Point(6) = { R4 / L, (height - vdelta) / M, 0., ncl };
Point(7) = { width / L, (height - vdelta) / M, 0., fcl };
Point(8) = { width / L, (- bottomclearance - R4 - dielectricextension - vdelta) / M, 0., fcl };

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
Plane Surface($BODIES_REGIONS_ORGANS) = {1};

Physical Surface($BODIES_REGIONS_ORGANS) = {$BODIES_REGIONS_ORGANS};
