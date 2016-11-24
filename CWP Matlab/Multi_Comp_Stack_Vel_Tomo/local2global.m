%==============================================================================
% Compute the matrix that azx1tes Cij from local coordinate frame (x', y', z') 
% to the global (x, y, z). The azx1tion is specified by three angles:
% azim -- azimuth of the local z'-axis with respect to the global x-axis
% tilt -- tilt of the local z'-axis with respect to the global z-axis
% azx1 -- azimuth of the local x'-axis with respect to the global x-axis
%==============================================================================

function [R] = local2global(azim, tilt, azx1); 

sa = sin(azim);   ca = cos(azim);   sb = sin(tilt);   cb = cos(tilt);
sd = sin(azx1);   cd = cos(azx1);   cda = ca*cd + sa*sd;
if abs(sb) < 1.e-6  |  abs(cda) < 1.e-6
  ct = 0;   st = 1;
else
  tt = -cb/(sb*cda);   ct = -1/sqrt(1+tt^2);   st = tt*ct;
end;
x1 = [cd*st, sd*st, ct];   x3 = [ca*sb, sa*sb, cb];   x2 = cross(x3, x1);
R = [x1; x2; x3];

%==============================================================================

