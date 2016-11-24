%==============================================================================
% Compute the 2x2 matrix of the second-order derivatives d^2 x3/(d xi d xj)
% of the interface at [x1, x2]
%==============================================================================

function curv = curvInt(Iinterface, x1, x2)

%==============================================================================

global Ninterface NpolX NpolY IntPol
global IntPolX IntPolY
global IntPolXX IntPolXY IntPolYY

% Compute the second-order partial derivatives of the interface
if NpolX > 2
  cxx = funInt(NpolX-2, NpolY,   IntPolXX, Iinterface, x1, x2);
else
  cxx = 0;
end

cxy = funInt(NpolX-1, NpolY-1, IntPolXY, Iinterface, x1, x2);

if NpolY > 2
  cyy = funInt(NpolX,   NpolY-2, IntPolYY, Iinterface, x1, x2);
else
  cyy = 0;
end

curv = [cxx, cxy; cxy, cyy];

%==============================================================================

