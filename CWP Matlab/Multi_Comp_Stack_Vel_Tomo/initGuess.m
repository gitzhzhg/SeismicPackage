%==============================================================================
% Initial guess for tracing zero-offset rays
%==============================================================================

function slowness = initGuess(trajectory, ipair);

%==============================================================================
% The horizontal components of initial slowness vector are taken to be 
% parallel to the reflector normal undet CMP location
%==============================================================================

global Cij
global Source Receiv Npair
global RayCode Nsegment NMOCode
global NReflector

%==============================================================================

Xcmp = (Source(1,ipair) + Receiv(1,ipair))/2;
Ycmp = (Source(2,ipair) + Receiv(2,ipair))/2;
normal = normInt(NReflector(trajectory+2), Xcmp, Ycmp);
if RayCode(1,1) == 1
  slowness(1:2) = normal(1:2)/sqrt(Cij(3,3,1));
else
  slowness(1:2) = normal(1:2)/sqrt(Cij(4,4,1));
end;

%==============================================================================

