%==============================================================================
% ART = Anisotropic Ray Tracing -- modeling data for inversion
%==============================================================================
% 3-D ray-tracing in block-homogeneous anisotropic media with irregular
% interfaces & parameter stimation constrained by vertical velocities
%==============================================================================

clear all;   close all;
format short;

% Global definitions 
global Source Receiv Npair
global NReflector

%==============================================================================
% Open output file
  filenmo = fopen('datanmo.01', 'w');
  filevel = fopen('datavel.01', 'w');

%==============================================================================
% Define constants  
constants;

% Build anisotropic model, input acquisition geometry and ray codes

% Input model interfaces
inputInt;

% Input positions of sources and receivers
inputSouRec;

% Plot interfaces, sources, and receivers
plotMod = 'Y';   plotIntermedInt = 'N';   plotRays = 'Y';
if plotMod == 'Y';
  figInterfaceall = plotModel(plotIntermedInt);
end;

% Input Cij's 
inputCij;

% Input the number of ray trajectories
Ntrajectory = 2;
NReflector(1:Ntrajectory+4) = 0;  
NvelCon = 0;

%==============================================================================
% Compute traveltime for all source-receiver pairs using shooting method
options(1) = 0;    options(3) = 1.e-01;   qNtol = 1.e-06; 

% Loop over ray trajectories
for trajectory=1:Ntrajectory
% Input ray code
  clear global RayCode Nsegment NMOCode
  clear global p1 p2 q ugr vgr xray tau time
  global RayCode Nsegment NMOCode
  global p1 p2 q ugr vgr xray tau time
  inputRayCode(trajectory);
% Check whether the receivers locate at any model interfaces
  checkRec;

% Loop over source-receiver pairs
  for ipair=1:Npair
    [trajectory, ipair]
%   Initial guess for slowness
    if ipair == 1;
      slow0 = initGuess(trajectory, ipair);
    else
      slow0 = slowness2;
    end;

    fprintf(' ==> Simplex \n');
    slowness1 = fminsRT('shooting2', slow0, options, [], ipair);

    fprintf('\n ==> Quasi-Newton \n');
    slowness2 = qNewton(slowness1, ipair, qNtol);

%   Plot computed ray trajectories
    if plotRays == 'Y';   plotRay(figInterfaceall);   end;

    DataEx(ipair,trajectory,1) = time;
    DataEx(ipair,trajectory,2) = p1(1);
    DataEx(ipair,trajectory,3) = p2(1);
    if abs(Source(1,ipair) - Receiv(1,ipair)) < 1.e-4;
      U = effCyl(p1, p2, q, xray, tau, NMOCode);
      DataEx(ipair,trajectory,4) = U(1,1);
      DataEx(ipair,trajectory,5) = U(1,2);
      DataEx(ipair,trajectory,6) = U(2,2);
    end;
    fprintf(filenmo, ...
    ['%3.0f %7.4f %7.4f %7.4f %7.4f %7.4f %7.4f %7.4f %7.4f \n'], ...
    trajectory, Source(1,ipair), Source(2,ipair), DataEx(ipair,trajectory,:));
  end;

%------------------------------------------------------------------------------
% Equality constraints for the vertical velocities of P- and SV-waves 
  NvelCon = NvelCon + 1;
  velConEx(NvelCon) = inputVelCon;
  fprintf(filevel, ...
          ['%3.0f %7.4f \n'], trajectory, velConEx(NvelCon));
end;

DataEx
velConEx

%==============================================================================

