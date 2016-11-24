%==============================================================================
% ART = Anisotropic Ray Tracing
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
  fout = fopen('Result.PSVinv01', 'w');

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
Ntrajectory = 3;
NReflector(1:Ntrajectory+4) = 0;  
NvelCon = 0;

%==============================================================================
% Compute traveltime for all source-receiver pairs using shooting method
options(1) = 0;    options(3) = 9.e-01;   qNtol = 1.e-06; 

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
  end;

%------------------------------------------------------------------------------
% Equality constraints for the vertical velocities of P- and SV-waves 
  NvelCon = NvelCon + 1;
  velConEx(NvelCon) = inputVelCon;
end;

DataEx
velConEx

fprintf('\n \n');

%==============================================================================
% Inversion
%==============================================================================
tolParam = eps;
OPTIONS = optimset('LargeScale', 'on', 'Display','off', ...
                   'Jacobian', 'off', ...
                   'MaxIter', 10000, 'MaxFunEvals', 50000, ...
                   'TolX', tolParam,   'TolFun', sqrt(tolParam));
 
randn('state',3);
 errt = 0.010;   errp = 0.010;   erru = 0.020;   errv = 0.005;
%errt = 0.000;   errp = 0.000;   erru = 0.000;   errv = 0.000;

% Array of layer parameters: 
  paramp0 = [1.50, 0.60,  0.11,  0.11, -0.11,  40.0,   40.0];
  paraMIN = [1.20, 0.40, -0.10, -0.10, -0.40,   0.0, -180.0];
  paraMAX = [2.00, 1.00,  0.40,  0.40,  0.40,  90.0,  180.0];

% Loop over errors in the data
for ierr=1:301
  for ivelCon=1:NvelCon 
    velCon(ivelCon) = velConEx(ivelCon)*(1 + errv*randn(1));
  end;
  for trajectory=1:Ntrajectory
  for ipair=1:Npair
    Data(ipair,trajectory,1) = DataEx(ipair,trajectory,1)*(1 + errt*randn(1));
    Data(ipair,trajectory,2) = DataEx(ipair,trajectory,2)*(1 + errp*randn(1));
    Data(ipair,trajectory,3) = DataEx(ipair,trajectory,3)*(1 + errp*randn(1));
    Data(ipair,trajectory,4) = DataEx(ipair,trajectory,4)*(1 + erru*randn(1));
    Data(ipair,trajectory,5) = DataEx(ipair,trajectory,5)*(1 + erru*randn(1));
    Data(ipair,trajectory,6) = DataEx(ipair,trajectory,6)*(1 + erru*randn(1));
  end;   
end;

% Initial guess
  if ierr ~= 1
    paramp0 = param;
  end;
  fprintf(' \n');
  fprintf('Error %4g \n', ierr);

% Gauss-Newton with equality constraints
  [param, resnorm, residual, exitflag, output] = ...
    lsqnonlin('artConGNinv', paramp0, paraMIN, paraMAX, OPTIONS, ...
              Data, Ntrajectory, velCon);
% param
% sqrt(resnorm)
% exitflag
% output

  fprintf(fout, ...
          ['%6.4f  %6.4f  %6.4f  %6.4f  %6.4f  %6.2f  %6.2f  %12.4e  %2g \n'], ...
          [param], sqrt(resnorm), exitflag);
end;

stop

%==============================================================================
% Plot inverted model & rays
%==============================================================================
if plotMod == 'Y';
  figInterfaceInv = plotModel(plotIntermedInt);
end;

% Loop over ray trajectories
for trajectory=1:Ntrajectory
% Input ray code
  clear global RayCode Nsegment NMOCode
  clear global p1 p2 q ugr vgr xray tau time
  global RayCode Nsegment NMOCode
  global p1 p2 q ugr vgr xray tau time
  inverRayCode(trajectory);

% Loop over source-receiver pairs
  for ipair=1:Npair
    [trajectory, ipair]
%   Initial guess for slowness
    slow0 = [DataEx(ipair,trajectory,2), DataEx(ipair,trajectory,3)];

    fprintf(' ==> Simplex \n');
    slowness1 = fminsRT('shooting2', slow0, options, [], ipair);

    fprintf('\n ==> Quasi-Newton \n');
    slowness2 = qNewton(slowness1, ipair, qNtol);

%   Plot computed ray trajectories
    if plotRays == 'Y';   plotRay(figInterfaceInv);   end;

  end;
end;
%==============================================================================

