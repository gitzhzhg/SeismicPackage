%==============================================================================
% ART = Anisotropic Ray Tracing -- stacking-velocity tomography
%==============================================================================
% 3-D ray-tracing in block-homogeneous anisotropic media with irregular
% interfaces & parameter stimation constrained by vertical velocities
%==============================================================================

clear all;   close all;
format short;

% Global definitions 
global Source Receiv Npair
global NpolX NpolY

%==============================================================================
% Open output file
  fout = fopen('Result.PSVtti2', 'w');

% Define constants
  constants;
% Dimensions of polynomials describing interfaces 
  NpolX = 2;   
  NpolY = 2;  

%==============================================================================
% Load NMO data and determine constants
  load datanmo.01; 
  trajectories = datanmo(:,1);   Ntrajectory = max(trajectories);
  Npair = length(trajectories)/Ntrajectory;

% Load vertical velocities
  load datavel.01;

%==============================================================================
NvelCon = 0;
% Loop over ray trajectories
for trajectory=1:Ntrajectory

% Loop over source-receiver pairs
  for ipair=1:Npair
    if trajectory == 1
       Source(1,ipair) = datanmo(ipair,2);
       Source(2,ipair) = datanmo(ipair,3);
       Source(3,ipair) = 0; 
       Receiv(:,ipair) = Source(:,ipair);
    end;

%   Input traveltimes, slownesses, and NMO ellipses
    DataEx(ipair,trajectory,1:6) = datanmo(ipair+Npair*(trajectory-1), 4:9);
  end;

% Input vertical velocities 
  NvelCon = NvelCon + 1;
  velConEx(NvelCon) = datavel(trajectory,2);
end;

DataEx
velConEx

fprintf('\n ');

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
% paramp0 = [1.50,  0.60,  0.15,  0.05,  20.0,  20.0];

% clear paramp0 paraMIN paraMAX
  paramp0 = [1.60,  0.70,  0.01,  0.01,    0.0];
  paraMIN = [1.00,  0.40, -0.20, -0.20, -180.0];
  paraMAX = [2.00,  1.00,  0.60,  0.60,  180.0];

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
% if ierr ~= 1
%   paramp0 = param;
% end;
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
          ['%6.3f %6.3f %6.3f %6.3f %6.2f %9.2e  %2g \n'], ...
          [param], sqrt(resnorm), exitflag);
end;

%==============================================================================
% Plot inverted model & rays
%==============================================================================
plotMod = 'Y';   plotIntermedInt = 'N';   plotRays = 'Y';
if plotMod == 'Y';
  figInterfaceInv = plotModel(plotIntermedInt);
end;

options(1) = 0;    options(3) = 9.e-01;   qNtol = 1.e-06;
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

