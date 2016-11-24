%==============================================================================
% Assign values of Cij's during optimization 
%==============================================================================

function [] = inverCij(param);
 
%==============================================================================
% Each layer is characterized by its set of Cij's.
% For convenience of input, I subdivide all models into four categories:
%   ISO (isotropy), 
%   VTI (vertical transverse isotropy), 
%   ORT (orthorhombic with the symmetry planes coinciding with the
%        global coordinate planes), 
%   MNC (monoclinic with the horizontal symmetry plane), and
%   ARB (arbitrary).
% Each category has its set of input anisotropic parameters:
%   Vp and Vs for ISO,
%   Thomsen's for VTI,
%   Tsvankin's for ORT,
%   Grechka's for MNC, and
%   6x6 matrix of Cij's for ARB.
%==============================================================================

global Nlayer      
global Cij 
global TypeAnis

% Specify anisotropy of layers
% TypeAnis = ['VTI'; 'VTI'; 'VTI'];
% TypeAnis = ['VTI'; 'VTI'];
  TypeAnis = ['VTI'];
sizeTypeAnis = size(TypeAnis);   Nlayer = sizeTypeAnis(1);

%==============================================================================
% Input rotations of local coordinate axes (x', y', z') with respect to
% global coordinates (x, y, z):
% azimAngle -- azimuth of the local z'-axis with respect to the global x-axis
% tiltAngle -- tilt of the local z'-axis with respect to the global z-axis
% azx1Angle -- azimuth of the local x'-axis with respect to the global x-axis

% Default values
azimAngle = zeros(1,Nlayer);   tiltAngle = zeros(1,Nlayer);
azx1Angle = zeros(1,Nlayer);
Cij = zeros(6,6,Nlayer);

%==============================================================================
% Input anisotropic parameters
for layer=1:Nlayer

  if TypeAnis(layer,:) == 'ISO'
%   Input Vp and Vs
    Vp0 = 2.0;   Vs0 = 1.0;
    CC = thomsen2Cij(Vp0, Vs0, 0, 0, 0);
  end;

  if TypeAnis(layer,:) == 'VTI'
%   Input Thomsen's parameters
    switch layer
      case 1
        Vp0 = param(1);       Vs0 = param(2);
        epsilon = param(3);   delta = param(4);   gamma = -0.3;    
%       Vp0 = 1.499;          Vs0 = 0.603;   
%       epsilon = 0.155;      delta = 0.057;      gamma = -0.3;    
        tiltAngle(layer) = 90*pi/180;  
        azimAngle(layer) = param(5)*pi/180; 
    end;
    CC = thomsen2Cij(Vp0, Vs0, epsilon, delta, gamma);
  end;

  if TypeAnis(layer,:) == 'ORT'
%   Input Tsvankin's parameters
    Vp0 = 1.0;   Vs0 = 0.5;
    epsilon2 = 0.15;   delta2 = 0.05;   gamma2 = -0.3;
    epsilon1 = 0.25;   delta1 = 0.15;   gamma1 = -0.2;   delta3 = 0.0;
    CC = tsvankin2Cij(Vp0, Vs0, epsilon1, epsilon2, delta1, delta2, delta3, ...
                      gamma1, gamma2);
    azimAngle(layer) = pi*60/180;
    tiltAngle(layer) = pi*40/180;
    azx1Angle(layer) = pi*20/180;
  end;

  if TypeAnis(layer,:) == 'MNC'
%   Input Grechka's parameters
    Vp0 = 2.0;   Vs0 = 1.0;
    epsilon2 = 0.15;   delta2 = 0.05;   gamma2 = 0.1;
    epsilon1 = 0.15;   delta1 = 0.05;   gamma1 = 0.1;   delta3 = 0.0;
    zeta1 = 0.01;   zeta2 = 0.02;   zeta3 = 0.03;
    CC = tsvankin2Cij(Vp0, Vs0, epsilon1, epsilon2, delta1, delta2, delta3, ...
                      gamma1, gamma2, zeta1, zeta2, zeta3);
%   azimAngle(layer) = pi*40/180;
%   tiltAngle(layer) = pi/6;
%   azx1Angle(layer) = pi/9;
  end;

% Check the velocities
  if Vp0 < Vs0;
     fprintf('Vp0 < Vs0 in layer %g \n', layer);
     error('Correct the input in inputCij');
  end;

% Rotate the stiffness matrix
  R = local2global(azimAngle(layer), tiltAngle(layer), azx1Angle(layer));
  Cij(:,:,layer) = bondTransf(CC, R);

end;

%==============================================================================

