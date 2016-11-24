%==============================================================================
% Input of ray codes 
%==============================================================================

function [] = inputRayCode(trajectory);

%==============================================================================
% Each code is the [3,NRaySegm]-matrix, where the lines are:
%   [1,:] -- types of rays: 
%          = 1 for P, 
%          = 2 for fast S (S1), 
%          = 3 for slow S (S2);
%   [2,:] -- number of layer where the segment is located;
%   [3,:] -- number of interface where the next reflection/transmission occurs
%==============================================================================

global Ninterface NpolX NpolY IntPol
global Nlayer
global RayCode Nsegment NMOCode
global NReflector

% Input the ray code
switch trajectory
  case 1
    RayCode(1,:) = [1, 1];     %% ray type
    RayCode(2,:) = [1, 1];     %% layers
    RayCode(3,:) = [2, 1];     %% interfaces
  case 2
    RayCode(1,:) = [2, 2];     %% ray type
    RayCode(2,:) = [1, 1];     %% layers
    RayCode(3,:) = [2, 1];     %% interfaces
  case 3
    RayCode(1,:) = [1, 1, 1, 1];     %% ray type
    RayCode(2,:) = [1, 2, 2, 1];     %% layers
    RayCode(3,:) = [2, 3, 2, 1];     %% interfaces
  case 4
    RayCode(1,:) = [2, 2, 2, 2];     %% ray type
    RayCode(2,:) = [1, 2, 2, 1];     %% layers
    RayCode(3,:) = [2, 3, 2, 1];     %% interfaces
  case 5
    RayCode(1,:) = [1, 1, 1, 1, 1, 1];     %% ray type
    RayCode(2,:) = [1, 2, 3, 3, 2, 1];     %% layers
    RayCode(3,:) = [2, 3, 4, 3, 2, 1];     %% interfaces
  case 6
    RayCode(1,:) = [2, 2, 2, 2, 2, 2];     %% ray type
    RayCode(2,:) = [1, 2, 3, 3, 2, 1];     %% layers
    RayCode(3,:) = [2, 3, 4, 3, 2, 1];     %% interfaces
end;
Nsegment = length(RayCode(1,:));
NMOCode = [Nsegment/2+1:Nsegment];
NReflector(trajectory+2) = max(RayCode(3,:));
Ninterface = max(RayCode(3,:));

%==============================================================================
% Check possible errors
if min(RayCode(2,:)) < 1  |  max(RayCode(2,:)) > Nlayer 
  error(['Ray propagates within a non-existing layer -- ', ... 
         'Check input of RayCode(2,:)']);
end;

if min(RayCode(3,:)) < 1  |  max(RayCode(3,:)) > Ninterface 
  error(['Ray hits a non-existing interface -- ' , ... 
         'Check input of RayCode(3,:)']);
end;

%==============================================================================

