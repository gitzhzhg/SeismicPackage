%==============================================================================
% Velocity constraint -- group velocity in a given direction 
%==============================================================================

function [velDiff] = inverVelCon(param, Ntrajectory, velCon)

global Cij

%==============================================================================
NvelCon = 0;

% Loop over ray trajectories
for trajectory=1:Ntrajectory
% Input ray code
  clear global RayCode Nsegment
  global RayCode Nsegment 
  inverRayCode(trajectory);

% Input Cij's
  inverCij(param);

% Loop over ray segments on the way down
  for isegment=Nsegment/2:Nsegment/2
%   Costraint for the vertical velocity
    p1 = 0;   p2 = 0;
%   Compute the vertical slowness 
    q = christoffel(p1, p2, Cij(:,:,RayCode(2,isegment)), isegment);
%   Compute the unit vector in the direction of the group velocity and 
%   the group velocity itself
    [ugr, vel] = groupVel([p1, p2, q], ...
                 Cij(:,:,RayCode(2,isegment)), isegment, 0); 
    NvelCon = NvelCon + 1;
    velDiff(NvelCon) = 1 - vel/velCon(NvelCon); 
  end;
end;

%==============================================================================

