%==============================================================================
% Shoot the ray specified by slowness p = [p1,p2] through the model 
%==============================================================================

function velCon = inputVelCon;

global Cij
global RayCode Nsegment NMOCode

%==============================================================================
% Loop over ray segments on the way down
for isegment=Nsegment/2:Nsegment/2
% Costraint for the vertical velocity
  p1 = 0;   p2 = 0;
% Compute the vertical slowness 
  q = christoffel(p1, p2, Cij(:,:,RayCode(2,isegment)), isegment);
% Compute the unit vector in the direction of the group velocity and 
% the group velocity itself
  [ugr, velCon] = groupVel([p1, p2, q], ...
           Cij(:,:,RayCode(2,isegment)), isegment, 0); 
end;
%==============================================================================

