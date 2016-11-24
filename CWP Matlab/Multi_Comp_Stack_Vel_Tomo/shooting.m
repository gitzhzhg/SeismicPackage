%==============================================================================
% Shoot the ray specified by slowness p = [p1,p2] through the model 
%==============================================================================

function xReceiv= shooting(p, ipair);

global Cij
global Source Receiv Npair 
global RayCode Nsegment NMOCode
global p1 p2 q ugr vgr xray tau time
global r

%==============================================================================
% Loop over ray segments
time = 0;
for isegment=1:Nsegment
  if isegment == 1;   
    p1(isegment) = p(1);   p2(isegment) = p(2);
    xray(:,isegment) = Source(:,ipair);   
%   Compute slowness at the first segment
    q(isegment) = christoffel(p1(isegment), p2(isegment), ...
                              Cij(:,:,RayCode(2,isegment)), isegment);
  end;

%%%  [p1(isegment), p2(isegment), q(isegment), ...
%%%   1/sqrt(p1(isegment)^2 + p2(isegment)^2 + q(isegment)^2)]

  if isnan(q(isegment)) | abs(q(isegment)) < eps;   
     xReceiv = [NaN, NaN];
     return;  
  end;
  
% Compute the unit vector in the direction of the group velocity and 
% the group velocity itself
  [ugr(:,isegment), vgr(isegment)] = ...
  groupVel([p1(isegment), p2(isegment), q(isegment)], ...
           Cij(:,:,RayCode(2,isegment)), isegment, 0); 
  
%%%  [vgr(isegment)*ugr(:,isegment)', vgr(isegment)]

% Compute the length of the ray segment that hits next interface
  length = fzero('hitInt', 0, [], [], ...
                  ugr(:,isegment), xray(:,isegment), isegment);
% r is the coordinates of the point where the ray hits the interface
  xray(:,isegment+1) = r;

% Traveltime along the segment
  tau(isegment) = length/vgr(isegment);
  time = time + tau(isegment);

% Solve the Snell's law 
  if isegment ~= Nsegment
%   Compute the normal to the interface at the reflection/transmission point
    normal = normInt(RayCode(3,isegment), r(1), r(2));

%   Construct the matrix that relates the global and local coordinates
    R = global2refl(normal, [p1(isegment), p2(isegment), q(isegment)]);

%   Transform the horizontal slowness and the stiffness tensor to 
%   local coordinates
    plocal = R*[p1(isegment), p2(isegment), q(isegment)]';
    CC = bondTransf(Cij(:,:,RayCode(2,isegment+1)), R');

%   Find the vertical slowness component in local coordinates
    qlocal = christoffel(plocal(1), plocal(2), CC, isegment+1);

%   Inverse transformation
    pnew = R'*[plocal(1), plocal(2), qlocal]';
    p1(isegment+1) = pnew(1);   p2(isegment+1) = pnew(2);
    q(isegment+1)  = pnew(3);
  end;
end;

xReceiv = xray(:,Nsegment+1);

%%% time

%==============================================================================

