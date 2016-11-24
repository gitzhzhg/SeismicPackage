%==============================================================================
% Shoot the ray specified by slowness p = [p1,p2] through the model 
%==============================================================================

function penalty = invershot(timeData, pData, ipair);

global Cij
global Source Receiv Npair
global RayCode Nsegment NMOCode
global p1 p2 q ugr vgr xray tau time
global r
global Xint Yint Zint Nint Rint

%==============================================================================
% Initial settings
penalty = 0;
timeComp = 0;

% Loop over ray segments
for isegment=1:Nsegment/2
  if isegment == 1;   
    p1(isegment) = pData(1);   p2(isegment) = pData(2);
    xray(:,isegment) = Source(:,ipair);   
%   Compute slowness at the first segment
    q(isegment) = christoffel(p1(isegment), p2(isegment), ...
                              Cij(:,:,RayCode(2,isegment)), isegment);
  end;

  if isnan(q(isegment)) | abs(q(isegment)) < eps;   
    penalty = penalty + 1;
%%  [p1(isegment), p2(isegment),q(isegment)]
%%  Cij(:,:,RayCode(2,isegment))
%%  RayCode(1,isegment)
%%  stop
    return;  
  end;
  
% Compute the unit vector in the direction of the group-velocity and 
% the group-velocity itself
  [ugr(:,isegment), vgr(isegment)] = ...
  groupVel([p1(isegment), p2(isegment), q(isegment)], ...
           Cij(:,:,RayCode(2,isegment)), isegment, 0);

  if isegment ~= Nsegment/2 
%   Compute the length of the ray segment that hits next interface
    length = fzero('hitInt', 0, [], [], ...
                    ugr(:,isegment), xray(:,isegment), isegment);
%   r is the coordinates of the point where the ray hits the interface
    xray(:,isegment+1) = r;

%   Traveltime along the segment
    tau(isegment) = length/vgr(isegment);
    timeComp = timeComp + tau(isegment);

%   Solve the Snell's law 
%   Compute the normal to the interface a
    normal(:,isegment) = normInt(RayCode(3,isegment), r(1), r(2));  

%   Construct the matrix that relates the global and local coordinates
    R = global2refl(normal(:,isegment), ...
                    [p1(isegment), p2(isegment), q(isegment)]);

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

  else
%   The ray ends at the reflector 
%   Traveltime at the last segment
    tau(isegment) = timeData/2 - timeComp;
%   Coordinates of the reflection point
    xray(:,isegment+1) = xray(:,isegment) + ...
                         ugr(:,isegment)*tau(isegment)*vgr(isegment);
    Xint(ipair) = xray(1,isegment+1);
    Yint(ipair) = xray(2,isegment+1);
    Zint(ipair) = xray(3,isegment+1);

%   Normal to the reflector
    nrm = [p1(isegment), p2(isegment), q(isegment)]./ ...
          sqrt(p1(isegment)^2 + p2(isegment)^2 + q(isegment)^2);
    Nint(:,ipair) = nrm';
    Rint = RayCode(3,isegment);
  end;
end;

%==============================================================================
% Reconstruct the upgoing part of zero-offset ray
for isegment=Nsegment/2+1:Nsegment
  jsegment = Nsegment+1-isegment;
  p1(isegment) = -p1(jsegment);
  p2(isegment) = -p2(jsegment);
  q(isegment) = -q(jsegment);
  ugr(:,isegment) = -ugr(:,jsegment);
  vgr(isegment) = vgr(jsegment);
  xray(:,isegment+1) = xray(:,jsegment);
  tau(isegment) = tau(jsegment);
end;

% Nsegment
% p1
% p2
% q
% ugr
% vgr
% xray
% tau

%==============================================================================

