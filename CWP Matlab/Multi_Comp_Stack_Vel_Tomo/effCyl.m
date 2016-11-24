%==============================================================================
% Compute the effective NMO cylinder through the Dix-type averaging

% !! At its current state the procedure is written for PLANE model
% !! interfaces
%==============================================================================

function Ueff = effCyl(p1, p2, p3, ray, tau, segments);

global Ninterface NpolX NpolY IntPol
global IntPolX IntPolY
global IntPolXX IntPolXY IntPolYY
global RayCode

% The array segments contains the sequence of the ray segments used in the 
% Dix-type averaging procedure. For example, if the RayCode has the form:
% RayCode(1,:) = [1, 1, 1, 1] (ray type),
% RayCode(2,:) = [1, 2, 2, 1] (layers),
% RayCode(3,:) = [2, 3, 2, 1] (interfaces),
% the array segments = [3, 4], which denotes the 3rd and 4th upgoing segments
% from the zero-offset reflection point. 

%==============================================================================
% The Dix-type averaging along the specified ray segments
nseg = length(segments);
time = 0;

for iseg=1:nseg
  sg = segments(iseg);
% [sg, tau(sg)]
  time = time + tau(sg);
  if iseg == 1;
    [Ueff, q1eff, q2eff] = intCyl(p1(sg), p2(sg), p3(sg), sg);
  else
    Ueff = ell2cyl(Weff, normal, q1int, q2int);
  end;
  if iseg == nseg;   break;   end;

  sg1 = segments(iseg+1);
  if sg < sg1
    sgm = sg;
    sgr = sg+1;
    pp1(sg) = p1(sg);     pp2(sg) = p2(sg);     pp3(sg) = p3(sg);
    pp1(sg1) = p1(sg1);   pp2(sg1) = p2(sg1);   pp3(sg1) = p3(sg1);
  else
    sgm = sg1;
    sgr = sg;
    pp1(sg) = -p1(sg);    pp2(sg) = -p2(sg);    pp3(sg) = -p3(sg);
    pp1(sg1) = -p1(sg1);  pp2(sg1) = -p2(sg1);  pp3(sg1) = -p3(sg1);
  end;
%[pp1(sg), pp2(sg), pp3(sg), sg]
%[pp1(sg1), pp2(sg1), pp3(sg1), sg1]

% Find the normal to the interface which the ray hits next
  normal = normInt(RayCode(3,sgm), ray(1,sgr), ray(2,sgr));

% Compute the effective NMO ellipses to be averaged
  Weff = cyl2ell(Ueff, normal);
% Weff
% Correct Weff for the interface curvature
  curv = curvInt(RayCode(3,sgm), ray(1,sgr), ray(2,sgr));
% Rotate the matrix curv to the coordinates related to the interface normal
  [azm, pol90, r] = cart2sph(normal(1), normal(2), normal(3));
  alph = [cos(azm), sin(azm); -sin(azm), cos(azm)];
  curvR = alph*curv*alph';
% Account for the fact that the second derivatives in curv are supposed to
% be taken with respect to the arclength on the interface rather than with
% respect to the global spatial coordinates
  curvv = [curvR(1,1)*normal(3)^2, curvR(1,2)*normal(3); ...
           curvR(2,1)*normal(3),   curvR(2,2)];  
  Weff = Weff - time*(pp3(sg1)-pp3(sg))*curvv;
% Weff

%[RayCode(3,sgm), ray(1,sgr), ray(2,sgr)]
%pp = [(pp1(sg1)-pp1(sg)), (pp2(sg1)-pp2(sg)), (pp3(sg1)-pp3(sg))] 
%normal'
%curvv
%[ sg, sg1, sgm, sgr, ...
%  normal(2)*pp(3) - normal(3)*pp(2), ...
%  -normal(1)*pp(3) + normal(3)*pp(1), ...
%  normal(1)*pp(2) - normal(2)*pp(1)] 

% Compute the interval NMO ellipse
  [Uint, q1int, q2int] = intCyl(pp1(sg1), pp2(sg1), pp3(sg1), sg1);
  Wint = cyl2ell(Uint, normal);

% The Dix-type averaging
  W = inv( (time*inv(Weff) + tau(sg1)*inv(Wint))/(time+tau(sg1)) );
  Weff = W;
end;

%==============================================================================

