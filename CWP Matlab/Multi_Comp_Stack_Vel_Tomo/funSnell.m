%==============================================================================
% Calculate the slowness of reflected/transmitted ray that satisfies
% Snell's law: [p x n] = const 
%==============================================================================

function f = funSnell(pp, p01, p02, p03, normal, isegment)   

% pp is the horizontal slowness component of reflected/transmitted ray
%==============================================================================

global psnell

%==============================================================================
if abs(normal(1)) < eps
  psnell(1) = p01;
  psnell(2) = pp;
  psnell(3) = christoffelOLD(psnell(1), psnell(2), isegment+1);
  if isnan(psnell(3));
    psnell(3) = 0;
  end;   
  f = (p02-psnell(2))*normal(3) - (p03-psnell(3))*normal(2);
%%[p01,  p02,  p03, psnell(1), psnell(2), psnell(3), f]
%%pause
%==============================================================================
elseif abs(normal(2)) < eps 
  psnell(1) = pp;
  psnell(2) = p02;
  psnell(3) = christoffelOLD(psnell(1), psnell(2), isegment+1);
  if isnan(psnell(3));
    psnell(3) = 0;
  end;   
  f = (p01-psnell(1))*normal(3) - (p03-psnell(3))*normal(1);
%%[p01,  p02,  p03, psnell(1), psnell(2), psnell(3), f]
%%pause

%==============================================================================
else
% Transform normal = [normal(1), normal(2), normal(3)] to spherical 
% coordinates:
% normal = [sin(pol)*cos(azm), sin(pol)*sin(azm), cos(pol)]
  [azm, pol90, r] = cart2sph(normal(1), normal(2), normal(3));
  pol = pi/2 - pol90;
% Projection of the slowness p0 onto the vector
% d(normal)/d(azm) = [-sin(azm), cos(azm), 0] which belongs 
% to the plane orthogonal to normal
  pazm = -p01*sin(azm) + p02*cos(azm);
  psnell(1) = pp;
  psnell(2) = (pazm + psnell(1)*sin(azm))/cos(azm);
  psnell(3) = christoffelOLD(psnell(1), psnell(2), isegment+1);
  if isnan(psnell(3));   
    psnell(3) = 0;
  end;
% Projection of the slowness p0 onto the vector
% d(normal)/d(pol) = [cos(pol)*cos(azm), cos(pol)*sin(azm), -sin(pol)], 
% which also belongs to the plane orthogonal to normal
  f = (p01-psnell(1))*cos(pol)*cos(azm) + ...
      (p02-psnell(2))*cos(pol)*sin(azm) - ...
      (p03-psnell(3))*sin(pol);

% [normal(1), normal(2), normal(3)]
% [p01,  p02,  p03, psnell(1), psnell(2), psnell(3), f]
% pause

end;

%==============================================================================

