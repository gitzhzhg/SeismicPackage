%==============================================================================
% Compute the transformation matrix R from global to local coordinate system, 
% where the reflection-transmission problem is solved. In this coordinate 
% system the normal to the reflector and the slowness vector become:
% normal --> [0, 0, 1]  and  [p1, p2, p3] --> [pl1, 0, pl3]
%==============================================================================

function [R] = global2refl(normal, p);

%==============================================================================

x3 = normal';
crosspr = cross(x3, p);   x2 = crosspr/sqrt(sum(crosspr.^2));
x1 = cross(x2, x3);

R = [x1; x2; x3];

%==============================================================================

