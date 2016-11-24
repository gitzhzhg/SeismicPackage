%==============================================================================
% Compute the interval NMO cylinder 
%==============================================================================

function [U, q1, q2] = intCyl(p1, p2, p3, isegment);

global Cij  
global RayCode
global f ff

%==============================================================================
[ugroup, vgr] = groupVel([p1, p2, p3], ...
                         Cij(:,:,RayCode(2,isegment)), isegment, 1);
q1 = -f(1)/f(3);   q2 = -f(2)/f(3);

% Construct the numerators of the matrix  Q = d^2 q/[d p(i), d p(j)] 
for i=1:2;   for j=1:i
  Q(i,j) = ff(i,j)*f(3)^2 - ff(i,3)*f(j)*f(3) - ...
                            ff(j,3)*f(i)*f(3) + ff(3,3)*f(i)*f(j);
end;   end;
Q(1,2) = Q(2,1);

% NMO ellipse
W = f(3)^2*sum([p1, p2, p3].*f)*inv(Q);  

% NMO cylinder
U = [               W(1,1),                W(1,2),  q1*W(1,1) + q2*W(1,2); ...
                    W(1,2),                W(2,2),  q1*W(1,2) + q2*W(2,2); ...
     q1*W(1,1) + q2*W(1,2), q1*W(1,2) + q2*W(2,2), ...
                               q1^2*W(1,1) + 2*q1*q2*W(1,2) + q2^2*W(2,2)];

%==============================================================================



