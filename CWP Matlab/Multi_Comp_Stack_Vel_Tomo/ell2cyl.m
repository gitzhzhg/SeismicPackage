%==============================================================================
% Reconstruct the NMO cylinder U from its cross-section W1 by the plane with 
% the normal _normal_
%==============================================================================

function U = ell2cyl(W1, normal, q1, q2); 

%==============================================================================

% Construct the normal vectors to _normal_ which lie in the plane of 
% the NMO ellipse
[azm, pol90, r] = cart2sph(normal(1), normal(2), normal(3));
pol = pi/2 - pol90;
b(:,1) = [cos(pol)*cos(azm), cos(pol)*sin(azm), -sin(pol)]';
b(:,2) = [-sin(azm), cos(azm), 0]';

% Build the 3*3 matrix to be inverted to find the NMO ellipse W -- the 
% cross-section of U by the horizontal plane: see ``SuperDix'', eqs (D7) 
% and (D9)
A(1:2,1:2,1:2,1:2) = 0;
for i=1:2
  for j=1:2
    for k=1:3
      for m=1:3
        term = (1/2)*(b(k,i)*b(m,j) + b(k,j)*b(m,i));
        if k == 1  &  m == 1;
          A(i,j,1,1) = A(i,j,1,1) + term; 
        elseif k == 1  &  m == 2;
          A(i,j,1,2) = A(i,j,1,2) + term;
        elseif k == 1  &  m == 3;
          A(i,j,1,1) = A(i,j,1,1) + q1*term;
          A(i,j,1,2) = A(i,j,1,2) + q2*term;
        elseif k == 2  &  m == 1;
          A(i,j,2,1) = A(i,j,2,1) + term;
        elseif k == 2  &  m == 2;
          A(i,j,2,2) = A(i,j,2,2) + term;
        elseif k == 2  &  m == 3;
          A(i,j,1,2) = A(i,j,1,2) + q1*term;
          A(i,j,2,2) = A(i,j,2,2) + q2*term;
        elseif k == 3  &  m == 1;
          A(i,j,1,1) = A(i,j,1,1) + q1*term;
          A(i,j,2,1) = A(i,j,2,1) + q2*term;
        elseif k == 3  &  m == 2;
          A(i,j,2,1) = A(i,j,2,1) + q1*term;
          A(i,j,2,2) = A(i,j,2,2) + q2*term;
        else k == 3  &  m == 3;
          A(i,j,1,1) = A(i,j,1,1) + q1^2*term;
          A(i,j,1,2) = A(i,j,1,2) + q1*q2*term;
          A(i,j,2,1) = A(i,j,2,1) + q1*q2*term;
          A(i,j,2,2) = A(i,j,2,2) + q2^2*term;
        end; 
      end;
    end;
  end;
end;

% ...see ``SuperDix'', eq (D11)
Am = [A(1,1,1,1), 2*A(1,1,1,2), A(1,1,2,2); ...
      A(1,2,1,1), 2*A(1,2,1,2), A(1,2,2,2); ...
      A(2,2,1,1), 2*A(2,2,1,2), A(2,2,2,2)];   
Wm = [W1(1,1), W1(1,2), W1(2,2)]';
W = inv(Am)*Wm;

%==============================================================================
% Reconstruct the NMO cylinder
U = [              W(1),               W(2),  q1*W(1) + q2*W(2); ...
                   W(2),               W(3),  q1*W(2) + q2*W(3); ...
      q1*W(1) + q2*W(2),  q1*W(2) + q2*W(3), ...
                          q1^2*W(1) + 2*q1*q2*W(2) + q2^2*W(3)];

%==============================================================================

