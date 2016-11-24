%==============================================================================
% Reconstruct the NMO ellipse W as a cross-section of the NMO cylinder U
% by the plane with normal _normal_
%==============================================================================

function W = cyl2ell(U, normal); 

%==============================================================================

% Construct the normal vectors to _normal_ which lie in the plane of 
% the NMO ellipse
[azm, pol90, r] = cart2sph(normal(1), normal(2), normal(3));
pol = pi/2 - pol90;
b(:,1) = [cos(pol)*cos(azm), cos(pol)*sin(azm), -sin(pol)]';
b(:,2) = [-sin(azm), cos(azm), 0]';

% Build the cross-section: see, ``SuperDix'', eqs (D7) and (D8)
for i=1:2
  for j=1:2
    W(i,j) = 0;
    for k=1:3
      for m=1:3
        W(i,j) = W(i,j) + (1/2)*(b(k,i)*b(m,j) + ...
                                 b(k,j)*b(m,i))*U(k,m);
      end;
    end;
  end;
end;

%==============================================================================

