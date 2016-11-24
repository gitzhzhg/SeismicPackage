%==============================================================================
% Compute the group velocity 
%==============================================================================

function [ugroup, vgr] = groupVel(p, Cij, isegment, NMOindex);

%==============================================================================
% The second derivatives of th Christofel determinant are computed
% if NMOindex is nonzero
%==============================================================================

global unity33 indexTM
global TypeAnis
global RayCode
global f ff

%==============================================================================
layer = RayCode(2,isegment);

if TypeAnis(layer,:) == 'ISO'
  f = 2*p;

else
% Construct the elements Christoffel matrix G(i,l) and find the partial 
% derivatives  dG(i,l)/d p(m)
  for i=1:3;   for l=1:3;
    G(i,l) = -unity33(i,l);
    for k=1:3;   for j=1:3;   
      G(i,l) = G(i,l) + Cij(indexTM(i,j),indexTM(k,l))*p(j)*p(k);
    end;   end;
  end;   end;

  for i=1:3;   for l=1:3;   for m=1:3;
    dG(i,l,m) = 0;
    for k=1:3
      dG(i,l,m) = dG(i,l,m) + (Cij(indexTM(i,m),indexTM(k,l)) + ...
                               Cij(indexTM(i,k),indexTM(m,l)))*p(k);
    end;
  end;   end;   end;

% Find the derivatives of the determinant  d det(G)/d p(m)
  for m=1:3
    f(m) = det([dG(:,1,m),    G(:,2),    G(:,3)]) + ...
           det([   G(:,1), dG(:,2,m),    G(:,3)]) + ...
           det([   G(:,1),    G(:,2), dG(:,3,m)]);
  end;
end;
 
%==============================================================================
% Group-velocity vector
if abs(sum(p.*f)) < eps
% Isotropic group velocity to avoid division by zero
  gr = p/sqrt(p.*p);
else
  gr = f/sum(p.*f);
end;
vgr = sqrt(sum(gr.^2));
ugroup = gr'/vgr;

%==============================================================================
% Compute the second-order derivatives of the Christoffel determinant  
if NMOindex ~= 0
  if TypeAnis(layer,:) == 'ISO'
    ff = 2*unity33;  

  else
%   The second derivatives of the Christoffel matrix
    for i=1:3;   for l=1:3;   for m=1:3;   for n=1:3;
      ddG(i,l,m,n) = Cij(indexTM(i,m),indexTM(n,l)) + ...
                     Cij(indexTM(i,n),indexTM(m,l));
    end;   end;   end;   end; 

%   The second derivatives of the Christoffel determinant
%   d^2 det(G)/[d p(m) d p(n)]
    for m=1:3;   for n=1:m;
      ff(m,n) = det([ddG(:,1,m,n),       G(:,2),       G(:,3)]) + ...
                det([   dG(:,1,m),    dG(:,2,n),       G(:,3)]) + ...
                det([   dG(:,1,m),       G(:,2),    dG(:,3,n)]) + ...
                det([   dG(:,1,n),    dG(:,2,m),       G(:,3)]) + ...
                det([      G(:,1), ddG(:,2,m,n),       G(:,3)]) + ...
                det([      G(:,1),    dG(:,2,m),    dG(:,3,n)]) + ...
                det([   dG(:,1,n),       G(:,2),    dG(:,3,m)]) + ...
                det([      G(:,1),    dG(:,2,n),    dG(:,3,m)]) + ...
                det([      G(:,1),       G(:,2), ddG(:,3,m,n)]);
    end;   end;
%   Fill the symmetric part
    ff(1,2) = ff(2,1);   ff(1,3) = ff(3,1);   ff(2,3) = ff(3,2);
  end;
end;

%==============================================================================












