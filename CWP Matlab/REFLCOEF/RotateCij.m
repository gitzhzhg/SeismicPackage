%=============================================================================
% Rotate the matrix Cij in the horizontal plane 
%=============================================================================

function Cr = RotateCij(C, azm)

% The idea is based on one-to-one correspondence of [6*6] matrix Cm and 
% [3*3*3*3] tensor Ct given by
%   Cm(n(i,j),n(k,l)) = Ct[i,j,k,l]
% with [3*3] matrix  
N = [1,6,5; 6,2,4; 5,4,3];

% The rotation matrix:
azim = azm*pi/180;
af = [ cos(azim), -sin(azim),  0; ...
       sin(azim),  cos(azim),  0; ...
               0,          0,  1];

% Input vectors of indeces of 13 Cij's that may change due to rotation in the 
% horizontal plane:  11 12 13 16 22 23 26 33 36 44 45 55 66
ii1 = [1  1  1  1  2  2  2  3  3  2  2  1  1];
ij1 = [1  1  1  1  2  2  2  3  3  3  3  3  2];
ik1 = [1  2  3  1  2  3  1  3  1  2  1  1  1];
il1 = [1  2  3  2  2  3  2  3  2  3  3  3  2];

for in=1:13
   i1 = ii1(in);   j1 = ij1(in);   k1 = ik1(in);   l1 = il1(in);
   Cr(N(i1,j1),N(k1,l1)) = 0;
   for i=1:3
      for j=1:3
         for k=1:3
            for l=1:3
               Cr(N(i1,j1),N(k1,l1)) = Cr(N(i1,j1),N(k1,l1)) + ...
                  af(i1,i)*af(j1,j)*af(k1,k)*af(l1,l)*C(N(i,j),N(k,l));
            end;
         end;
      end;
   end;
end;

% Make symmetric matrix
for i=1:6
   for j=1:i-1
      Cr(i,j) = Cr(j,i);
   end;
end;

%=============================================================================

