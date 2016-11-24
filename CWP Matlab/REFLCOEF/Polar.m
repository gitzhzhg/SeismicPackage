%=============================================================================
% Compute the matrix of polarization vectors for given slownesses 
%=============================================================================

function A = Polar(c, p1, p2, p3)

%=============================================================================

N = [1,6,5; 6,2,4; 5,4,3];
nn = [2,1,1; 3,3,2];
p(1) = p1;   p(2) = p2;

% Loop over wave types
for iq=1:3
   p(3) = p3(iq);
%  Construct Christoffel matrix
   for i=1:3
      for j=1:3
         if i == j
            G(i,j) = -1;
         else
            G(i,j) = 0;
         end;
         for k=1:3
            for l=1:3
               G(i,j) = G(i,j) + c(N(i,k),N(l,j))*p(k)*p(l);
            end;
         end;
      end;
   end;

%=============================================================================

%  Find the largest minor of matrix G
   ij = 0;
   for ii=1:3
      for jj=1:ii
         ij = ij + 1;
         da = G(nn(1,ii),nn(1,jj))*G(nn(2,ii),nn(2,jj)) - ...
              G(nn(1,ii),nn(2,jj))*G(nn(2,ii),nn(1,jj));
         if ij == 1 
            i = ii;   j = jj;   d = da;
         else
            if abs(da) > abs(d) 
               i = ii;   j = jj;   d = da;
            end; 
         end;
      end;
   end;

%=============================================================================

%  Find non-normalized polarization vector
   An(nn(1,j))=( G(nn(1,i),j)*G(nn(2,i),nn(2,j)) - ...
                 G(nn(2,i),j)*G(nn(1,i),nn(2,j)) )/d;
   An(nn(2,j))=( G(nn(1,i),nn(1,j))*G(nn(2,i),j) - ...
                 G(nn(2,i),nn(1,j))*G(nn(1,i),j) )/d;
   for l=1:3
      if l == nn(1,j);   A(l,iq) = -An(nn(1,j));   end;
      if l == nn(2,j);   A(l,iq) = -An(nn(2,j));   end;
      if l == j;         A(l,iq) = 1;              end;
   end;

%  Normalize polarization vector
   b = sqrt( abs(A(1,iq))^2 + abs(A(2,iq))^2 + abs(A(3,iq))^2 );
   A(:,iq) = A(:,iq)/b;
end;

%  Make P-wave polarization not deviate from the slowness vector by more
%  than pi/2
   sc = p(1)*A(1,1) + p(2)*A(2,1) + p3(1)*A(3,1);
   if sc < 0;   A(:,1) = -A(:,1);   end;

%=============================================================================


