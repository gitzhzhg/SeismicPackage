%==============================================================================
% Solve the Christoffel equation for vertical slowness
%==============================================================================

function q = christoffel(p1, p2, Cij, isegment);

global unity33 indexTM
global TypeAnis
global RayCode Nsegment NMOCode

%==============================================================================
layer = RayCode(2,isegment);

% Construct the Christoffel polynomial 
if TypeAnis(layer,:) == 'ISO'
% Isotropy
  c33 = Cij(3,3);   c44 = Cij(4,4);  
  f(1) = c33*c44^2;
  f(2) = 0;
  f(3) = c44*(-c44 + c33*(-2 + 3*c44*(p1^2 + p2^2))); 
  f(4) = 0;
  f(5) = (-1 + c44*(p1^2 + p2^2))*(-2*c44 + c33*(-1 + 3*c44*(p1^2 + p2^2)));
  f(6) = 0;
  f(7) = (-1 + c33*(p1^2 + p2^2))*(-1 + c44*(p1^2 + p2^2))^2;

else
% Anisotropy  
  for i=1:3;   for j=1:3;
% Construct the elements of Christoffel matrix 
% U(i,j) = c(i,i1,j1,j)*p(i1)*p(j1) - unity33(i,j)
% as polynomials of q = p(3)
%   Zero powers of q:
    U(i,j,1) = Cij(indexTM(i,1),indexTM(1,j))*p1^2 + ...
              (Cij(indexTM(i,1),indexTM(2,j)) + ...
               Cij(indexTM(i,2),indexTM(1,j)))*p1*p2 + ...
               Cij(indexTM(i,2),indexTM(2,j))*p2^2 - unity33(i,j);
%   First powers of q:
    U(i,j,2) = (Cij(indexTM(i,1),indexTM(3,j)) + ...
                Cij(indexTM(i,3),indexTM(1,j)))*p1 + ...
               (Cij(indexTM(i,2),indexTM(3,j)) + ...
                Cij(indexTM(i,3),indexTM(2,j)))*p2;
%   Second powers of q:
    U(i,j,3) = Cij(indexTM(i,3),indexTM(3,j));
  end;   end;

% Construct the Christoffel polynomial  f(q) = 0  with the roots q = p(3)
  for l=1:7
    f(8-l) = 0;   
    for i=1:3;   for j=1:3;   for k=1:3;
      if i+j+k == l+2
%       Add the determinants
        f(8-l) = f(8-l) + ...
                 U(1,1,i)*(U(2,2,j)*U(3,3,k) - U(2,3,j)*U(3,2,k)) + ...
                 U(1,2,i)*(U(3,1,j)*U(2,3,k) - U(2,1,j)*U(3,3,k)) + ...
                 U(1,3,i)*(U(2,1,j)*U(3,2,k) - U(3,1,j)*U(2,2,k));
      end;
    end;   end;   end;
  end;
end;

%==============================================================================
% Solve the Christoffel equation
p3found = roots(f);

%==============================================================================
% Select appropriate roots

% Distinguish between up- and down-going rays
if RayCode(3,isegment) > RayCode(2,isegment);
  direction = +1;
else
  direction = -1;
end;

nroot = 0;
for iroot=1:6
  if abs(imag(p3found(iroot))) < 1.e-4  & ...
     sign(real(p3found(iroot))) == direction
    nroot = nroot + 1;
    p3select(nroot) = real(p3found(iroot));
  end;
end;

if nroot ~= 0;   p3sort = sort(abs(p3select));   end;
 
%==============================================================================
% Separate rays according to the type
% ??? I still have to think what to do if, say, nroot = 4

if nroot == 3
% Normal case: all three roots are real
  q = direction*p3sort(RayCode(1,isegment));

elseif nroot == 2
% One complex root
  if RayCode(1,isegment) == 1;   
    q = NaN;  
%%  nroot
%%  p3found
  else
    q = direction*p3sort(RayCode(1,isegment)-1);
  end;

elseif nroot == 1
% Two complex roots
  if RayCode(1,isegment) == 1  |  RayCode(1,isegment) == 2;
    q = NaN;
%%  nroot
%%  p3found
  else
    q = direction*p3sort(1);
  end;

else
% All three roots are complex and  nroot = 0
  q = NaN;   
%%  nroot
%%  p3found
end;

%==============================================================================

