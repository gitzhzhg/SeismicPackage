%==============================================================================
% Compute Cij's from Grechka's MNC parameters 
%==============================================================================

function [Cij] = ... 
  grechka2Cij(Vp0, Vs0, epsilon1, epsilon2, delta1, delta2, delta3, ...
              gamma1, gamma2, zeta1, zeta2, zeta3)             

c33 = Vp0^2;   c55 = Vs0^2;
c11 = c33*(2*epsilon2 + 1);   c22 = c33*(2*epsilon1 + 1);   
c66 = c55*(2*gamma1 + 1);     c44 = c66/(2*gamma2 + 1);
c36 = zeta3*c33;
c16 = 2*zeta1*c33 + c36;      c26 = 2*zeta2*c33 + c36;          

cc = (c33 - c55)*(2*delta2*c33 + c33 - c55);
if cc > 0      
   c13 = sqrt(cc) - c55;
   else 
   c13 = 0;
end;

cc = (c33 - c44)*(2*delta1*c33 + c33 - c44);
if cc > 0      
   c23 = sqrt(cc) - c44;
   else
   c23 = 0;
end;

cc = (c11 - c66)*(2*delta3*c11 + c11 - c66);
if cc > 0      
   c12 = sqrt(cc) - c66;
   else
   c12 = 0
end;

%==============================================================================
% Construct Cij's
Cij = zeros(6);
Cij(1,1) = c11;   Cij(1,2) = c12;   Cij(1,3) = c13;   Cij(1,6) = c16;
                  Cij(2,2) = c22;   Cij(2,3) = c23;   Cij(2,6) = c26;
                                    Cij(3,3) = c33;   Cij(3,6) = c36;
Cij(4,4) = c44;   Cij(5,5) = c55;   Cij(6,6) = c66;

% Fill the symmetric part
for i=1:6
  for j=1:i-1
    Cij(i,j) = Cij(j,i);
  end;
end;

%==============================================================================

