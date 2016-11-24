%=============================================================================
% Solve the Christoffel equation for the vertical slowness 
%=============================================================================

function Vel = ChristEq(c, p1, p2, p3)

%=============================================================================

% Write elements of the Christoffel matrix as polynomials of p3 
% Here, *(1) means quadratic term in p3,  *(2) -- linear,  and 
%       *(3) -- term independent on p3
GA11(1) = c(5,5)*p3^2;  GA11(2) = 0;
GA11(3) = c(1,1)*p1^2 + 2*c(1,6)*p1*p2 + c(6,6)*p2^2;

GA22(1) = c(4,4)*p3^2;   GA22(2) = 0;
GA22(3) = c(6,6)*p1^2 + 2*c(2,6)*p1*p2 + c(2,2)*p2^2;

GA33(1) = c(3,3)*p3^2;   GA33(2) = 0;
GA33(3) = c(5,5)*p1^2 + 2*c(4,5)*p1*p2 + c(4,4)*p2^2;

GA12(1) = c(4,5)*p3^2;   GA12(2) = 0;
GA12(3) = c(1,6)*p1^2 + (c(1,2) + c(6,6))*p1*p2 + c(2,6)*p2^2;

GA13(1) = 0;  GA13(2) = (c(1,3) + c(5,5))*p1*p3 + (c(3,6) + c(4,5))*p2*p3;  GA13(3) = 0;

GA23(1) = 0;  GA23(2) = (c(3,6) + c(4,5))*p1*p3 + (c(2,3) + c(4,4))*p2*p3;  GA23(3) = 0;

G11 = GA11(1)+GA11(2)+GA11(3);
G12 = GA12(1)+GA12(2)+GA12(3);
G13 = GA13(1)+GA13(2)+GA13(3);
G22 = GA22(1)+GA22(2)+GA22(3);
G23 = GA23(1)+GA23(2)+GA23(3);
G33 = GA33(1)+GA33(2)+GA33(3);


%=============================================================================
% Construct polynomial which roots are to be found

pol(1) = -1;
pol(2) = G11+G22+G33;
pol(3) = G13^2 + G12^2 + G23^2 - G11*G22 - G11*G33 - G22*G33;
pol(4) = G11*G22*G33 + 2*G12*G13*G23 - G13^2*G22 - G12^2*G33 - G23^2*G11;

%=============================================================================

% Solve Christoffel equation
p3s = roots(pol);
p3  = sqrt(p3s);

% Check if there are evanescent waves
for i=1:3
   if abs(imag(p3(i))) > 1.e-4
      fprintf('*** Evanescent reflection for  p1 = %7.4f,  p2 = %7.4f \n', ...
              p1, p2); 
   end;
end;

% Put the roots in assending order. This way, they will correspond to
% P-, S1-, and S2-waves 
for i=2:3
   if p3(i) > p3(1)
       aa = p3(i);   p3(i) = p3(1);   p3(1) = aa;
   end;
end;
if p3(3) > p3(2)
       aa = p3(3);   p3(3) = p3(2);   p3(2) = aa;
end;

Vel = p3;

%=============================================================================

