%=============================================================================
% Solve the Christoffel equation for the vertical slowness 
%=============================================================================

function p3 = ChristEq(c, p1, p2)

%=============================================================================

% Write elements of the Christoffel matrix as polynomials of p3 
% Here, *(1) means quadratic term in p3,  *(2) -- linear,  and 
%       *(3) -- term independent on p3
G11(1) = c(5,5);   G11(2) = 0;
G11(3) = -1 + c(1,1)*p1^2 + 2*c(1,6)*p1*p2 + c(6,6)*p2^2;
G22(1) = c(4,4);   G22(2) = 0;
G22(3) = -1 + c(6,6)*p1^2 + 2*c(2,6)*p1*p2 + c(2,2)*p2^2;
G33(1) = c(3,3);   G33(2) = 0;
G33(3) = -1 + c(5,5)*p1^2 + 2*c(4,5)*p1*p2 + c(4,4)*p2^2;
G12(1) = c(4,5);   G12(2) = 0;
G12(3) = c(1,6)*p1^2 + (c(1,2) + c(6,6))*p1*p2 + c(2,6)*p2^2;
G13(1) = 0;  G13(2) = (c(1,3) + c(5,5))*p1 + (c(3,6) + c(4,5))*p2;  G13(3) = 0;
G23(1) = 0;  G23(2) = (c(3,6) + c(4,5))*p1 + (c(2,3) + c(4,4))*p2;  G23(3) = 0;

%=============================================================================

% Combine corresponding powers of p3 that appear in the determinant of
% Christoffel equation
for l=1:7
   p(l) = 0;   n = l+2;
   for i=1:3
      for j=1:3 
         for k=1:3
            if i+j+k == n
               p(l) = p(l) + det( [G11(i), G12(i), G13(i); ...
                                   G12(j), G22(j), G23(j); ...
                                   G13(k), G23(k), G33(k)] ); 
            end;
         end;
      end;
   end;
end;

% Construct polynomial which roots are to be found
for i=1:4
   pol(i) = p(2*i-1);
end;

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
   if p3(i) < p3(1)
       aa = p3(i);   p3(i) = p3(1);   p3(1) = aa;
   end;
end;
if p3(3) < p3(2)
       aa = p3(3);   p3(3) = p3(2);   p3(2) = aa;
end;

%=============================================================================

