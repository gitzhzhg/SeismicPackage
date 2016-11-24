function y = lamwin(a,b,k)
% LAMWIN creates a lamoureux window 
% LAMWIN(a,b,k) where a,b k are non-negative integers
% k indicates the degree of smoothness (k continuous derivatives)
%
% NOTE: This SOFTWARE may be used by any individual or corporation for any purpose
% with the exception of re-selling or re-distributing the SOFTWARE.
% By using this software, you are agreeing to the terms detailed in this software's
% Matlab source file.

% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by
% its 'AUTHOR' (identified above) and the CREWES Project.  The CREWES
% project may be contacted via email at:  crewesinfo@crewes.org
%
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) This SOFTWARE may be used by any individual or corporation for any purpose
%    with the exception of re-selling or re-distributing the SOFTWARE.
%
% 2) The AUTHOR and CREWES must be acknowledged in any resulting publications or
%    presentations
%
% 3) This SOFTWARE is provided "as is" with no warranty of any kind
%    either expressed or implied. CREWES makes no warranties or representation
%    as to its accuracy, completeness, or fitness for any purpose. CREWES
%    is under no obligation to provide support of any kind for this SOFTWARE.
%
% 4) CREWES periodically adds, changes, improves or updates this SOFTWARE without
%    notice. New versions will be made available at www.crewes.org .
%
% 5) Use this SOFTWARE at your own risk.
%
% END TERMS OF USE LICENSE

% a is the length of the rampup (and down), b is the length of plateau

% we build an odd polynomial p(x) of degree 2k+1, with the first k derivatives
% equal to zero at x = 1. This involves solving a linear system, whose
% entries are related to the derivatives of p. The code is weird because
% we only keep track of non-zero coefficients in p, and order them the
% way that MATLAB likes polys to be ordered (higher coefficients first).

matt = zeros(k+1,k+1); % a matrix we invert to find ploynomial p.
matt(1,:) = ones(1,k+1);
for j=1:k
    matt(j+1,:) = matt(j,:).*(((2*k+2):-2:2) - j);
end
p = zeros(k+1,1);
p(1) = 1;
p = inv(matt)*p; % the polynomial coefficients for p(x)

x = -1:(2/(a-1)):1; % x values between -1 and 1
y = polyval(p,x.*x).*x; % the ramp up, from -1 to -1
y = [.5 + .5*y, ones(1,b), .5 + .5*fliplr(y)]; % ramp up, constant, ramp down.