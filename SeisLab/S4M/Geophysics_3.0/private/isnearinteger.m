function bool=isnearinteger(x,tol)
% Function checks if x is approximately integer (with tolerance "tol")
%
% Written by: E. Rietsch: November 10, 2005
% Last updated:
%
%        bool=isnearinteger(x,tol)
% INPUT
% x      variable to check
% tol    maximum deviation from integer; 
%        default 0
% OUTPUT
% bool logical variable (1  if abs(x-round(x)) <= tol, 
%                           0  otherwise)

if nargin == 1
   bool=mod(x,1) == 0;

else
   bool=abs(x-round(x)) <= tol;

end
