function bool=isregularlysampled(x,tol)
% Check if vector "x" is regularly sampled (constant increment from one 
% value to the next)
%
% Written by: E. R.: September 5, 2006
% Last updated:
%
%        bool=isregularlysampled(x,tol)
% INPUT
% x      vector to check
% tol    relative error of increments; default: 1.0e-7
% OUTPUT
% bool   logical variable; "true" if "x" is regularly sampled; "false" otherwise

if nargin == 1
   tol=1.0e-6;
end

dx=diff(x);
bool=all(isnearinteger(dx/mean(dx),tol));
