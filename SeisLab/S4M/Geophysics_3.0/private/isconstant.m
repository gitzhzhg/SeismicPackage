function bool=isconstant(a,epsilon)
% Function checks if all elements of array "a" are the same within
% limits median(a)*(1 +/- epsilon) (relative error) or,
% if median(a) < eps, within limits +/- epsilon (absolute error).
%
% Written by: E. Rietsch: November 14, 2005
% Last updated: April 16, 2008: rewritten to work with matrices
%
%          bool=isconstant(a,epsilon)
% INPUT
% a        numeric vector or matrix
% epsilon  maximum relative deviation 
%          Default: epsilon = 0
% OUTPUT
% bool     logical variable
%          true if all entries of "a" are the same within the limits,
%          false  otherwise
%
% EXAMPLE
%          bool=isconstant([1 1 1 1.2],0.5)

[a,nshifts]=shiftdim(a);

if nargin == 1  ||  epsilon == 0
   bool=all(bsxfun(@eq,a,a(1,:)));

else
   ma=median(a);
   meps=abs(ma*epsilon);
   meps(abs(ma)<eps)=abs(epsilon);
   bool=all((bsxfun(@le,a,a(1,:)+meps))  &  (bsxfun(@ge,a,a(1,:)-meps))); 
end

bool=shiftdim(bool,-nshifts);
