function mat=mycumsum(mat,dim)
% Modification of Matlab function "cumsum".
% For vectors, "mycumsum(x)" is a vector containing the cumulative sum of
% the elements of "x". For matrices, "mycumsum(x)" is a matrix the same size
% as "x" containing the cumulative sums over each column.  For N-D
% arrays, "mycumsum(x)" operates along the first non-singleton dimension.
%
% The difference between "cumsum" and "mycumsum" is the following":
% The cumultive sum is always computed in double precision regardless of the 
% precision of the input data. The result, however, is converted to the
% precision of the input data.
%
% Written by: E. Rietsch: December 27, 2006
% Last updated:
%
%         mat=mycumsum(mat,dim)
% INPUT
% mat     vector or matrix to be summed
% dim     dimension along which to sum
% OUTPUT
% mat     cumulative sum of "mat"

if isa(mat,'double')
   if nargin == 1
      mat=cumsum(mat);
   elseif nargin == 2
      mat=cumsum(mat,dim);
   else
      error('Number of arguments must be 1 or 2.')
   end

else
   if nargin == 1
      mat=cumsum(double(mat));
   elseif nargin == 2
      mat=cumsum(double(mat),dim);
   else
      error('Number of arguments must be 1 or 2.')
   end
   mat=single(mat);
end
