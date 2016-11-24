function [nearest_b,idx,err]=nearest_intersect(a,b,maxerr)
% Analog to Matlab function "intersect" for numeric data; does not require 
% identity but only "nearness".
%
% OBSOLETE: Replace by "approx_intersect".
%
% Written by: E. Rietsch: September 26, 2003
% Last updated: April 24, 2004: bug fix
%
%       [nearest_b,idx,err]=nearest_intersect(a,b,maxerr)
% INPUT
% a     constant or numeric vector
% b     numeric vector
% maxerr   error either a constant or a vector of length "length(a)"
%       if "maxerr" is not given or "inf" there are no constraints to closeness;
%       if "maxerr == 0" the result is the same as intersect
%       Default: maxerr=inf
% OUTPUT
% nearest_b     elements of "b" closest to "a"; if "maxerr" is big enough there are
%       as many entries in "nearest_b" as there are in "a".
% idx  index vector so that "nearest_b == b(idx)"
% err   actual error |c-b(ib)|


na=length(a);
if nargin < 3
   maxerr=inf;
else
   nerr=length(maxerr);
     if nerr > 1  &&  nerr ~= na
        error('"maxerr" must be a constant or a vector with the same length as "a".')
     end
end

bu=unique(b);
nearest_b=interp1(bu,bu,a,'nearest',bu(1));
nearest_b(a >= bu(end))=bu(end);
[dummy,idx]=ismember(nearest_b,b);
err=abs(a-nearest_b);
if ~all(isinf(maxerr))
   bool=err <= maxerr;
   nearest_b=nearest_b(bool);
   idx=idx(bool);
   err=err(bool);
end
