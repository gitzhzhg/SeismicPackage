function bool=isnull(ds)
% Check if field "null" of dataset "ds" exists and, if yes, if it is 
% set to NaN.
%
% Written by: E. Rietsch: November 22, 2006
% Last updated:
%
%       bool=isnull(ds)
% INPUT
% ds    dataset (structure)
% OUTPUT
% bool  logical variable; true if "ds" has a field "null" AND if it is 
%       set to NaN.


if isfield(ds,'null')  &&  ~isempty(ds.null)  &&  isnan(ds.null)
   bool=true;
else
   bool=false;
end
