function ds=flipud(ds)
% Flip direction of numeric fields and cell vectors of a specific 
% structure 
%
% Written by: E. Rietsch: July 4, 2008
% Last updated:
%
%        ds=flipud(ds)
% INPUT
% ds     structure with datasets of type "stock"
% OUTPUT
% ds     input structure with numeric fields and cell vectors flipped

% UPDATE HISTORY

if isstock(ds)
   fnames=fieldnames(ds);

%       Modify all fields of the structure that are numeric and double-precision
   for jj=1:length(ds)
      for ii=find(structfun(@(x) (isnumeric(x) || iscell(x)),ds(jj)))'
         ds(jj).(fnames{ii})=flipud(ds(jj).(fnames{ii}));
      end
   end
else
   if isempty(ds)
      error('Operator "flipud" is not defined for an empty dataset.')
   else
      error('Operator "flipud" is not defined for the type of this argument.')
   end
end
