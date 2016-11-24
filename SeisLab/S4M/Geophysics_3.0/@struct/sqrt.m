function ds=sqrt(ds)
% Function takes the square root of the traces of a seismic dataset
%
% Written by: E. Rietsch: September 12, 2005
% Last updated: September 18, 2006: Handle structure arrays

if isstruct(ds)  &&  strcmp(ds(1).type,'seismic')
   for ii=1:numel(ds)
      ds(ii).traces=sqrt(ds(ii).traces);
   end
else
   error('Operator "sqrt" is not defined for this argument.')
end
