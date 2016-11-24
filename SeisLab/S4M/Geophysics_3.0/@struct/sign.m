function ds=sign(ds)
% Function takes the sign of the traces of a seismic dataset
%
% Written by: E. Rietsch: September 11, 2005
% Last updated: September 18, 2006: Handle structure arrays

if isstruct(ds)  &&  strcmp(ds(1).type,'seismic')
   for ii=1:numel(ds)
      ds(ii).traces=sign(ds(ii).traces);
   end
else
   error('Operator "sign" is not defined for this argument.')
end
