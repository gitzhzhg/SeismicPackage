function ds=abs(ds)
% Function takes the absolute value of the traces of a seismic dataset
%
% Written by: E. Rietsch: August 18, 2005
% Last updated: September 18, 2006: Handle structure arrays

if isstruct(ds)  &&  strcmp(ds(1).type,'seismic')
   for ii=1:numel(ds)
      ds(ii).traces=abs(ds(ii).traces);
   end
else
   error('Function "abs" is not defined for this argument.')
end
