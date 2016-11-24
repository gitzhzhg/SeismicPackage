function ds=cumsum(ds)
% Function computes the cumulative sum of the traces of a seismic dataset
%
% Written by: E. Rietsch: November 4, 2006
% Last updated:
%
%        ds=cumsum(ds)

if isstruct(ds)  &&  strcmp(ds(1).type,'seismic')
   for ii=1:numel(ds)
      ds(ii).traces=cumsum(ds(ii).traces);
   end
else
   error('Function "cumsum" is not defined for this argument.')
end
