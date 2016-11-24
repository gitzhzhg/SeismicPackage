function ds=imag(ds)
% Function takes the imaginary part of the traces of a seismic dataset
%
% Written by: E. Rietsch: August 9, 2006
% Last updated: September 18, 2006: Handle structure arrays

if isstruct(ds)  &&  strcmp(ds(1).type,'seismic')
   for ii=1:numel(ds)
      ds(ii).traces=imag(ds(ii).traces);
   end
else
   error('Operator "imag" is not defined for this argument.')
end
