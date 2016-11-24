function ds=power(i1,i2)
% Function takes the power of the traces of a seismic dataset
%
% Written by: E. Rietsch: September 11, 2005
% Last updated:
%
%       ds=power(i1,i2)

ds=moperation(i1,i2,'^');
