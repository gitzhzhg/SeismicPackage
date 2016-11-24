function ds=plus(i1,i2)
% Add a dataset to another dataset or to a constant
%
% Written by: E. Rietsch: August 7, 2006
% Last updated: December 7, 2006: Add handling of PDF's
%
%       ds=plus(i1,i2)
% INPUT
% i1   Dataset or constant
% i2   Dataset or constant
%      At least one of the two operands is a dataset.
% OUTPUT
% ds   i1+i2

ds=moperation(i1,i2,'+');
