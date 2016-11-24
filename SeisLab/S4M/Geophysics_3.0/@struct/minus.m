function ds=minus(i1,i2)
% Subtract a dataset from another dataset or from a constant
%
% Written by: E. Rietsch: August 7, 2006
% Last updated: December 7, 2006: Add handling of PDF's
%
%       ds=minus(i1,i2)
% INPUT
% i1   datset or constant
% i2   datset or constant
%      At least one of the two operands is a dataset.
% OUTPUT
% ds   i1-i2

ds=moperation(i1,i2,'-');
