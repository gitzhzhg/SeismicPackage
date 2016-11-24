function ds=mrdivide(i1,i2)
% Divide a dataset by another dataset or by a constant or divide a 
% constant by a dataset
%
% Written by: E. Rietsch: August 7, 2006
% Last updated: December 7, 2006: Add handling of PDF's
%
%       ds=rmdivide(i1,i2)
% INPUT
% i1   dataset or constant
% i2   dataset or constant
%      At least one of the two operands is a dataset
% OUTPUT
% ds   i1/i2

ds=moperation(i1,i2,'/');
