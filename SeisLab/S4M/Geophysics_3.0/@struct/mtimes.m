function ds=mtimes(i1,i2)
% Multiply a dataset with another dataset or with a constant
%
% Written by: E. Rietsch: August 7, 2006
% Last updated: December 7, 2006: Add handling of PDF's
%
%       ds=mtimes(i1,i2)
% INPUT
% i1   PDF or constant
% i2   PDF or constant
%      At least one of the two operands is a PDF.
% OUTPUT
% ds   i1*i2

ds=moperation(i1,i2,'*');
