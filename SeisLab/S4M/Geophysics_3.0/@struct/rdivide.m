function ds=rdivide(i1,i2)
% Function divides traces of a seismic dataset by a constant or matrix
% a constant or matrix by a seismic data set
%
% Written by: E. Rietsch: September 11, 2005
% Last updated: September 21, 2006: Adapt to Matlab 7
%
%       ds=rdivide(i1,i2)
% INPUT
% i1   dataset or constant
% i2   dataset or constant
%      At least one of the two operands is a dataset
% OUTPUT
% ds   i1/i2

ds=moperation(i1,i2,'/');
