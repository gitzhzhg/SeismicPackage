function data2=ibm2double(data)
% Convert IBM 32-bit floating-point format to IEEE 64-bit floating-point format
%      (based on the algorithm of function "ibm2num" written by Brian Farrelly)
% See also: ibm2single
%
% Written by: E. Rietsch: July 19, 2009 
% Last updated:
%
%         data2=ibm2double(data)
% INPUT 
% data    a matrix of unsigned 4-byte integers (uint32) representing data
%         in IBM floating-point format
% OUTPUT
% data2   corresponding matrix of doubles (IEEE format)

% get sign from first bit
% get exponent from first byte, last 7 bits
% remove bias from exponent 
% get mantissa from last 3 bytes

data2=(1-2*double(bitget(data,32))).*16.^ ...
  (double(bitshift(bitand(data,uint32(hex2dec('7f000000'))),-24))-64) .* ...
  (double(bitand(data,uint32(hex2dec('00ffffff'))))/2^24);
