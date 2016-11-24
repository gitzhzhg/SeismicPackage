function data=ibm2single(data)
% Convert IBM 32-bit floating-point format to IEEE 32-bit floating-point format
%      (based on the algorithm of function "ibm2num" written by Brian Farrelly)
% See also: ibm2double
%
% Written by: JC: March 2009
% Last updated: 
%
%         data2=ibm2double(data)
% INPUT 
% data    a matrix of unsigned 4-byte integers (uint32) representing data
%         in 32-bit IBM floating-point format
% OUTPUT
% data    corresponding matrix of singles (IEEE format)

% get sign from first bit
% get exponent from first byte, last 7 bits
% remove bias from exponent 
% get mantissa from last 3 bytes

data=(1-2*single(bitget(data,32)))...
    .*16.^(single(bitshift(bitand(data,uint32(hex2dec('7f000000'))),-24))- 64)...
    .* single(bitand(data,uint32(hex2dec('00ffffff'))))/2^24;
