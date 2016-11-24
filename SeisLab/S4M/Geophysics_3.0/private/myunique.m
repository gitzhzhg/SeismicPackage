function [out,index]=myunique(in)
% Remove duplicates from "in" in such a way that the last one is retained
% 
% Written by: E. Rietsch: December 2, 2006
% Last updated:
%
%        out=myunique(in)
% INPUT
% in     cell vector of strings
% OUTPUT
% out    input cell vector with leading duplicates removed
% index  index such that out=in(index)
%
% EXAMPLE
%       a=[2 3 4 3 1 3 6];
%       [b,index]=myunique(a)

global S4M

if S4M.matlab_version > 7.2
   [dummy,index]=unique(in,'last'); %#ok  First output argument is not required
else
   [dummy,index]=unique(in);        %#ok  First output argument is not required
end

index=sort(index);
out=in(index);
