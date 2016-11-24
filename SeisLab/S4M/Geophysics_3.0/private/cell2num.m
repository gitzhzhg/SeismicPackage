function x=cell2num(cx)
% Convert a cell array of numbers into a numeric array or 
%         a cell array of logical variables into an array of logical variables
%
% Written by: E. Rietsch: March 1, 2006
% Last updated:
%
%          x=cell2num(cx)
% INPUT
% cx       cell array whose entries a numbers or logical variables
% OUTPUT
% x        numeric or logical array of the same dimension with the same 
%          numbers or logical variables
%
% EXAMPLE
%          cell2num({1,2;3,4})
%          cell2num({true,false;false,true})

x=[cx{:}];
x=reshape(x,size(cx));
