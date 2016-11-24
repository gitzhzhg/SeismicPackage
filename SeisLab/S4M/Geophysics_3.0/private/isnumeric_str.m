function [bool,value]=isnumeric_str(str)
% Check if a string represents a number
%
% Written by: E. Rietsch: February 23, 2004
% Last updated:
%
%        bool=isnumeric_str(str)
% INPUT
% str    string
% OUTPUT
% bool   logical variable; set to true if the string represents a numeric value
%        and to false if it does not or has length zero

value=str2num(str);   %#ok  Can be more than one numeric value
if isempty(str) || isempty(value)
   bool=false;
else
   bool=true;
end
