function bool=isnumber(str)
% Check if a string contains only numbers, spaces, commas, decimal points,
% semicolons, +/- signs
%
% Written by: E. Rietsch: August 16, 2003
% Last updated: 
%
%       bool=isnumber(str)
% INPUT
% str   string to be checked
% OUTPUT
% bool  logical variable; bool is set to false if it contains characters 
%       other than those listed above or if the string has length 0
%       bool=true otherwise

if isempty(str)
   bool=false;
else
   bool=all(ismember(double(str),double('1234567890,.; +-')));
end
