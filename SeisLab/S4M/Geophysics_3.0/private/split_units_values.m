function comp=split_units_values(str)
% Find the first blank in the input string and split into two cells
% (before and after the string); remove leading and trailing blanks
% For internal use in LAS readers
%
% Written by: E. Rietsch: December 17, 2006
% Last updated:
% 
%        comp=split_units_values(str)
% INPUT
% str    string to split into components
% OUTPUT
% comp   cell vector with two components resulting from the split;
%        empty in case of an error

comp=cell(1,2);
temp=strfind(str,' ');
if isempty(temp)
   comp{1}=str;

elseif temp(1) == 1		% No units
   comp{2}=strtrim(str);

else
   comp{1}=strtrim(str(1:temp(1)-1));
   comp{2}=strtrim(str(temp(1)+1:end));
end
