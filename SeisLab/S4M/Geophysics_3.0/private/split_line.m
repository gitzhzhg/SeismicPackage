function comp=split_line(str)
% Find the separators ".", ":" and "|" in the input string and split it into up
% to four cells
% For internal use in LAS readers
%
% Written by: E. Rietsch: December 16, 2006
% Last updated:
% 
%        comp=split_line(str)
% INPUT
% str    string to split into components
% OUTPUT
% comp   cell vector with up to four components resulting from the split;
%        empty in case of an error

comp=cell(1,4);

idx=strfind(str,'.');
comp{1}=strtrim(str(1:idx(1)-1));

idx2=strfind(str,':');
comp{2}=str(idx(1)+1:idx2(1)-1);

idx3=strfind(str,'|');
if ~isempty(idx3)
   comp{3}=strtrim(str(idx2(1)+1:idx3(1)-1));
   comp{4}=strtrim(str(idx3(1)+1:end));
else
   comp{3}=strtrim(str(idx2(1)+1:end));
end
