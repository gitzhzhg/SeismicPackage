function mnems=fix_mnemonics(mnems)
% Function modifies mnemonics that do not represent valid Matlab names
% Written by: E. Rietsch: February 10, 2005
% Last updated:
%
%
% INPUT
% mnems   original curve mnemonics
% OUPUT
% mnems   modified curve mnemonics

%	Find blanks and replace by underscores
for ii=1:length(mnems)
   mnems{ii}=strrep(mnems{ii},' ','_');
end
  

