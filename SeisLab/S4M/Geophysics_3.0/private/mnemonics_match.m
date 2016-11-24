function [index,ier]=mnemonics_match(list,mnemonics)
% Function outputs index vector representing the position of each element 
% of "mnemonics" in the cell array "list"
% an error message is issued if en element of "mnemonics" is not found in
% "list"
% INPUT
% list      cell array with mnemonics
% mnemonics string or cell array of mnemonics
% OUTPUT
% index     index vector such that list(index(ii))=mnemonics(ii);
% ier       error code:    error ==> ier = 1 
%                       no error ==> ier = 0
%             [index,ier]=mnemonics_match(list,mnemonics)

if ~iscell(mnemonics)
  mnemonics={mnemonics};
end
% index=find(ismember(list,mnemonics));
index=ismember_ordered(list,mnemonics);
ier=0;
lm=length(mnemonics);
if length(index) ~= lm
  for ii=1:lm
    if ~ismember(list,mnemonics(ii))
      disp([' Mnemonic ',mnemonics{ii},' not found'])
    end
  end
  disp(' Available mnemmonics are:')
  disp(list(:)')
  ier=1;
end

