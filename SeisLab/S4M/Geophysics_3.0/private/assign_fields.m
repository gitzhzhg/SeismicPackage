function param=assign_fields(param,newparam)
% Copy fields from structure "newparam" to like-named fields of 
% structure "param"
%
% Written by: E. Rietsch: October 15, 2006
% Last updated:
%
%          param=assign_fields(param,newparam)
% INPUT
% param    structure with fields representing default parameters
% newparam  structure with fields representing new parameters
% OUTPUT
% param    input structure with updated fields

%	Check if the fields of newparam are a subset of those of "param"
if isempty(newparam)
   return
end

fields=fieldnames(param);
newfields=fieldnames(newparam);
bool=ismember(newfields,fields);
if ~all(bool)
   disp(' The following fieldes are probably misspelled:') 
   disp([' ',cell2str(newfields(~bool),',')])
   disp(' Possible fields are:')
   disp([' ',cell2str(fields,',')])
   error('Abnormal termination')
end

for ii=1:length(newfields)
   param.(newfields{ii})=newparam.(newfields{ii});
end
