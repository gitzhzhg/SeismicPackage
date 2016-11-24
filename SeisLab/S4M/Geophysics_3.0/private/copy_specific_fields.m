function dsout=copy_specific_fields(dsin,names,dsout)
% Function copies fields in structure "dsin" specified in cell array "names"  
% to "dsout". If they already exist they will be overwritten.
%
% Written by: E. R.: September 16, 2006
% Last updated: September 19, 2007: handle dataset vectors
%
%          dsout=copy_specific_fields(dsin,names,dsout)
% INPUT
% dsin     structure (or structure vector) from which to copy fields
% names    cell array with names of fields to copy
% dsout    structure to which to copy fields (if given it must have the same
%          dimension as "dsin".
% OUPUT
% dsout    second input structure with additional fields from first structure
%
% EXAMPLE
%          seismic=s_data;
%          seismic1=copy_specific_fields(seismic,{'first','last','step','traces'})

if nargin == 1
   dsout=dsin;
   return
end

nds=length(dsin);

if nargin == 3
   if isempty(names)  
      return
   else
      if length(dsout) ~= nds
         error('Structure vectors "dsin" and "dsout" must have the length.')
      end
   end
end


fieldsin=fieldnames(dsin(1));

index=find(ismember(fieldsin,names));
if length(index) < length(names)
   disp(' Not all specified fields exit in the first input structure')
   disp(' Fields in structure:')
   disp(cell2str(fieldsin,', '))
   disp(' Fields names specified:')
   disp(cell2str(names,', '))
   pause(0)
   error('Abnormal termination.')
end


for ii=1:length(index)
   [dsout(1:nds).(names{ii})]=deal(dsin.(names{ii}));
end
