function ier=param_check(structure)
% Check if parameter-related fields of a structure are compatible
%
% Written by: E. Rietsch: September 12, 2003
% Last updated:
%
%       ier=param_check(structure)
% INPUT
% structure   structure whose parameter fields need to be tested
% OUTPUT
% ier    logical variable; error if ier == true

global S4M

ier=false;

%       Check parameters (if there are any)
if isfield(structure,'parameter_info')
   fields=fieldnames(structure);
   if ~iscell(structure.parameter_info)
      disp(' Field "parameter_info" must be a cell array; other errors may exist.')
      ier=true;
      return
   else
      m=size(structure.parameter_info,2);
      if m ~= 3
         disp(' Field "parameter_info" is not a cell array with 3 columns; other errors may exist.')
         ier=true;
         return
      end
        %       Check for uniqueness of parameter mnemonics
      parfields=structure.parameter_info(:,1);
      if S4M.case_sensitive
         if length(unique(parfields)) ~= length(parfields)
            disp(' Case sensitive parameter mnemonics are not unique:');
            disp(cell2str(parfields,', '))
            ier=true;
            return
         end
      else
         if length(unique(lower(parfields))) ~= length(parfields)
            disp(' Case insensitive parameter mnemonics are not unique:');
            disp(cell2str(parfields,', '))
            ier=true;
            return
         end
      end

      index=ismember(parfields,fields);
      if ~all(index)
         disp(' Field "parameter_info" provides information about the following parameter(s)')
         disp('      that is/are not present in the structure:')
         disp(['      "',cell2str(parfields(~index),'", "'),'"'])
         ier=true;
      end
      
      if ~all(cellfun(@ischar,structure.parameter_info))
         disp(' Not all entries of field "parameter_info" are strings.')
      end
   end  
end
