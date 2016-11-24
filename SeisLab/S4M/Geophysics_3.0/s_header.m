function seismic=s_header(seismic,action,mnem,values,units,description)
% Function manipulates header(s) of a seismic dataset
% DEPRECATED: Use "ds_header" instead.
%
% Written by E. Rietsch; Date March 16, 2000
% Last updated: October 16, 2007: Allow four input arguments if "action" is 'replace'.
%
%           seismic=ds_header(seismic,action,mnem,values,units,description)
%           seismic=ds_header(seismic,action,mnem)    for action=delete,delete_ne,keep,keep_ne,list
%           seismic=ds_header(seismic)                assumes action list          
% INPUT
% seismic  Seismic structure;
%          If this is the only input argument, "action" is set to 'list' and "mnem' is
%          set to '*'. The function prints a list of all header values as describend 
%          below. 
% action   Defines action to take. Possible values are:
%          'add'      Add header with mnemonic mnem. Gives error message if 
%                     header already exists
%          'add_ne'   Add header with mnemonic mnem. Replaces it if it already exists.
%          'replace'  Replaces header with mnemonic mnem; error if
%                     header does not exist
%          'delete'   Delete header(s) with mnemonic(s) mnem; 
%                     error if header does not exist 
%          'delete_ne' Delete header(s) with mnemonic(s) mnem if the header is present
%                     no error if one or more of the header(s) specified does not exist 
%          'keep'     Keep header(s) with mnemonic(s) mnem and deletes all others;
%                     error if one or more of the header(s) do not exist 
%          'keep_ne'  Keep header(s) with mnemonic(s) mnem if it is (they are) present
%                     and deletes all others; no error if any or all headers specified 
%                     are not present
%          'rename'   Rename header mnemonic, keep everything else the same
%          'list'     Print short list: for specified header mnemonic(s) it lists minimum 
%                     and maximum value, smallest and greatest trace-to-trace change,
%                     units of measurement, and header description
%          
%          The other input parameters depend on the parameter "action"
%
%                   CASE action = 'add', 'add_ne', or 'replace'
% mnem     Header mnemonic
% values   Header values; if only one value is given the header is assumed 
%          to be constant
% units    Units of measurement for curve values (optional if 'action' is 
%          'replace')
% description  Description of curve mnemonic (optional if 'action' is 'replace')
%          If "action" is 'replace' "units" and "description" need not be given; 
%          in this case the original "units" and "description" are retained.
%
%                  CASE action = 'delete', delete_ne', 'keep', 'keep_ne', or 'list'
% mnem     Header mnemonic or cells array with header mnemonics
%          '*' means all headers
%
%                  CASE action = 'rename'
% mnem     Cell array consisting of two strings: the old and 
%          the new name of the header
%
% OUTPUT
% seismic  "Updated" seismic structure (no output if action == 'list')
%
% EXAMPLE
%         seismic=s_data;
%         s_header(seismic)

% UPDATE HISTORY
%          January 17, 2006: more checks of input data
%          October 16, 2007: Allow four input arguments if "action" is 'replace'.

% disp('Function "ds_header" is deprecated: Use "ds_header" instead.')

switch nargin
case 1
   seismic=ds_header(seismic);

case 3
   seismic=ds_header(seismic,action,mnem);

case 4
   if strcmpi(action,'replace')
      seismic=ds_header(seismic,action,mnem,values);
   else
      error('Four input arguments are only allowed if input argument "action" is set to ''replace''.');
   end
case 6
   seismic=ds_header(seismic,action,mnem,values,units,description);

otherwise
   error(' Incorrect number of input arguments.')

end

if nargout == 0;
   clear seismic
end
