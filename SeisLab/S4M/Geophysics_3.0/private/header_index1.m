function [index,ier]=header_index1(seismic,mnem,abort)
% Function outputs index of header in seismic structure "seismic".
% If no header mnemonic is found matching "mnem" then "index" is []
%
%         [index,ier]=header_index1(seismic,mnem)
% INPUT
% seismic  seismic structure whose headers are requested
% mnem   header mnemonics
% abort  optional parameter indicating if function should terminate abnormally
%        if no or two or more mnemonics are found
%        abort = 0 ==> do not terminate abnormally (default for two output arguments)
%        abort = 1 ==> terminate abnormally  (default for one output argument)
% OUTPUT
% index  index of header with mnemonic "mnem"
% ier    error indicator; If only one output argument is given, the function will abort
%        with an error message if no header or more than one header is found
%        Otherwise: ier=0 if no error was detected and ier=1 if an error was found     

global S4M

if nargin < 3 
   if nargout == 1
      abort=1;
   else
      abort=0;
   end
end

if S4M.case_sensitive
   index=find(ismember(seismic.header_info(:,1),mnem));
else
   index=find(ismember(lower(seismic.header_info(:,1)),lower(mnem)));
end

%       Check for errors
if nargout < 2 
   if isempty(index)  &&  abort ~= 0    % Print error message
      disp([' Header with mnemonic  "',mnem,'" not found'])
      disp(' The following header mnemonics exist: ')
      disp(seismic.header_info(:,1)');
      error(' Abnormal termination')

   elseif length(index) > 1  &&  abort ~= 0    % Print error message
      disp([' More than one header with mnemonic "',mnem,'" found.'])
      disp(' The following header mnemonics exist: ')
      disp(seismic.header_info(:,1)');
      error(' Abnormal termination')

   else
     return
   end

else
   if isempty(index) || length(index) > 1
      ier=1;
   else
      ier=0;
   end
end

