function handle=msgdlg(message,CreateStruct)
% Open message box "msgbox" with name "S4M.name"
%
% Written by: E. Rietsch: October 23, 2003
% Last updated: January 4, 2004: Second input argument
%
%              handle=msgdlg(message,CreateStruct)
% INPUT
% message      message to display
% CreateStruct structure to control display; optional; type "help msgbox" for details
%              Default:  CreateStruct.Interpreter='none';
%                        CreateStruct.WindowStyle='modal'
% OUTPUT
% handle       handle of message box


global S4M

if nargin == 1
  CreateStruct.Interpreter='none';
  CreateStruct.WindowStyle='modal';

else
  if ~isfield(CreateStruct,'Interpreter')
     CreateStruct.Interpreter='none';
  end
  if ~isfield(CreateStruct,'WindowStyle')
     CreateStruct.WindowStyle='modal';
  end
end

handle=msgbox(message,[S4M.name,' message']);

drawnow
try
   figure(handle)
catch
   disp(['Message box with message "',message,'" has already been deleted.'])
end

%uiwait(handle)
if nargout == 0
   clear handle
end
