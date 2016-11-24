function myerrordlg(message,CreateStruct)
% Open message box "errordlg" with name "S4M.name"
%
% Written by: E. Rietsch: January 25, 2004
% Last updated:
%
%              myerrordlg(message,CreateStruct)
% INPUT
% message      message to display
% CreateStruct structure to contro display; optional; type "help msgbox" for details
%              Default:  CreateStruct.Interpreter='none';
%                        CreateStruct.WindowStyle='modal'

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

uiwait(errordlg(message,S4M.name))
