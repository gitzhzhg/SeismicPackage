function myerror(message)
% Generalization of Matlabs error message for compiled code
% S4M.deployed == 0   same as standard error message
% S4M.deployed == 1   Error message is displayed in dialog window and global
% variable ABORTED is set to true.
%
% Written by: E. Rietsch: January 25, 2004
% Last updated: May 5, 2005:

%           myerror(message)
% INPUT
% message   string with error message to display

global S4M ABORTED

if S4M.deployed 
   disp(message)
   myerrordlg(message)
   ABORTED=true;

else
   error(message)
end
