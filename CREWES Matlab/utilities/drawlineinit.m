function drawlineinit(action,button)
% DRAWLINEINIT: initialize line drawing
%
%  drawlineinit(transfer,button)
%  drawlineinit(transfer)
%  drawlineinit
%
% initialize the current figure and axes for a selectionbox. See help for
% DERAWLINE.
% transfer ... MATLAB string containing an executable command which is 
%		called at the completion of the line drawing
%	********** default = '' ************
% button ... mouse button number to respond to the linedraw. If the
%		user presses a different button than this, no action is taken
%	********** default = 1 ************
%
% G.F. Margrave, CREWES, JUNE 2000
%
% NOTE: This SOFTWARE may be used by any individual or corporation for any purpose
% with the exception of re-selling or re-distributing the SOFTWARE.
% By using this software, you are agreeing to the terms detailed in this software's
% Matlab source file.

% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by
% its 'AUTHOR' (identified above) and the CREWES Project.  The CREWES
% project may be contacted via email at:  crewesinfo@crewes.org
%
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) This SOFTWARE may be used by any individual or corporation for any purpose
%    with the exception of re-selling or re-distributing the SOFTWARE.
%
% 2) The AUTHOR and CREWES must be acknowledged in any resulting publications or
%    presentations
%
% 3) This SOFTWARE is provided "as is" with no warranty of any kind
%    either expressed or implied. CREWES makes no warranties or representation
%    as to its accuracy, completeness, or fitness for any purpose. CREWES
%    is under no obligation to provide support of any kind for this SOFTWARE.
%
% 4) CREWES periodically adds, changes, improves or updates this SOFTWARE without
%    notice. New versions will be made available at www.crewes.org .
%
% 5) Use this SOFTWARE at your own risk.
%
% END TERMS OF USE LICENSE

set(gcf,'windowbuttondownfcn','drawline(''init'')');
if( nargin < 1)
   set(gcf,'windowbuttonupfcn','drawline(''fini'')');
else
   set(gcf,'windowbuttonupfcn',...
	['drawline(''fini'');' action]);
end