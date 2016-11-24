% to use this, save it to your Matlab directory and rename it to startup.m
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

global SCALE_OPT GRAY_PCT NUMBER_OF_COLORS 
global CLIP COLOR_MAP NOBRIGHTEN NOSIG
global BIGFIG_X BIGFIG_Y BIGFIG_WIDTH BIGFIG_HEIGHT
%set parameters for plotimage
SCALE_OPT=2;
GRAY_PCT=20;
NUMBER_OF_COLORS=64;
CLIP=4;
COLOR_MAP='seisclrs';
NOBRIGHTEN=1;
NOSIG=1;
% set parameters for bigfig (used by prepfig)
% try to make the enlarged figure size 1100 pixels wide and 700 pixels high
scr=get(0,'screensize');
BIGFIG_X=1;
BIGFIG_Y=30;
if(scr(3)>1100)
    BIGFIG_WIDTH=1100;
else
    BIGFIG_WIDTH=scr(3)-BIGFIG_X;
end
if(scr(4)>730)
    BIGFIG_HEIGHT=700;
else
    BIGFIG_HEIGHT=scr(4)-BIGFIG_Y;
end
%
% By default, your working directory will be documents\matlab (under
% Windows) You startup.m file should reside in this directory. 
% If you wish to always begin working in another directory, for example
% \documents\matlab\work, then uncomment the following line
% cd work