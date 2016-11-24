function posnfig(x,y,hfig)
% POSNFIG ... positions a figure (default in the middle of the screen)
%
% x ... position of the center of the figure relative to the screen width in
% normalized units
% ********* default =.5 ************
% y ... position of the center of the figure relative to the screen height in
% normalized units
% ********* default =.5 ************
% hfig ... figure handle
% ********* default = gcf *************
%
% example: posnfig; With no inputs, posnfig positions the current figure in
% the center of the screen
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

%

%check for old argument order
if(isgraphics(x))
    tmp=hfig;
    hfig=x;
    x=y;
    y=tmp;
end

if(nargin<2)
    y=.5;
end
if(nargin<1)
    x=.5;
end
if(nargin<3)
    hfig=gcf;
end

%get screensize
screensize=get(0,'screensize');
width=screensize(3);
height=screensize(4);

set(hfig,'units','pixels');
figposn=get(hfig,'position');
figwidth=figposn(3);
figheight=figposn(4);
% figx=figposn(1);
% figy=figposn(2);

newfigx=round(x*width-figwidth/2);
newfigy=round(y*height-figheight/2);

figposn(1)=newfigx;
figposn(2)=newfigy;

set(hfig,'position',figposn);