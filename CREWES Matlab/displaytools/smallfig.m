function smallfig(figno,figsize)
%SMALLFIG shrinks a figure to the original size
% smallfig(figno)
%
% figno is the handle of the figure to be returned to original size
% 
%
% SMALLFIG requires that both arguments be passed to the function and that
% figsize is in normalized units or pixels
%
%figsize=[lowerleftx, lowerlefty, width, height];
%
%If the function is being used in a button call back the figsize can be
%stored in the userdata of the handle for the button, then accessed by
%retriving it in the callback function as illustrated below
%
%uimenu('parent',menus, 'Label','Small Figure','Userdata',figsize,...
%           'Callback','smallfig(gcf,get(gcbo,''Userdata''))');
%
%
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

% H.J.E Lloyd,
scrsz = get(0,'ScreenSize');
figunits=get(figno,'Units');

if figsize(4)<=1
    sz=[figsize(1)*scrsz(3),figsize(2)*scrsz(4),figsize(3)*scrsz(3),figsize(4)*scrsz(4)];
else
    sz=figsize;
end

set(figno,'units','pixels','position',sz);
set(figno,'units',figunits);