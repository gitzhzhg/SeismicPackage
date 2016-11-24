function PI_positionaxes_lineposition()
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

mainax=findobj(gcf,'type','axes','tag','MAINAXES');
posax=findobj(gcf,'type','axes','tag','POSITIONAXES');
h=get(get(posax,'title'),'userdata');
% [bottom top left right]
xdat=get(mainax,'xlim');
ydat=get(mainax,'ylim');
if(strcmp(get(posax,'visible'),'on'))
    vis='on';
else
    vis='off';
end
set(h(1),'ydata',[ydat(1) ydat(1)],'xdata',[xdat(1) xdat(2)],'visible',vis);
set(h(2),'ydata',[ydat(2) ydat(2)],'xdata',[xdat(1) xdat(2)],'visible',vis);
set(h(3),'ydata',[ydat(1) ydat(2)],'xdata',[xdat(1) xdat(1)],'visible',vis);
set(h(4),'ydata',[ydat(1) ydat(2)],'xdata',[xdat(2) xdat(2)],'visible',vis);
set(h(6),'ydata',[ydat(1) ydat(2) ydat(2) ydat(1)],'xdata',[xdat(2) xdat(2) xdat(1) xdat(1)],'visible',vis);