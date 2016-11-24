function PI_limmoveend()
% this routine occures at the end of moving the limit lines.  It ensure
% that the limit box parts are reset to the original order.  This section
% also forces the limit lines to be 5% away from the edge of the image
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

% data.

h=get(gcf,'userdata');
hi=h{5}; % Image

hlimbox=h{14};
limdat=get(hlimbox,'userdata');
limptdat=limdat{1};

set(limptdat,'visible','on');
limlndat=limdat{2};
limcent=limdat{4};

set(gcf,'windowbuttonupfcn',[]);
set(gcf,'windowbuttonmotionfcn',[]);
selboxinit('plotimage(''zoom'')',1);

% need to stop axis from resizing
parentaxis=get(limptdat(1),'parent');
xdat=get(parentaxis,'xlim');
ydat=get(parentaxis,'ylim');
% [top bottom left right]
ximdat=get(hi,'xdata');
yimdat=get(hi,'ydata');
tpnum=get(limlndat(1),'ydata');
btnum=get(limlndat(2),'ydata');
dy=sort([ydat(1) ydat(2)]);
dy=(dy(2)-dy(1))/120;
hornum=sort([yimdat(1)+dy tpnum(1) btnum(2) yimdat(end)-dy]);
hornum=[hornum(2) hornum(3)];
lfnum=get(limlndat(3),'xdata');
rtnum=get(limlndat(4),'xdata');
dx=sort([xdat(1) xdat(2)]);
dx=(dx(2)-dx(1))/120;
vernum=sort([ximdat(1)+dx lfnum(1) rtnum(1) ximdat(end)-dx]);
vernum=[vernum(2) vernum(3)];

%update userdata in hlimbox
mdat = [hornum vernum];
limdat{3} = mdat;
set(hlimbox,'userdata',limdat);

% setting lines back
set(limlndat(1),'ydata',[mdat(2) mdat(2)],'xdata',[mdat(3) mdat(4)]);
set(limlndat(2),'ydata',[mdat(1) mdat(1)],'xdata',[mdat(3) mdat(4)]);
set(limlndat(3),'ydata',[mdat(1) mdat(2)],'xdata',[mdat(3) mdat(3)]);
set(limlndat(4),'ydata',[mdat(1) mdat(2)],'xdata',[mdat(4) mdat(4)]);
% setting points back
set(limptdat(1),'ydata',mdat(2),'xdata',mdat(3));
set(limptdat(2),'ydata',mdat(2),'xdata',mdat(4));
set(limptdat(3),'ydata',mdat(1),'xdata',mdat(3));
set(limptdat(4),'ydata',mdat(1),'xdata',mdat(4));
set(parentaxis,'ylim',ydat,'xlim',xdat);
% setting center line back (if it needs to go back)
set(limcent,'ydata',(mdat(2)-mdat(1))/2+mdat(1),'xdata',(mdat(4)-mdat(3))/2+mdat(3));

hgcf=gcf;
plotimage('limboxrescale');
set(0,'currentfigure',hgcf);
h=get(gcf,'userdata');
hzoompick=h{9};
value=get(hzoompick,'value');
delete(findobj(gcf,'type','line','tag','PICKMARKER'));
delete(findobj(gcf,'type','text','tag','PICKTEXT'));
switch value
case 1
    selboxinit('plotimage(''zoom'')',1);
    set(gcf,'name','Seismic Image Plot, Simplezooming installed (Use MB1)');
case 2
    drawlineinit('plotimage(''pick'')');
    set(gcf,'name','Seismic Image Plot, Picking resummed (Use MB1)');
case 3
    drawlineinit('plotimage(''pick'')');
    set(gcf,'name','Seismic Image Plot, Picking new (Use MB1)');
    set(hzoompick,'userdata',[]);
end