function PI_limcentmove2(arg1,arg2,arg3)
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

axes1=gca;
h=get(gcf,'userdata');
hi=h{5};

xlimits=get(hi,'xdata');
xlimits=sort([xlimits(1) xlimits(end)]);
ylimits=get(hi,'ydata');
ylimits=sort([ylimits(1) ylimits(end)]);

hlimbox=h{14};
limdata=get(hlimbox,'userdata');
limlnpositions=limdata{2};

% [top bottom left side right side]
lntop=limlnpositions(1);
ytop=get(lntop,'ydata'); ytop=ytop(2);
lnbot=limlnpositions(2);
ybot=get(lnbot,'ydata'); ybot=ybot(1);
lnlft=limlnpositions(3);
xlft=get(lnlft,'xdata'); xlft=xlft(1);
lnrgt=limlnpositions(4);
xrgt=get(lnrgt,'xdata'); xrgt=xrgt(2);
% [ytop ybot xlft xrgt];
limptpositions=limdata{1};
% this is cheating, but I don't want to move the points here, so I will
% just make their visibility 'off'... hehehe... tricky me
set(limptpositions,'visible','off');
% [upper left upper right lower left lower right]
limcent=limdata{4};
 
opt=get(axes1,'userdata');
xdat=get(axes1,'xlim');
ydat=get(axes1,'ylim');
cpt=get(axes1,'currentpoint');
newxpt=sort([xdat(1) cpt(1,1) xdat(2)]);
newypt=sort([ydat(1) cpt(1,2) ydat(2)]);
newpt=[newxpt(2) newypt(2)];    % limits for instant axes limits

% this will not let the cent out of the axis
set(limcent,'xdata',newpt(1),'ydata',newpt(2));

dx=opt(1,1)-cpt(1,1);
dy=opt(1,2)-cpt(1,2);
set(axes1,'userdata',cpt);
newxdat=sort([xlft-dx xrgt-dx]);
newydat=sort([ybot-dy ytop-dy]);

if(newxdat(1)<=xlimits(1)||newxdat(2)>=xlimits(2))
    newxdat=[xlft xrgt];
end
if(newydat(1)<=ylimits(1)||newydat(2)>=ylimits(2))
    newydat=[ybot ytop];
end
mdat=[newydat newxdat];
set(lntop,'xdata',[mdat(3) mdat(4)],'ydata',[mdat(1) mdat(1)]);
set(lnbot,'xdata',[mdat(3) mdat(4)],'ydata',[mdat(2) mdat(2)]);
set(lnlft,'xdata',[mdat(3) mdat(3)],'ydata',[mdat(2) mdat(1)]);
set(lnrgt,'xdata',[mdat(4) mdat(4)],'ydata',[mdat(2) mdat(1)]);