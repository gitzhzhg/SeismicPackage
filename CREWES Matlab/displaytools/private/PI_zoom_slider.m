function PI_zoom_slider()
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

haxs=findobj(gcf,'type','axes','tag','MAINAXES');
h=get(gcf,'userdata');
hi=h{5};
hvertscrol=h{16};
hhorscrol=h{17};

xdat=sort(get(hi,'xdata'));
ydat=sort(get(hi,'ydata'));

xlim=get(haxs,'xlim');
ylim=get(haxs,'ylim');

verval=get(hvertscrol,'value');
horval=get(hhorscrol,'value');
udat=get(gcbo,'userdata');
ckslide=udat{1};
switch ckslide
    case 1  % Vertical Slider
        dy=verval-udat{2};
        newylim=ylim-dy;
        if(newylim(1)<=ydat(1))
            dlim=ylim(2)-ylim(1);
            newylim=[ydat(1) ydat(1)+dlim];
            y1=ydat(end)-(newylim(1)-ydat(1));
            y2=ydat(1)+(ydat(end)-newylim(2));
            udat{2}=y2+(y1-y2)/2;
        elseif(newylim(2)>=ydat(end))
            dlim=ylim(2)-ylim(1);
            newylim=[ydat(end)-dlim ydat(end)];
            y1=ydat(end)-(newylim(1)-ydat(1));
            y2=ydat(1)+(ydat(end)-newylim(2));
            udat{2}=y2+(y1-y2)/2;
        else
            udat{2}=udat{2}+dy;
        end
        set(haxs,'ylim',newylim);
        set(hvertscrol,'value',udat{2},'userdata',udat);
    case 2  % Horizontal Slider
        dx=horval-udat{2};
        newxlim=xlim+dx;
        if(newxlim(1)<=xdat(1))
            dlim=xlim(2)-xlim(1);
            newxlim=[xdat(1) xdat(1)+dlim];
        elseif(newxlim(2)>=xdat(end))
            dlim=xlim(2)-xlim(1);
            newxlim=[xdat(end)-dlim xdat(end)];
        else        
        end
        udat{2}=(newxlim(2)-newxlim(1))/2+newxlim(1);
        set(haxs,'xlim',newxlim);
        set(hhorscrol,'value',udat{2},'userdata',udat);
end
PI_positionaxes_lineposition;
% global ZOOM_LOCKS
% xdat=get(gca,'xlim');
% ydat=get(gca,'ylim');
PI_zoomlock;