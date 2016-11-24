function PI_zoom
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

%global ZOOM_LOCKS
if(strcmp(get(gca,'visible'),'on'))
    set(gcf,'pointer','arrow');
    h=get(gcf,'userdata');
    hi=h{5};
    %haxs=gca;
%     hvertscrol=h{16};
%     hhorscrol=h{17};
    % making sure that user is trying to zoom and not clicking on
    % on the limit box lines of points
    box=selboxfini;
    if(isempty(box)), return; end
    try
        delete(box{2});
    catch
        %selbox rectangle already deleted
    end
    box = box{1}; %strip off cell array containing handle to selbox rectangle
    if(length(box)<4) return; end
    

    xmin=min([box(1) box(3)]);
    xmax=max([box(1) box(3)]);
    ymin=min([box(2) box(4)]);
    ymax=max([box(2) box(4)]);
    %get the current axis settings
    xlim=get(gca,'xlim');
    ylim=get(gca,'ylim');
    test1=xmin-xlim(1)+xmax-xlim(2)+ymin-ylim(1)+ymax-ylim(2);
    test2=(xmin-xmax)*(ymin-ymax);
    if(abs(test1)<10*eps||abs(test2)< 10*eps||strcmp(get(gcf,'selectiontype'),'extend'))
        %this is unzooming
        xdat=get(hi,'xdata');
        ydat=get(hi,'ydata');
        xdat=[min(xdat) max(xdat)];
        ydat=[min(ydat) max(ydat)];
        set(gca,'xlim',xdat,'ylim',ydat);
%         set(hhorscrol,'visible','off');
%         set(hvertscrol,'visible','off');
        posax=findobj(gcf,'type','axes','tag','POSITIONAXES');
        lns=get(get(posax,'title'),'userdata');
        set(lns(1),'visible','off');
        set(lns(2),'visible','off');
        set(lns(3),'visible','off');
        set(lns(4),'visible','off');
        set(lns(6),'visible','off');
    else
        imxdat=get(hi,'xdata');
        imydat=get(hi,'ydata');
        xdat=sort([imxdat(1) xmin xmax imxdat(end)]);
        ydat=sort([imydat(1) ymin ymax imydat(end)]);
        xdat=[xdat(2) xdat(3)];
        ydat=[ydat(2) ydat(3)];
        set(gca,'xlim',xdat,'ylim',ydat);
        %NOTE: Margrave (April 2016) disabled the scroll bars that appear
        %when zooming. They seem to cause more problem than not and are not
        %really necessary because the position axis can be used or you can
        %just unzoom and rezoom.
%         ximlim=sort(get(hi,'xdata'));
%         yimlim=sort(get(hi,'ydata'));
%         xdat2=sort(get(gca,'xlim'));
%         ydat2=sort(get(gca,'ylim'));
%         set(hhorscrol,'value',(xdat2(2)-xdat2(1))/2+xdat2(1),'visible','on','enable','on',...
%             'userdata',{2 (xdat2(2)-xdat2(1))/2+xdat2(1) []},...
%             'max',ximlim(end),'min',ximlim(1));
%         y1=yimlim(end)-(ydat2(1)-yimlim(1));
%         y2=yimlim(1)+(yimlim(end)-ydat2(2));
%         set(hvertscrol,'value',y2+(y1-y2)/2,'visible','on','userdata',{1 y2+(y1-y2)/2 []},...
%             'enable','on','max',yimlim(end),'min',yimlim(1));
        PI_positionaxes_lineposition;
    end
    PI_zoomlock;   
else
end
% set(findobj(gcf,'type','line','tag','LIMITLINE'),'erasemode','normal');
% set(findobj(gcf,'type','line','tag','LIMITPOINT'),'erasemode','normal');