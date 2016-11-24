function PI_MovePickLineMotion()
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

newpt=get(gca,'currentpoint');
xdat=get(gca,'xlim');
ydat=get(gca,'ylim');
nm=get(gco,'tag');
switch nm
    case 'PICKMARKER'
        xdat=sort([xdat(1) newpt(1,1) xdat(2)]);
        ydat=sort([ydat(1) newpt(1,2) ydat(2)]);
        pickdat=get(gco,'userdata');
        pxdat=get(pickdat{1},'xdata');
        pydat=get(pickdat{1},'ydata');
        if(pickdat{2}==2)
            pxdat=[pxdat(1) xdat(2)];
            pydat=[pydat(1) ydat(2)];
        else
            pxdat=[xdat(2) pxdat(2)];
            pydat=[ydat(2) pydat(2)];
        end
        %set(pickdat{1},'xdat',pxdat,'ydat',pydat,'erasemode','xor');
        %set(gco,'ydata',[ydat(2)],'xdata',[xdat(2)],'erasemode','xor');
        %set(findobj(gcf,'type','line','tag','LIMITLINE'),'erasemode','xor');
        %set(findobj(gcf,'type','line','tag','LIMITPOINT'),'erasemode','xor');
        set(pickdat{1},'xdat',pxdat,'ydat',pydat);
        set(gco,'ydata',ydat(2),'xdata',xdat(2));        
        udat=get(pickdat{1},'userdata');
        textstr=char(['X: ' num2str(pxdat(1)) ' ' num2str(pxdat(2))],['Y: ' num2str(pydat(1)) ' ' num2str(pydat(2))]);
%         set(udat(3),'string',textstr,'visible','on','erasemode','xor',...
%             'position',[xdat(2) ydat(2)]);
        set(udat(3),'string',textstr,'visible','on',...
            'position',[xdat(2) ydat(2)]);
    case 'PICKS'
        h=get(gcf,'userdata');
        hi=h{5};
        xlim=get(hi,'xdata');
        ylim=get(hi,'ydata');
        oldpt=get(gca,'userdata');
        dx=newpt(1,1)-oldpt(1,1);
        dy=newpt(1,2)-oldpt(1,2);
        udat=get(gco,'userdata');
        set(udat(1),'visible','off');
        set(udat(2),'visible','off');
        xpt=get(gco,'xdata');
        ypt=get(gco,'ydata');
        newxdat=[xpt(1)+dx xpt(2)+dx];
        newydat=[ypt(1)+dy ypt(2)+dy];
        if(newxdat(1)<=xlim(1)||newxdat(1)>=xlim(end)||newxdat(2)<=xlim(1)||newxdat(2)>=xlim(end))
            newxdat(1)=xpt(1);
            newxdat(2)=xpt(2);
        end
        if(newydat(1)<=ylim(1)||newydat(1)>=ylim(end)||newydat(2)<=ylim(1)||newydat(2)>=ylim(end))
            newydat(1)=ypt(1);
            newydat(2)=ypt(2);
        end
        set(gco,'xdata',[newxdat(1) newxdat(2)],'ydata',[newydat(1) newydat(2)]);
        set(gca,'userdata',newpt);        
end