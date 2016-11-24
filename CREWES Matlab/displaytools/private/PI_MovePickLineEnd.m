function PI_MovePickLineEnd()
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

global PICKS 
h=get(gcf,'userdata');
hmsg=h{2};
% set(findobj(gcf,'type','line','tag','PICKS'),'erasemode','normal');
set(gcf,'windowbuttonmotionfcn','','windowbuttonupfcn','');
hobj=gco;
nm=get(hobj,'tag');
udat=get(hobj,'userdata');
switch nm
    case 'PICKMARKER'
%         set(hobj,'erasemode','normal');
%         set(udat{1},'erasemode','normal');
        xdat=get(udat{1},'xdata');
        ydat=get(udat{1},'ydata');
        hpick=udat{1};
    case 'PICKS'
        xdat=get(hobj,'xdata');
        ydat=get(hobj,'ydata');
%         set(udat(1),'erasemode','normal','ydata',ydat(1),'xdata',xdat(1),...
%             'visible','on');
%         set(udat(2),'erasemode','normal','ydata',ydat(2),'xdata',xdat(2),...
%             'visible','on');
        set(udat(1),'ydata',ydat(1),'xdata',xdat(1),...
            'visible','on');
        set(udat(2),'ydata',ydat(2),'xdata',xdat(2),...
            'visible','on');        
        hpick=hobj;
end
if(~isempty(PICKS))
    for ii=1:size(PICKS,1)
        CheckFigure=PICKS{ii,1};
        if(CheckFigure==gcf)
            CheckHandles=PICKS{ii,3};
            for jj=1:length(CheckHandles)
                if(CheckHandles(jj)==hpick)
                    LinePositions=PICKS{ii,2};
                    LinePositions(jj,:)=[xdat(1) ydat(1) xdat(2) ydat(2)];
                    PICKS{ii,2}=LinePositions;
                end
            end
        end
    end
end
% set(findobj(gcf,'type','line','tag','LIMITLINE'),'erasemode','normal');
% set(findobj(gcf,'type','line','tag','LIMITPOINT'),'erasemode','normal');
% set(findobj(gcf,'type','text','tag','PICKTEXT'),'erasemode','normal','visible','off');
set(findobj(gcf,'type','text','tag','PICKTEXT'),'visible','off');
set(gcf,'windowbuttonmotionfcn','','windowbuttonupfcn','');
stringinfo='Pick line has been moved';
set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);