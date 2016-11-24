function PI_zoomscrollmotion(action)
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

%global SCALE_OPT NUMBER_OF_COLORS GRAY_PCT CLIP COLOR_MAP NOBRIGHTEN PICKS PICKCOLOR XAXISTOP ZOOM_VALUE ZOOM_LOCKS
h=get(gcf,'userdata');
hi=h{5};
ha=gca;
oldpt=get(findobj(gcf,'type','axes','tag','MAINAXES'),'userdata');
newpt=get(gca,'currentpoint');
dxpt=newpt(1,1)-oldpt(1);
dypt=newpt(1,2)-oldpt(2);
imxdat=get(hi,'xdata');
imydat=get(hi,'ydata');
axdat=get(ha,'xlim');
aydat=get(ha,'ylim');
if(strcmp(action,'zoomscrollmotion'))
    newxdat=axdat+dxpt;
    newydat=aydat+dypt;
    if(newydat(1)<=imydat(1)||newydat(end)>=imydat(end))
        newydat=aydat;  % limits motion to image, y data
    end
    if(newxdat(1)<=imxdat(1)||newxdat(end)>=imxdat(end))
        newxdat=axdat;  % limits motion to image, x data
    end
elseif(strcmp(action,'zoominoutmotion'))
    dxdat=(axdat(end)-axdat(1))*.02;
    dydat=(aydat(end)-aydat(1))*.02;
    xdat=[axdat(1)-dxdat axdat(end)+dxdat axdat(1)+dxdat axdat(end)-dxdat];
    ydat=[aydat(1)-dydat aydat(end)+dydat aydat(1)+dydat aydat(end)-dydat];
    if(dypt>=0)
        % enlarging box
        nx1=sort([imxdat(end) xdat(2)]);
        nx2=sort([imxdat(1) xdat(1)]);
        ny1=sort([imydat(1) ydat(1)]);
        ny2=sort([imydat(end) ydat(2)]);
    elseif(dypt<=0)
        % shrinking zoom box
        nx1=sort([imxdat(end) xdat(3)]);
        nx2=sort([imxdat(1) xdat(4)]);
        ny1=sort([imydat(1) ydat(3)]);
        ny2=sort([imydat(end) ydat(4)]);
    end
    newxdat=sort([nx1(1) nx2(2)]);
    newydat=sort([ny1(2) ny2(1)]);
end
axspts=get(gca,'currentpoint');
figpts=get(gcf,'currentpoint');
set(gca,'userdata',[axspts(1,1) axspts(1,2) figpts(2)],...
    'ylim',newydat','xlim',newxdat);
PI_positionaxes_lineposition;
PI_zoomlock;