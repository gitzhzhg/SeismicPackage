function PI_positionaxes_linemotionend(arg1,arg2,arg3)
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

%global OLDDOWNFCN OLDMOTIONFCN OLDUPFCN
global OLDDOWNFCN OLDUPFCN

% h=get(gcf,'userdata');
% hmsg=h{2};
%hi=h{5};
% hzoompick=h{9};
set(gcf,'windowbuttonmotionfcn',[]);
mainax=findobj(gcf,'type','axes','tag','MAINAXES');
% set(get(mainax,'children'),'erasemode','normal');
posax=findobj(gcf,'type','axes','tag','POSITIONAXES');
lns=get(get(posax,'title'),'userdata');
% set(lns(1),'erasemode','normal');
% set(lns(2),'erasemode','normal');
% set(lns(3),'erasemode','normal');
% set(lns(4),'erasemode','normal');
% set(lns(6),'erasemode','normal','visible','on');
set(lns(6),'visible','on');

set(gcf,'windowbuttondownfcn',OLDDOWNFCN);
set(gcf,'windowbuttonupfcn',OLDUPFCN);

%avoid getting a zoom box in posax KWH, 2014
%set(gcf,'windowbuttonmotionfcn',OLDMOTIONFCN);
set(gcf,'windowbuttonmotionfcn','');


%value=get(hzoompick,'value');
% switch value
%     case 1
%         selboxinit('plotimage(''zoom'')',1);
%         set(gcf,'name','Seismic Image Plot, Simplezooming installed (Use MB1)');
%     case 2
%         drawlineinit('plotimage(''pick'')');
%         set(gcf,'name','Seismic Image Plot, Picking resummed (Use MB1)');
%     case 3
%         drawlineinit('plotimage(''pick'')');
%         set(gcf,'name','Seismic Image Plot, Picking new (Use MB1)');
%         set(hzoompick,'userdata',[]);
%     case 4
% %         udat=get(hi,'userdata');
% %         seisstruct.SEIS=udat{1};
% %         seisstruct.X=udat{2};
% %         seisstruct.T=udat{3};
% %         pref.hmsg=hmsg;
% %         masteraxes=gca;
% %         holdhandle=gca;
% %         picksle('plotimage(''pick'')',seisstruct,masteraxes,pref);
% %         set(gcf,'name','Seismic Image Plot, Picking resummed (Use MB1)');
% %         stringinfo='Automatic Picking has been enabled';
%         
%     
% end

ylim=sort([get(lns(1),'ydata') get(lns(2),'ydata')]);
xlim=sort([get(lns(3),'xdata') get(lns(4),'xdata')]);
set(mainax,'xlim',[xlim(1) xlim(end)],'ylim',[ylim(1) ylim(end)]);
%set(hi,'alphadata',1);
PI_zoomlock;