function [sonout,picks]=stretchwell(trcs,t,son,rho,z,wlet,tw)
% STRETCHWELL is a gui interface that will allow the user to alter a sonic
%   log to match seismic data.
%
% [sonout,picks]=stretchwell(trcs,t,son,rho,z,wlet,tw)
%
% Variables In
%  trcs - 5 traces from the seismic centered at the well location
%  t    - time vector that matches seismic
%  son  - sonic log in depth
%  rho  - density log in depth
%  z    - depth vector that matches sonic and density
%  wlet - wavelet
%  tw   - time vector for wavelet.
% Variables out
% sonout - new sonic log;
% picks  - Time picks [top well seis]
%
% H.J.E. Lloyd, December 2012
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
if nargin < 7
    errordlg('All Variables must be present');
end


%% initiate gui
h.Ffig=figure('units','pixels','position',[50 50 1000 700],'name','Stretch Sonic Logs','NumberTitle','off','CloseRequestFcn',@close,'menubar','none');
set(h.Ffig,'DefaultaxesFontsize',18,'DefaultuicontrolFontsize',16,'DefaultuipanelFontsize',16,'units','normalized','position',[0 0 .82 1]);
h.Amaxis=subplot('position',[.60 .1 .35 .8]);
h.Aoaxis=subplot('position',[.1 .55 .4 .35]);
h.Rpson=uicontrol('units','normalized','position',[.1 .925 .15 .03],'string','Sonic Logs','style','radio','callback',@pview,'value',1,'BackgroundColor',[0.8 0.8 0.8]);
h.Rtzcv=uicontrol('units','normalized','position',[.25 .925 .15 .03],'string','Time Depth Curves','style','radio','callback',@pview,'BackgroundColor',[0.8 0.8 0.8]);
h.Btpic=uicontrol('units','normalized','position',[.1 .40 .2 .05],'string','Top Pick','style','pushbutton','callback',@pick,'userdata','tpick');
h.Bwpic=uicontrol('units','normalized','position',[.1 .35 .2 .05],'string','Well Pick','style','pushbutton','callback',@pick,'userdata','wpick');
h.Bspic=uicontrol('units','normalized','position',[.1 .30 .2 .05],'string','Trace Pick','style','pushbutton','callback',@pick,'userdata','spick');
h.Ttpic=uicontrol('units','normalized','position',[.3 .40 .2 .05],'string','--','style','text');
h.Twpic=uicontrol('units','normalized','position',[.3 .35 .2 .05],'string','--','style','text');
h.Tspic=uicontrol('units','normalized','position',[.3 .30 .2 .05],'string','--','style','text');
h.Bstch=uicontrol('units','normalized','position',[.1 .20 .13 .05],'string','Stretch','style','pushbutton','callback',@stretch);
h.Bulst=uicontrol('units','normalized','position',[.23 .20 .14 .05],'string','Undo Last Stretch','style','pushbutton','callback',@undostretch);
h.Brset=uicontrol('units','normalized','position',[.37 .20 .13 .05],'string','Reset','style','pushbutton','callback',@reset);
h.Bfin=uicontrol('units','normalized','position',[.2 .10 .2 .05],'string','Finished','style','pushbutton','callback',@close);
h.Vtrcs=trcs./max(trcs(:));
h.Vt=t(:);
h.Vson=son(:);
h.Voson=son(:);
h.Vlson=son(:);
h.Vrho=rho(:);
[tz,zt,vins]=sonic2tz(son,z,-10000);
h.Vtzo=[tz(:),zt(:)];
h.Vtz=[tz(:),zt(:)];
h.Vz=z(:);
h.Vwlet=wlet(:);
h.Vtw=tw(:);
h.Vsyn=nan;
h.Vtpic=nan;
h.Vwpic=nan;
h.Vspic=nan;
h.Vmpwr=nan;
h.Vopwr=h.Vz(find(cumsum(imp2rcs(h.Vson).^2)>.99*max(cumsum(imp2rcs(h.Vson).^2)),1));
h.picks=[];
h.Fwtva=0;
h.Fsontz=1;

guidata(h.Ffig,h);
init(h.Ffig,[]);
uiwait(h.Ffig);
h=guidata(h.Ffig);
sonout=h.Vson;
picks=h.picks;
delete(h.Ffig)
end

function init(hobj,u)
h=guidata(hobj);
[sgram,tlog,rcs,pm,p]=seismo(h.Vson,h.Vrho,h.Vz,0,0,h.Vwlet,h.Vtw);
h.Vsyn=interp1(tlog,sgram,h.Vt);
h.Vsyn(isnan(h.Vsyn))=0;
h.Vsyn=h.Vsyn./max(h.Vsyn);
h.Vmpwr=h.Vt(find(cumsum(h.Vsyn.^2)>.99*max(cumsum(h.Vsyn.^2)),1));
set(h.Ffig,'currentaxes',h.Amaxis);
for k=1:5
    if k==3
        wtva(h.Vtrcs(:,k)+k,h.Vt,[.5 0 0],h.Fwtva+k);
        wtva(h.Vsyn+k+5,h.Vt,[0 0 .5],h.Fwtva+k+5);
    else
        wtva(h.Vtrcs(:,k)+k,h.Vt,'r',h.Fwtva+k);
        wtva(h.Vsyn+k+5,h.Vt,'b',h.Fwtva+k+5);
    end
end
ylabel('Time (s)');
l=legend('Seismic','','Synthetic','','orientation','horizontal');
set(l,'units','normalized','position',[0.60 0.015 0.35 0.04]);
flipy;
set(gca,'ylim',[0 h.Vmpwr],'xlim',[0 11]);
set(h.Ffig,'currentaxes',h.Aoaxis);
plot(h.Vz,h.Voson,h.Vz,h.Vson,'r');
set(gca,'xlim',[0 h.Vopwr]);
legend('Original Sonic','Modified Sonic','location','northeast');
xlabel('Depth (m)');
ylabel('Slowness');
guidata(hobj,h);
end

function pview(hobj,u)
h=guidata(hobj);
if hobj == h.Rpson
    set(h.Rpson,'value',1);
    set(h.Rtzcv,'value',0);
    h.Fsontz=1;
elseif hobj == h.Rtzcv
    set(h.Rpson,'value',0);
    set(h.Rtzcv,'value',1);
    h.Fsontz=0;
end


if h.Fsontz
set(h.Ffig,'currentaxes',h.Aoaxis);
plot(h.Vz,h.Voson,h.Vz,h.Vson,'r');
set(gca,'xlim',[0 h.Vopwr]);
legend('Original Sonic','Modified Sonic','location','northeast');
xlabel('Depth (m)');
ylabel('Slowness')

else
set(h.Ffig,'currentaxes',h.Aoaxis);
plot(h.Vtzo(:,2),h.Vtzo(:,1),h.Vtz(:,2),h.Vtz(:,1),'r');
set(gca,'xlim',[0 h.Vopwr]);
legend('Original Time-Depth Curve','Modified Time-Depth Curve','location','northeast');
xlabel('Depth (m)');
ylabel('Time (s)');
end

guidata(hobj,h);
end

function pick(hobj,u)
h=guidata(hobj);
if strcmp(get(hobj,'userdata'),'tpick')
    set(h.Btpic,'string','Finished Pick','callback',@fpick);
    set(h.Bwpic,'enable','off');
    set(h.Bspic,'enable','off');
    set(h.Amaxis,'userdata',1);
end
if strcmp(get(hobj,'userdata'),'wpick')
    set(h.Bwpic,'string','Finished Pick','callback',@fpick);
    set(h.Btpic,'enable','off');
    set(h.Bspic,'enable','off');
    set(h.Amaxis,'userdata',2);
end
if strcmp(get(hobj,'userdata'),'spick')
    set(h.Bspic,'string','Finished Pick','callback',@fpick);
    set(h.Bwpic,'enable','off');
    set(h.Btpic,'enable','off');
    set(h.Amaxis,'userdata',3);
end
set(h.Ffig,'windowbuttondownfcn',@ppick,'pointer','fullcross');
guidata(hobj,h);
end

function ppick(hobj,u)
h=guidata(hobj);
if h.Amaxis==get(h.Ffig,'currentaxes')
    pp=get(h.Amaxis,'currentpoint');
    if get(h.Amaxis,'userdata')==1;
        pic=pp(1,2);
        h.Vtpic=h.Vt(near(h.Vt,pic));
        set(h.Ttpic,'string',num2str(h.Vtpic));
    end
    if get(h.Amaxis,'userdata')==2;
        pic=pp(1,2);
        h.Vwpic=h.Vt(near(h.Vt,pic));
        set(h.Twpic,'string',num2str(h.Vwpic));
    end
    if get(h.Amaxis,'userdata')==3;
        pic=pp(1,2);
        h.Vspic=h.Vt(near(h.Vt,pic));
        set(h.Tspic,'string',num2str(h.Vspic));
    end
    cla;
    hold on;
    for k=1:5
        if k==3
        wtva(h.Vtrcs(:,k)+k,h.Vt,[.5 0 0],h.Fwtva+k);
        wtva(h.Vsyn+k+5,h.Vt,[0 0 .5],h.Fwtva+k+5);
    else
        wtva(h.Vtrcs(:,k)+k,h.Vt,'r',h.Fwtva+k);
        wtva(h.Vsyn+k+5,h.Vt,'b',h.Fwtva+k+5);
    end
    end
    plot([0 11],[h.Vtpic h.Vtpic],'k:','linewidth',3);
    plot([7  9],[h.Vwpic h.Vwpic],'k:','linewidth',3);
    plot([2  4],[h.Vspic h.Vspic],'k:','linewidth',3);
    
    ylabel('Time (s)');
    l=legend('Seismic','','Synthetic','','orientation','horizontal');
    set(l,'units','normalized','position',[0.60 0.015 0.35 0.04]);
    %flipy;
    set(gca,'ylim',[0 h.Vmpwr],'xlim',[0 11]);
end
guidata(hobj,h);
end

function fpick(hobj,u)
h=guidata(hobj);
if strcmp(get(hobj,'userdata'),'tpick')
    set(h.Btpic,'string','Top Pick','callback',@pick);
    set(h.Bwpic,'enable','on');
    set(h.Bspic,'enable','on');
end
if strcmp(get(hobj,'userdata'),'wpick')
    set(h.Bwpic,'string','Well Pick','callback',@pick);
    set(h.Btpic,'enable','on');
    set(h.Bspic,'enable','on');
end
if strcmp(get(hobj,'userdata'),'spick')
    set(h.Bspic,'string','Trace Pick','callback',@pick);
    set(h.Bwpic,'enable','on');
    set(h.Btpic,'enable','on');
end
set(h.Amaxis,'userdata',[]);
set(h.Ffig,'windowbuttondownfcn',[],'pointer','arrow');
guidata(hobj,h);
end

function stretch(hobj,u)
h=guidata(hobj);
if any(isnan([h.Vtpic,h.Vwpic,h.Vspic]))
    errordlg('Picks must be made for the Top, Well and Seismic')
    return;
end

if any(h.Vtpic>[h.Vwpic,h.Vspic])
    errordlg('The Top Pick must be less than the Well and Seismic picks')
    return;
end
h.Vlson=h.Vson;
h.Vson=modifysonic(h.Vson,h.Vz,h.Vt,h.Vtpic,h.Vwpic,h.Vspic);
h.picks=[h.picks;h.Vtpic,h.Vwpic,h.Vspic];
guidata(hobj,h);
strchplot(hobj,u);
end

function reset(hobj,u)
h=guidata(hobj);
rstwarn = questdlg({'Are you sure you would like to reset the well tie?';
    'All modifications will be lost'}, ...
    'Reset Warning');
switch rstwarn,
    case 'No',
        return;
    case 'Cancel',
        return;
    case 'Yes',
        h.Vlson=h.Voson;
        h.Vson=h.Voson;
        set(h.Ttpic,'string','--');h.Vtpic=nan;
        set(h.Twpic,'string','--');h.Vwpic=nan;
        set(h.Tspic,'string','--');h.Vspic=nan;
        h.picks=[];
        guidata(hobj,h);
        strchplot(hobj,u)
end %switch
end %function reset

function undostretch(hobj,u)
h=guidata(hobj);
h.Vson=h.Vlson;
h.picks=h.picks(1:end-1,:);
guidata(hobj,h);
strchplot(hobj,u)

end


function strchplot(hobj,u)
h=guidata(hobj);
[tz,zt,vins]=sonic2tz(h.Vson,h.Vz,-10000);
h.Vtz=[tz(:),zt(:)];
[sgram,tlog,rcs,pm,p]=seismo(h.Vson,h.Vrho,h.Vz,0,0,h.Vwlet,h.Vtw);
h.Vsyn=interp1(tlog,sgram,h.Vt);
h.Vsyn(isnan(h.Vsyn))=0;
h.Vsyn=h.Vsyn./max(h.Vsyn);
h.Vmpwr=h.Vt(find(cumsum(h.Vsyn.^2)>.99*max(cumsum(h.Vsyn.^2)),1));
set(h.Ffig,'currentaxes',h.Amaxis);
cla;
hold on;
for k=1:5
    if k==3
        wtva(h.Vtrcs(:,k)+k,h.Vt,[.5 0 0],h.Fwtva+k);
        wtva(h.Vsyn+k+5,h.Vt,[0 0 .5],h.Fwtva+k+5);
    else
        wtva(h.Vtrcs(:,k)+k,h.Vt,'r',h.Fwtva+k);
        wtva(h.Vsyn+k+5,h.Vt,'b',h.Fwtva+k+5);
    end
end
plot([0 11],[h.Vtpic h.Vtpic],'k:','linewidth',3);
plot([7  9],[h.Vspic h.Vspic],'k:','linewidth',3);
plot([2  4],[h.Vspic h.Vspic],'k:','linewidth',3);
ylabel('Time (s)');
l=legend('Seismic','','Synthetic','','orientation','horizontal');
set(l,'units','normalized','position',[0.60 0.015 0.35 0.04]);
set(gca,'ylim',[0 h.Vmpwr],'xlim',[0 11]);

set(h.Ffig,'currentaxes',h.Aoaxis);
plot(h.Vz,h.Voson,h.Vz,h.Vson,'r');
set(gca,'xlim',[0 h.Vopwr]);
legend('Original Sonic','Modified Sonic','location','northeast');
xlabel('Depth (m)');
ylabel('Slowness');
guidata(hobj,h);
end

function close(hobj,u)
uiresume
end