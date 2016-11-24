function dataout=winextractor(masteraxes,trc,midpt,smpls,lockto);
%*******************
%*** WINEXTACTOR ***
%*******************
%
% dataout=winextractor(masteraxes,trc,midpt,smples);
% dataout=winextractor(masteraxes,trc,midpt,smples,lockto);
% 
% WINEXTRACTOR makes it easy for a user to visualize and adjusting a
% desired windowed area on a single trace.  
% 
% This program will open up a new figure
%
% masteraxes:  Is the handle of the axes of the main figure from where
% WINEXTRACTOR is being called from.  This is for alignment purposes.
%
% trc:  As of August 19th, 2004, WINEXTRACTOR accepts only one trace for
% window extraction.  This imput can have two columns, the first column
% being the trace with the second column being the y coordinates.
%
% midpt:  The midpoint is the center of the initial window.
%
% smples:  Is the number of samples, up and down, from the mid point where
% the window will be extracted from.
%
% lockto: can be set to 'open', 'peak', 'trough', 'zerocrossing'
% (default = open)
%
% dataout: Is a structure set with two elements. midpoint, which is the
% midpoint of the windowed area and samples, with are the equal distance of
% the top and bottom of window from the midpoint.
%
% 
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

%
if(nargin==1)
    % this should be changed one day, it is only here due to the various
    % selbox routines that are needed for zooming purposes
    if(~char(masteraxes))
        stringinfo={'Not enough arguments, see winextractor help for details'};
        helpdlg(stringinfo,'Check WINEXTRACTOR help file');
        return
    elseif(strcmp(masteraxes,'ZOOM'))
        % Pulling Data out
        udat=get(gcf,'userdata');
        figdata=getfield(udat,'Data');
        trc=getfield(figdata,'Trace');
        y=getfield(figdata,'Y');
        smpls=getfield(figdata,'Sample');
        midpt=getfield(figdata,'Midpoint');
        fighandles=getfield(udat,'Handles');
        hsliderx=getfield(fighandles,'SliderX');
        hslidery=getfield(fighandles,'SliderY');
        hmsg=getfield(fighandles,'Message');
        hzoom=getfield(fighandles,'Zoom');
        topln=getfield(fighandles,'Top');
        botln=getfield(fighandles,'Bottom');
        centpt=getfield(fighandles,'Center');
        %--------------------------------
        box=selboxfini;
        try
 	     delete(box{2});
        catch
	     %no selbox to delete
        end
	box = box{1};

        if(isempty(box)|iscell(box)|length(box)<=3) return; end
        xmin=min([box(1) box(3)]);
        xmax=max([box(1) box(3)]);
        ymin=min([box(2) box(4)]);
        ymax=max([box(2) box(4)]);
        %get the current axis settings
        xlim=get(gca,'xlim');
        ylim=get(gca,'ylim');
        test1=xmin-xlim(1)+xmax-xlim(2)+ymin-ylim(1)+ymax-ylim(2);
        test2=(xmin-xmax)*(ymin-ymax);
        if(abs(test1)<10*eps|abs(test2)< 10*eps|strcmp(get(gcf,'selectiontype'),'extend'))
            xdat=[-1.2 1.2];
            ydat=[min(y) max(y)];
            set(gca,'xlim',xdat,'ylim',ydat);
            set(hsliderx,'visible','off');
            set(hslidery,'visible','off');
        else
            xdat=sort([-1.2 xmin xmax 1.2]);
            ydat=sort([y(1) ymin ymax y(end)]);
            xdat=[xdat(2) xdat(3)];
            ydat=[ydat(2) ydat(3)];
            set(gca,'xlim',[xdat],'ylim',[ydat]);
            ximlim=trc;
            yimlim=y;
            xdat2=sort(get(gca,'xlim'));
            ydat2=sort(get(gca,'ylim'));
            set(hsliderx,'value',(xdat2(2)-xdat2(1))/2+xdat2(1),'visible','on','enable','on',...
                'userdata',{[2] [(xdat2(2)-xdat2(1))/2+xdat2(1)] []},...
                'max',1.2,'min',-1.2);
            y1=yimlim(end)-(ydat2(1)-yimlim(1));
            y2=yimlim(1)+(yimlim(end)-ydat2(2));
            set(hslidery,'value',y2+(y1-y2)/2,'visible','on','userdata',{[1] [y2+(y1-y2)/2] []},...
                'enable','on','max',yimlim(end),'min',yimlim(1));
        end
        return   
    else
        % something is wrong
        return
    end
elseif(nargin<4)
    stringinfo={'Not enough arguments, see winextractor help for details'};
    helpdlg(stringinfo,'Check WINEXTRACTOR help file');
    return
elseif(nargin==4)
    lockto='open';
elseif(nargin==5)
end
% checking values -----
if(~ishandle(masteraxes))
    stringinfo={'Masteraxes not choosen properly'};
    helpdlg(stringinfo,'Check WINEXTRACTOR help file');
    return
elseif(~crisgraphics(masteraxes,'axes'))
    stringinfo={'Masteraxes not choosen properly'};
    helpdlg(stringinfo,'Check WINEXTRACTOR help file');
    return
end
if(~isnumeric(trc))
    stringinfo={'Trace sent to WINEXTRACTOR has to be numeric'};
    helpdlg(stringinfo,'Check WINEXTRACTOR help file');
    return
else
    [rol col]=size(trc);
    if(col>3)
        % the sucker is wrong orientation, needs to be shifted, this was a
        % quick fix made on Monday November 1st 2004, might have to change
        % in the future
        trc=trc';
    end
    if(size(trc,2)<2)
        % y coordinates not give so just using numbers
        y=[1:1:length(trc)];
        x=trc;
    else
        y=trc(:,2);
        x=trc(:,1);
    end
end
if(~isnumeric(midpt))
    stringinfo={'Midpoint sent to WINEXTRACTOR has to be numeric'};
    helpdlg(stringinfo,'Check WINEXTRACTOR help file');
    return
else
    if(midpt<=0)|(length(trc)<midpt)
        stringinfo={'Midpoint sent to WINEXTRACTOR has to be on trace'};
        helpdlg(stringinfo,'Check WINEXTRACTOR help file');
        return
    end
end
if(~isnumeric(smpls))
    stringinfo={'Samples sent to WINEXTRACTOR have to be numeric'};
    helpdlg(stringinfo,'Check WINEXTRACTOR help file');
    return
else
    smpls=round(abs(smpls));
end
if(~isstr(lockto))
    stringinfo={'Lockto sent to WINEXTRACTOR has to be set as "open", "peak", "trough", or "zerocross"'};
    helpdlg(stringinfo,'Check WINEXTRACTOR help file');
    return
else
    checkstr=['open peak trough zerocross+ zerocross-'];
    if(isempty(findstr(checkstr,lockto)))
        stringinfo={'Lockto sent to WINEXTRACTOR has to be set as "open", "peak", "trough", or "zerocross"'};
        helpdlg(stringinfo,'Check WINEXTRACTOR help file');
        return
    end
    LockTo=deblank(lockto);
end
% checking is finished -----
masterfig=get(masteraxes,'parent');
% checking trace
if(size(trc,2)<2)
    % y coordinates not give so just using numbers
    y=[1:1:length(trc)];
    x=trc;
else
    y=trc(:,2);
    x=trc(:,1);
end

% assuming every thing else is ok...for now
hfig=figcent(380,698,'pixels',masterfig);

set(hfig,'MenuBar','none','Name','Window Extractor','NumberTitle','off',...
    'HandleVisibility','callback','Tag','WINEXTACTOR',...
    'closerequestfcn',@WE_Cancel);
haxes=axes('Position',[0.1289 0.1375 0.8289 0.7893],'Tag','axes1');
% plotting information on axes
x=x-mean(x);
x=x/(max(x));   % Normalizing data, incase it is not normalized already
plot(x,y,'k');
if((midpt+smpls+3>length(y)))
    yend=length(y);
else
    yend=midpt+smpls+3;
end
if((midpt-smpls-3<1))
    ystr=1;
else
    ystr=midpt-smpls-3;
end
yall=sort([y(1) y(end) y(ystr) y(yend)]);
if((midpt+smpls>length(y)))
    yend2=length(y);
else
    yend2=midpt+smpls;
end
if((midpt-smpls<1))
    ystr2=1;
else
    ystr2=midpt-smpls;
end
ytp=sort([y(1) y(ystr)]);
ybt=sort([y(end) y(yend2)]);
% patch([1 -1 -1 1],[ytp(2) ytp(2) ybt(1) ybt(1)],'r','FaceAlpha',.05);
ttl=get(masteraxes,'title');
ttl=get(ttl,'string');
title(ttl,'interpreter','none');
ylbl=get(masteraxes,'ylabel');
ylbl=get(ylbl,'string');
ylabel(ylbl);
line([0 0],[y(1) y(end)],'color','k'); % line down the middle
centerpt=line([x(midpt) x(midpt)],[y(midpt) y(midpt)],'color','b','marker','*','markersize',5,...
    'buttondownfcn',@WE_LineButtonDown,'tag','MIDPOINT');
xlm=get(haxes,'xlim');
dy=abs(y(2)-y(1));
topln=line([1.2 -1.2],[ytp(2) ytp(2)],'color','b','linestyle','--',...
    'buttondownfcn',@WE_LineButtonDown,'tag','TOP');
botln=line([1.2 -1.2],[ybt(1) ybt(1)],'color','b','linestyle','--',...
    'buttondownfcn',@WE_LineButtonDown,'tag','BOTTOM');
yall=sort([y(1) y(end) y(ystr) y(yend)]);
set(gca,'xlim',[-1.2 1.2],'ylim',[yall(2) yall(3)]);
flipy;
hsliderY=uicontrol('Units','normalized','Callback',@WE_slider,...
    'Position',[0.958 0.138 0.034 0.788],'Style','slider','Tag','Y',...
    'visible','off');
hsliderX=uicontrol('Units','normalized','Callback',@WE_slider,...
    'Position',[0.129 0.119 0.821 0.02],'Style','slider','Tag','X',...
    'visible','off');

hmsg=uicontrol('Units','characters','BackgroundColor',[1 1 1],...
    'Position',[0 -0.0769230769230769 76 1.46153846153846],'String','Window Extractor',...
    'Style','text');

hcontinue = uicontrol('Units','normalized','Callback',@WE_Continue,...
    'Position',[0.3763 0.0343 0.2657 0.0329],'String','Conitnue','Tag','Continue');

hcancel = uicontrol('Units','normalized','Callback',@WE_Cancel,...
    'Position',[0.7052 0.03438 0.2657 0.0329],'String','Cancel','Tag','Cancel');

hzoom = uicontrol('Units','normalized','Callback',@WE_Zoom,...
    'Position',[0.05789 0.03438 0.2657 0.0329],'String','Un-Zoom','Tag','Zoom');

% Taking care of lockto requirements here.


fighandles.SliderY=hsliderY;
fighandles.SliderX=hsliderX;
fighandles.Message=hmsg;
fighandles.Continue=hcontinue;
fighandles.Cancel=hcancel;
fighandles.Zoom=hzoom;
fighandles.Top=topln;
fighandles.Bottom=botln;
fighandles.Center=centerpt;

figdata.MasterAxes=masteraxes;
figdata.Trace=x;
figdata.Y=y;
figdata.Sample=smpls;
figdata.Midpoint=midpt;
figdata.LockTo=LockTo;

figinfo.Data=figdata;
figinfo.Handles=fighandles;

set(gcf,'userdata',figinfo);

uiwait;
udat=get(gcf,'userdata');
figdata=getfield(udat,'Data');
trc=getfield(figdata,'Trace');
y=getfield(figdata,'Y');
smpls=getfield(figdata,'Sample');
midpt=getfield(figdata,'Midpoint');
fighandles=getfield(udat,'Handles');
hmsg=getfield(fighandles,'Message');
topln=getfield(fighandles,'Top');
botln=getfield(fighandles,'Bottom');
centpt=getfield(fighandles,'Center');
%--------------------------------

if(isempty(smpls)|isempty(centpt))
    % canceling
    dataout=[];
else
    ydat=get(centpt,'ydata');
    ydat=find(ydat(1)==y);
    dataout.midpoint=ydat;
    dataout.samples=abs(smpls);
end
delete(gcf);

% Axes Slider Conotrl
%---------------------
function WE_slider(hObject, eventdata, handles)
% This controls the slider for the axes
% not activated yet
% Pulling Data out
udat=get(gcf,'userdata');
figdata=getfield(udat,'Data');
trc=getfield(figdata,'Trace');
y=getfield(figdata,'Y');
smpls=getfield(figdata,'Sample');
midpt=getfield(figdata,'Midpoint');
fighandles=getfield(udat,'Handles');
hsliderx=getfield(fighandles,'SliderX');
hslidery=getfield(fighandles,'SliderY');
hmsg=getfield(fighandles,'Message');
hzoom=getfield(fighandles,'Zoom');
topln=getfield(fighandles,'Top');
botln=getfield(fighandles,'Bottom');
centpt=getfield(fighandles,'Center');
%--------------------------------
xdat=sort([1.2 -1.2]);
ydat=sort([y(1) y(end)]);
xlim=get(gca,'xlim');
ylim=get(gca,'ylim');
ckslide=get(gcbo,'tag');
switch ckslide
    case 'Y'  % Vertical Slider
        verval=get(hslidery,'value');
        udat=get(hslidery,'userdata');
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
        set(gca,'ylim',newylim);
        set(hslidery,'value',udat{2},'userdata',udat);
    case 'X'  % Horizontal Slider
        horval=get(hsliderx,'value');
        udat=get(hsliderx,'userdata');
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
        set(gca,'xlim',newxlim);
        set(hsliderx,'value',udat{2},'userdata',udat);
end

% Continue
%----------
function WE_Continue(hObject, eventdata, handles)
% Accepting 
uiresume;

% Cancel
%--------
function WE_Cancel(hObject, eventdata, handles)
% Canceling Window extractor
% Pulling Data out
udat=get(gcf,'userdata');
figdata=getfield(udat,'Data');
trc=getfield(figdata,'Trace');
y=getfield(figdata,'Y');
smpls=getfield(figdata,'Sample');
midpt=getfield(figdata,'Midpoint');
fighandles=getfield(udat,'Handles');
hmsg=getfield(fighandles,'Message');
topln=getfield(fighandles,'Top');
botln=getfield(fighandles,'Bottom');
centpt=getfield(fighandles,'Center');
%--------------------------------

checkwuser=questdlg('Do you want to cancel?','Cancel Winextractor',...
    'Yes','No','Yes');
switch checkwuser
    case 'Yes'
        figdata.Sample=[];
        figdata.centpt=[];
        udat.Data=figdata;
        set(gcf,'userdata',udat);
        uiresume;
    case 'No'
        stringinfo='Action Canceled';
        set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);
end

% Zoom
%------
function WE_Zoom(hObject, eventdata, handles)
% Will control zooming in and out of figure
% Pulling Data out
udat=get(gcf,'userdata');
figdata=getfield(udat,'Data');
trc=getfield(figdata,'Trace');
y=getfield(figdata,'Y');
smpls=getfield(figdata,'Sample');
midpt=getfield(figdata,'Midpoint');
fighandles=getfield(udat,'Handles');
hmsg=getfield(fighandles,'Message');
hzoom=getfield(fighandles,'Zoom');
topln=getfield(fighandles,'Top');
botln=getfield(fighandles,'Bottom');
centpt=getfield(fighandles,'Center');
%--------------------------------
nm=get(hzoom,'string');
switch nm
    case 'Un-Zoom'
        set(gca,'ylim',[y(1) y(end)],'xlim',[-1.2 1.2]);
        set(hzoom,'string','Zoom');
        stringinfo='Un-Zoomed';
        selboxinit('winextractor(''ZOOM'');');
        col=[1 1 1];
    case 'Zoom'
        set(hzoom,'string','Zoom');
        % using selboxinit... which should change to something else one
        % day.
        selboxinit('winextractor(''ZOOM'');');
        stringinfo='Zooming has been enabled';
        col=[1 1 1];
    case '*Zoom*'
        % was something... now is not
    case 'XXX'
end
set(hmsg,'string',stringinfo,'backgroundcolor',col);

% Line button down
%---------------------
function WE_LineButtonDown(hObject, eventdata, handles)
% Pulling Data out
udat=get(gcf,'userdata');
figdata=getfield(udat,'Data');
trc=getfield(figdata,'Trace');
y=getfield(figdata,'Y');
smpls=getfield(figdata,'Sample');
midpt=getfield(figdata,'Midpoint');
fighandles=getfield(udat,'Handles');
hmsg=getfield(fighandles,'Message');
topln=getfield(fighandles,'Top');
botln=getfield(fighandles,'Bottom');
centpt=getfield(fighandles,'Center');
%--------------------------------

pt=get(gca,'currentpoint');

checktag=get(gco,'tag');
switch checktag
    case 'MIDPOINT'
    case 'TOP'
    case 'BOTTOM'
end
set(gcf,'windowbuttonmotionfcn',@WE_WindowButtonMotion);
set(gcf,'windowbuttonupfcn',@WE_WindowButtonUp);
set(gca,'userdata',pt);

% Window Button Motion Function
%-------------------------------
function WE_WindowButtonMotion(hObject, eventdata, handles)
% Pulling Data out
udat=get(gcf,'userdata');
figdata=getfield(udat,'Data');
trc=getfield(figdata,'Trace');
y=getfield(figdata,'Y');
smpls=getfield(figdata,'Sample');
midpt=getfield(figdata,'Midpoint');
lockto=getfield(figdata,'LockTo');
fighandles=getfield(udat,'Handles');
hmsg=getfield(fighandles,'Message');
topln=getfield(fighandles,'Top');
botln=getfield(fighandles,'Bottom');
centpt=getfield(fighandles,'Center');
%--------------------------------
ylm=get(gca,'ylim');
oldpt=get(gca,'userdata');
newpt=get(gca,'currentpoint');
checktag=get(gco,'tag');
% not allowing point to go off of axes
if(newpt(1,2)<ylm(1))
    nm=find(ylm(1)>=y);
elseif(newpt(1,2)>ylm(2))
    nm=find(ylm(2)>=y);
else
    nm=find(newpt(1,2)>=y);
end
if(isempty(nm))
    nm=length(y);
else
    nm=nm(end);
end
switch checktag
    case 'MIDPOINT'
        if(~strcmp(lockto,'open'))
            % locking only to points requested by user
            dataout=nodefind(trc,nm,lockto);
            if(isempty(dataout))
                return
            end
            nm=dataout;
        end
        nmtp=sort([(nm-smpls) 1 length(y)]);
        nmtp=nmtp(2);
        nmbt=sort([(nm+smpls) 1 length(y)]);
        nmbt=nmbt(2);
        nmctx=[trc(nm) trc(nm)];
        nmcty=[y(nm) y(nm)];
        newsmpls=smpls;
    case 'TOP'
        nmtp=nm;
        nmbt=sort([(nmtp+2*smpls+1) 1 length(y)]);
        nmbt=nmbt(2);
        nmctx=get(centpt,'xdata');
        nmcty=get(centpt,'ydata');
        nm=find(nmcty(1)>=y);
        newsmpls=(nm(end)-nmtp);
    case 'BOTTOM'
        nmbt=nm;
        nmctx=get(centpt,'xdata');
        nmcty=get(centpt,'ydata');
        nmxx=find(nmcty(1)>=y);
        nmtp=sort([(nmbt-2*smpls-1) 1 length(y)]);
        nmtp=nmtp(2);
        newsmpls=(nmbt-nmxx(end));
end
set(centpt,'xdata',nmctx,'ydata',nmcty);
set(topln,'ydata',[y(nmtp) y(nmtp)]);
set(botln,'ydata',[y(nmbt) y(nmbt)]);
figdata.Sample=newsmpls;
udat.Data=figdata;
set(gcf,'userdata',udat);

% Window Button Up Function
%---------------------------
% Pulling Data out
udat=get(gcf,'userdata');
figdata=getfield(udat,'Data');
trc=getfield(figdata,'Trace');
y=getfield(figdata,'Y');
smpls=getfield(figdata,'Sample');
midpt=getfield(figdata,'Midpoint');
fighandles=getfield(udat,'Handles');
hmsg=getfield(fighandles,'Message');
topln=getfield(fighandles,'Top');
botln=getfield(fighandles,'Bottom');
centpt=getfield(fighandles,'Center');
%--------------------------------
function WE_WindowButtonUp(hObject, eventdata, handles)

checktag=get(gco,'tag');
switch checktag
    case 'MIDPOINT'
    case 'TOP'
    case 'BOTTOM'
end
set(gcf,'windowbuttonupfcn','');
set(gcf,'windowbuttonmotionfcn','');
set(gcf,'windowbuttondownfcn','');
selboxinit('winextractor(''ZOOM'');');