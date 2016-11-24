function tzdefine(action,arg2)
% TZDEFINE is the central part of a dialog used by LOGSEC to define time-depth
% curves. Inititate the dialog by calling TZDEFINEINIT and finish it by calling
% TZDEFINEFINI.
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
if( strcmp(action,'init'))
	stuff=get(gca,'userdata');
	ind=find(isnan(stuff));
	transfer=char(stuff(1:ind(1)-1));
	sections=char(stuff(ind(1)+1:ind(2)-1));
	hmasterfig=stuff(ind(2)+1);
	holealg=stuff(ind(2)+2);
	numlegs=stuff(ind(2)+3);
	vo=stuff(ind(2)+4);
	alphax=stuff(ind(2)+5);
	alphaz=stuff(ind(2)+6);
	isec=stuff(ind(2)+7);
    secmean=stuff(ind(2)+8:end);
   
	%make a new figure
    col=get(0,'DefaultUicontrolBackgroundColor');
	hfig=figure('visible','off','menubar','none','Units','Normalized',...
        'position',[.35 .3 .3 .4],'color',col,...
        'closerequestfcn','tzdefine(''cancel'');');
	bgkol=[0 1 1];
	fgkol=[0 0 0];
	msgkol=[1 0 0];
xnow =.05;
ynow=.85;
height=.07;
width=.9;
	htitle=uicontrol('style','text','Units','Normalized',...
        'position',[xnow ynow width height],'string',...
		'Define Time - Depth (T-Z) Curves','foregroundcolor',msgkol);
    
	ynow=ynow-height;
	hmsg=uicontrol('style','text','Units','Normalized',...
        'position',[xnow,ynow,width,height],'string',...
		'MB1: select sonic for analysis display','foregroundcolor',msgkol);
	ynow=ynow-height;
	width=(1-xnow-.05-.025)/2;
	hsectionlbl=uicontrol('style','text','Units','Normalized',...
        'string','Sonic Log Section:','position',...
		[xnow,ynow,width,height]);
	xnowans=xnow+width+.025;
	hsection=uicontrol('style','popupmenu','string',sections,...
        'Units','Normalized','position',...
		[xnowans,ynow,width,height],'callback','tzdefine(''setsection'')'...
		,'foregroundcolor',fgkol,'backgroundcolor',bgkol,...
		'value',isec);
	ynow=ynow-height;
	hholelbl=uicontrol('style','text','string','Hole Filling Algorithm:',...
		'Units','Normalized','position',[xnow+.05,ynow,width,height]);
    
    path=iconslocator;
    hholehelp=uicontrol('style','pushbutton','cdata',imread([path 'questmark.png']),...
        'Units','Normalized','position',[xnow,ynow,.05,height],...
        'callback','tzdefine(''help'');');
    
	hholealg=uicontrol('style','popupmenu','string',...
		'Constant|Linear|Mean|Layer Mean|Layer Trend','Units','Normalized',...
		'position',[xnowans,ynow,width,height],'value',holealg);
    ynow=ynow-height;
    hmodelbl=uicontrol('style','text','string','Mode:','Units','Normalized',...
        'position',[xnow,ynow,width,height]);
    
    hmode=uicontrol('style','popupmenu','string','Analyze|Compute','Units','Normalized',...
        'position',[xnowans,ynow,width,height]','callback','tzdefine(''mode'');',...
        'foregroundcolor',fgkol,'backgroundcolor',bgkol);
    
    ynow=ynow-height;
    
    hnumlegslbl=uicontrol('style','text','string','Number of Legs:',...
        'Units','Normalized','position',[xnow,ynow,width,height]);
    
    
    hnumlegs=uicontrol('style','edit','string',num2str(numlegs),'Units','Normalized',...
        'position',[xnowans,ynow,width,height]);
    
    ynow=ynow-height;
    hoverburdlab=uicontrol('style','text','string',...
        'Overburden Parameters','tooltipstring','V=Vo+ax*X  &  V=Vo+az*Z',...
    'Units','Normalized','position',[xnow,ynow,width,height]);
    
    width=.325;
    xnow=xnow+.1;
    ynow=ynow-height;
    widthans=width-.15;
    hvolbl=uicontrol('style','text','string','Surface Velocity (Vo):',...
        'Units','Normalized','position',[xnow,ynow,width,height]);
    
    
    hvo=uicontrol('style','edit','string',num2str(vo),'Units','Normalized',...
        'position',[xnowans,ynow,widthans,height],'callback','tzdefine(''getaz'');');
    
    ynow=ynow-height;
    halphaxlbl=uicontrol('style','text','string','Lateral Coefficient (ax):',...
        'Units','Normalized','Position',[xnow,ynow,width,height]);
	
	
	halphax=uicontrol('style','edit','string',num2str(alphax),...
        'Units','Normalized','position',[xnowans,ynow,widthans,height]);
	
	ynow=ynow-height;
	halphazlbl=uicontrol('style','text','string','Depth Coefficent (az):',...
        'Units','Normalized','Position',[xnow,ynow,width,height]);
	
	
	halphaz=uicontrol('style','edit','string',num2str(alphaz),...
        'Units','Normalized','position',[xnowans,ynow,widthans,height],...
        'Userdata',secmean);
		
	ynow=ynow-height;
    xnow=.05;
    width=.425;
    xnowans=0.5125;
	hnamelbl=uicontrol('style','text','string','TZ name:','Units','Normalized',...
        'Position',[xnow,ynow,width,height]);
		
	sections=strvec2mat(sections);
	hname=uicontrol('style','edit','string',['TZ: ' ...
		sections(isec,:)],'Units','Normalized','position',...
        [xnowans,ynow,width,height]);
	%the buttons
	xnow=.325;
	ynow=.05;
	width=.15;
	% the done button is enabled when the mode is switched to compute
	hdone=uicontrol('style','pushbutton','string','Done',...
        'Units','Normalized','position',[xnow,ynow,width,height],...
        'callback','tzdefine(''done'');',...
		'userdata',transfer,'enable','off');
	xnow=xnow+width+.05;
	hcancel=uicontrol('style','pushbutton','string','Cancel',...
        'Units','Normalized','position',[xnow,ynow,width,height],...
        'callback','tzdefine(''cancel'');');
ch=get(hfig,'children');
set(ch,'fontsize',10,'HorizontalAlignment','left')
chtm=[htitle hmsg];
    set(chtm,'fontweight','bold','HorizontalAlignment','center')
    
	set(hfig,'userdata',[hmasterfig,hmsg,hsection,hholealg,hmode,hnumlegs,hvo,...
		halphax,halphaz,hdone,hcancel hname]);
		
	tzdefine('mode');
	set(hfig,'visible','on');
	return;
end
%
% handle the done button
%
if(strcmp(action,'done'))
	h=get(gcf,'userdata');
	hmasterfig=h(1);
	hdone=h(10);
	
	tzdefine('getparameters',gcf);
	
	%call the transfer function
	transfer=get(hdone,'userdata');
	delete(gcf);
	figure(hmasterfig);
	eval(transfer);
	
	return;
end
% 
% do a cancel
% 
if(strcmp(action,'cancel'))
	h=get(gcf,'userdata');
	hmasterfig=h(1);
	hmode=h(5);
	hdone=h(10);
	
	fcn=get(hmode,'userdata');
	if(~isempty(fcn))
		set(hmasterfig,'windowbuttonupfcn',fcn);
	else
			set(hmasterfig,'windowbuttonupfcn','');
	end
	
	transfer=get(hdone,'userdata');
	
	hax=get(hmasterfig,'currentaxes');
	
	set(hax,'userdata',-1);
	
	delete(gcf);
	
	figure(hmasterfig);
	eval(transfer);
	
	return;
	
end
%
% the mode switch
%
if(strcmp(action,'mode'))
	h=get(gcf,'userdata');
	hmasterfig=h(1);
	hmsg=h(2);
	hmode=h(5);
	hdone=h(10);
	flag=get(hmode,'value');
	
	if(flag==1)
		set(hdone,'enable','off');
		
		fcn=get(hmasterfig,'windowbuttonupfcn');
		tst=get(hmode,'userdata');
		if(isempty(tst))
			set(hmode,'userdata',fcn);
		end
		
		set(hmasterfig,'windowbuttonupfcn','logsec(''tzanal'')');
		
		set(hmsg,'string','MB1: select sonic for analysis display');
		
	else
		set(hdone,'enable','on');
		
		fcn=get(hmode,'userdata');
		if(~isempty(fcn))
			set(hmasterfig,'windowbuttonupfcn',fcn);
		else
			set(hmasterfig,'windowbuttonupfcn','');
		end
		
		set(hmsg,'string','Push done to compute tz function');
	end
	
	return;
	
end
%
%
%
if(strcmp(action,'getparameters'))
	hfig=arg2;
	
	h=get(hfig,'userdata');
	hmasterfig=h(1);
	hmsg=h(2);
	hsection=h(3);
	hholealg=h(4);
	hmode=h(5);
	hnumlegs=h(6);
	hvo=h(7);
	halphax=h(8);
	halphaz=h(9);
	hdone=h(10);
	hcancel=h(11);
	hname=h(12);
	hax=get(hmasterfig,'currentaxes');
	
	%get the number of legs
	numlegs=sscanf(get(hnumlegs,'string'),'%d');
	if( numlegs<0 | isempty(numlegs) | floor(numlegs)~=numlegs)
		set(hax,'userdata',-1);
		figure(hfig);
		set(hmsg,'string','Number of legs must be a positive integer');
		error('Number of legs must be a positive integer');
	end
	
	%get the initial velocity
	vo=sscanf(get(hvo,'string'),'%f');
	if( vo<0 | isempty(vo) | isnan(vo) )
		set(hax,'userdata',-1);
		figure(hfig);
		set(hmsg,'string','Vo must be a positive real number or inf');
		error('Vo must be a positive real number or inf');
	end
	
	%get the x accelerator
	alphax=sscanf(get(halphax,'string'),'%f');
	if( isempty(alphax) | alphax<-10 | alphax>10 )
		set(hax,'userdata',-1);
		figure(hfig);
		set(hmsg,'string','Alphax must be between -10 and 10');
		error('Alphax must be between -10 and 10');
	end
	
	
	%get the z accelerator
	alphaz=sscanf(get(halphaz,'string'),'%f');
	if( isempty(alphaz) | alphaz<-10 | alphaz>10 )
		set(hax,'userdata',-1);
		figure(hfig);
		set(hmsg,'string','Alphaz must be between -10 and 10');
		error('Alphaz must be between -10 and 10');
	end
	
	%get the name
	name=get(hname,'string');
	if( strcmp(name,'') | strcmp(name,' ') | length(name)>30 )
		set(hax,'userdata',-1);
		figure(hfig);
		set(hmsg,'string','Provide a non-blank name, max of 30 chars');
		error('Provide a non-blank name');
	end
	
	% get the section choice
	sectionum=get(hsection,'value');
	
	%the hole algorithm
	holealg=get(hholealg,'value');
	
	%store this stuff
	
	set(hax,'userdata',[sectionum holealg numlegs vo alphax alphaz abs(name)]);
	
	return;
end
%
%
%
if(strcmp(action,'setsection'))
	h=get(gcf,'userdata');
	hsection=h(3);
	hname=h(12);
    
	secnum=get(hsection,'value');
	secnames=get(hsection,'string');
	set(hname,'string',['TZ: ' secnames(secnum,:)]);
	tzdefine('getaz');
	return;
end
if (strcmp(action,'getaz'))
	h=get(gcf,'userdata');
	hsection=h(3);
	halphaz=h(9);
    hvo=h(7);
    
    secmean=get(halphaz,'userdata');
	secmeansz=size(secmean);
    secnum=get(hsection,'value');
    if secnum==1
     set(halphaz,'string','0.6');   
    else
    secmeans=secmean(1,1:(secmeansz(2)/2));
    depths=secmean(1,(1+secmeansz(2)/2):end);
    vo=str2double(get(hvo,'string'));
    mean=(secmeans(secnum-1)-vo)/depths(secnum-1);
	set(halphaz,'string',num2str(mean));
    end
	return;
end
if(strcmp(action,'help'))
    d=dialog('Name','Hole Filling Algorithm Help','units','normalized',...
        'position',[.325,.25,.35,.5],'closerequestfcn','delete(gcbf)');
    helpmsg={'';
        'Constant:';
        '     Fills a hole in the data with a constant that is derived from';
        '        the mean of the last 10 samples above the hole';
        '';
        'Linear:';
        '     Fills a hole in the data with a linear trend from the 10 samples';
        '        above the hole to the mean of the 10 samples below the hole.';
        '';
        'Mean:';
        '     Fills a hole in the data with the mean value of the section.';
        '         All holes are filled with the same value';
        '';
        'Layer Mean:';
        '     Fills a hole with the mean value of the layer in which the hole';
        '         is located.  Empty layers are filled with the mean value of';
        '         the layer above and below the layer of interest';
        '';
        'Layer Trend:';
        '     Fill a hole with a linear trend fit to the samples in a layer.';
        '         Empty Layers will get a trend derived from the layers above';
        '         and below the layer of interest';
        '';};
    listbox=uicontrol(d,'style','listbox','units','normalized','position',...
        [0,.1,1,.9],'String',helpmsg,'Fontsize',12,'SelectionHighlight','off');
    closebutt=uicontrol(d,'style','pushbutton','units','normalized','position',...
        [.4,.0125,.2,.075],'String','OK','callback','close(gcbf)');
	return
end
	
		
	