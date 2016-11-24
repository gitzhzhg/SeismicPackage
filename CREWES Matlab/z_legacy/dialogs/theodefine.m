function theodefine(action,arg2)
% THEODEFINE is the central part of a dialog used by LOGSEC to define
% reflection coefficient sections. Inititate the dialog by calling
% THEODEFINEINIT and finish
% it by calling THEODEFINEFINI.
%
% G.F. Margrave April 1994
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
	transfer=setstr(stuff(1:ind(1)-1));
	sections=setstr(stuff(ind(1)+1:ind(2)-1));
	msg=setstr(stuff(ind(2)+1:ind(3)-1));
	hmasterfig=stuff(ind(3)+1);
	densopt=stuff(ind(3)+2);
	holealg=stuff(ind(3)+3);
	theopt=stuff(ind(3)+4);
	a=stuff(ind(3)+5);
	m=stuff(ind(3)+6);
	sonicnum=stuff(ind(3)+7);
	densitynum=stuff(ind(3)+8);
	name=stuff(ind(3)+9:length(stuff));
	fgkol=[0 0 0];
	bgkol=[0 1 1];
	%make a new figure
	hfig=figure('visible','off','menubar','none');
	pos=get(hfig,'position');
	height=20;
	%figheight=11*height;
	figheight=12*height;
	figwidth=402;
	sep=1;
	xnow=sep;
	ynow=figheight-height-sep;
	width=400;
	htitle=uicontrol('style','text','position',[xnow,ynow,width,height],'string',...
		'Theogram Computation Specifications','foregroundcolor','r');
	xnow=sep;
	ynow=ynow-height-sep;
	hmsg=uicontrol('style','text','position',[xnow,ynow,width,height],'string',...
		msg);
	xnow=sep;
	ynow=ynow-height-sep;
	width=150;
	hsonicseclbl=uicontrol('style','text','string','Sonic Log Section:','position',...
		[xnow,ynow,width,height]);
	xnow=xnow+width+sep;
	width=260;
	hsonicsec=uicontrol('style','popupmenu','string',sections,'position',...
		[xnow,ynow,width,height],'callback','theodefine(''setsection'')',...
		'value',sonicnum,'backgroundcolor',bgkol,'foregroundcolor',fgkol);
		
	xnow=sep;
	ynow=ynow-height-sep;
	width=150;
	hdensityseclbl=uicontrol('style','text','string','Density Log Section:','position',...
		[xnow,ynow,width,height]);
	xnow=xnow+width+sep;
	width=260;
	hdensitysec=uicontrol('style','popupmenu','string',sections,'position',...
		[xnow,ynow,width,height],'value',densitynum);
		
	xnow=sep;
	ynow=ynow-height-sep;
	width=150;
	hdensoptlbl=uicontrol('style','text','string','Density Options:','position',...
		[xnow,ynow,width,height]);
	xnow=xnow+width+sep;
	width=260;
	doptmsg=['Constant Density' '|Exclusivly Gardners' '|Density Section plus Gardners' ...
			'|Exclusively Density Section'];
	hdensopt=uicontrol('style','popupmenu','string',doptmsg,...
		'position',[xnow,ynow,width,height],'callback','theodefine(''densopt'')',...
		'value',densopt+1,'backgroundcolor',bgkol,'foregroundcolor',fgkol);
	ynow=ynow-height-sep;
	xnow=sep;
 width=400;
 hgardnerlbl=uicontrol('style','text','string',...
 		'Gardners relation is density=a*(vins)^m. Provide a and m:',...
		'position',[xnow,ynow,width,height],'visible','off');
	xnow=sep;
	ynow=ynow-height-sep;
 width=40;
 halbl=uicontrol('style','text','string','a:','position',...
		[xnow,ynow,width,height],'visible','off');
 xnow=xnow+width+sep;
 width=60;
 ha=uicontrol('style','edit','string',num2str(a),'position',...
		[xnow,ynow,width,height],'backgroundcolor',bgkol,'foregroundcolor',fgkol,'visible','off');
 xnow=xnow+width+sep;
 width=40;
	hmlbl=uicontrol('style','text','string','m:','Position',...
		[xnow,ynow,width,height],'visible','off');
	
	xnow=xnow+sep+width;
	width=60;
	hm=uicontrol('style','edit','string',num2str(m),'position',...
		[xnow,ynow,width,height],'backgroundcolor',bgkol,'foregroundcolor',fgkol,'visible','off');
	ynow=ynow-height-sep;
	xnow=sep;
	width=150;
	hholelbl=uicontrol('style','text','string','Hole Filling Algorithm:',...
		'position',[xnow,ynow,width,height]);
	xnow=xnow+width+sep;
	width=250;
	hholealg=uicontrol('style','popupmenu','string',...
		'Constant|Linear|Mean|Layer Mean|Layer Trend',...
		'position',[xnow,ynow,width,height],'value',holealg);
	xnow=sep;
	ynow=ynow-height-sep;
	width=150;
	htheoptlbl=uicontrol('style','text','string','Theogram Options:','position',...
		[xnow,ynow,width,height],'visible','on');
	xnow=xnow+width+sep;	
	width=250;
	htheopt=uicontrol('style','popupmenu','string',...
		'Primaries only|Primaries plus multiples',...
		'foregroundcolor',fgkol,'backgroundcolor',bgkol,...
		'position',[xnow,ynow,width,height]','value',theopt,'visible','on');
		
	xnow=sep;
	ynow=ynow-height-sep;
	width=150;
	hnamelbl=uicontrol('style','text','string','Theo name (no spaces):','Position',...
		[xnow,ynow,width,height]);
		
	xnow=xnow+width+sep;
	width=250;
	secnames=get(hsonicsec,'string');
	if(length(name)==1)
		name=['seis__' secnames(sonicnum,:)];
	else
		name=setstr(name);
	end
	hname=uicontrol('style','edit','string',name, 'position',...
						[xnow,ynow,width,height],'backgroundcolor',bgkol,'foregroundcolor',fgkol);
	%the buttons
	xnow=sep;
	ynow=ynow-height-sep;
	width=60;
	% the done button is enabled when the mode is switched to compute
	hdone=uicontrol('style','pushbutton','string','Done','position',...
		[xnow,ynow,width,height],'callback','theodefine(''done'');',...
		'userdata',transfer,'enable','on');
	xnow=xnow+width+sep;
 	width=60;
	hcancel=uicontrol('style','pushbutton','string','Cancel','position',...
		[xnow,ynow,width,height],'callback','theodefine(''cancel'');');
	set(hfig,'userdata',[hmasterfig,hmsg,hsonicsec,hdensitysec,hdensopt,...
		hholealg,htheopt,hgardnerlbl,halbl,ha,hmlbl,hm,hdone,hcancel hname]);
				
	set(hfig,'position',[pos(1:2) figwidth figheight]);
	set(hfig,'visible','on');
	return;
end
%
% handle the done button
%
if(strcmp(action,'done'))
	h=get(gcf,'userdata');
	hdone=h(13);
	
	theodefine('getparameters',gcf);
	
	%call the transfer function
	transfer=get(hdone,'userdata');
	close(gcf);
	eval(transfer);
	
	return;
end
% 
% do a cancel
% 
if(strcmp(action,'cancel'))
	h=get(gcf,'userdata');
	hmasterfig=h(1);
	hdone=h(13);
	
	transfer=get(hdone,'userdata');
	
	hax=get(hmasterfig,'currentaxes');
	
	set(hax,'userdata',-1);
	
	close(gcf);
	
	eval(transfer);
	
	return;
	
end
%
% the density option 
%
if(strcmp(action,'densopt'))
	h=get(gcf,'userdata');
	hdensitysec=h(4);
	hdensopt=h(5);
	hgardnerlbl=h(8);
	halbl=h(9);
	ha=h(10);
	hmlbl=h(11);
	hm=h(12);
    hdone=h(13);
	flag=get(hdensopt,'value');
	flag=flag-1;
	fgkol=[0 0 0];
	bgkol=[0 1 1];
	
	if(flag==1|flag==2)
		set(hgardnerlbl,'visible','on');
		set(halbl,'visible','on');
		set(ha,'visible','on');
		set(hmlbl,'visible','on');
		set(hm,'visible','on');
	else
		set(hgardnerlbl,'visible','off');
		set(halbl,'visible','off');
		set(ha,'visible','off');
		set(hmlbl,'visible','off');
		set(hm,'visible','off');
		set(hdone,'enable','on');
	end
	if( flag==2 | flag==3)
		set(hdensitysec,'backgroundcolor',bgkol,'foregroundcolor',fgkol);
	else
		set(hdensitysec,'backgroundcolor',[.702 .702 .702],'foregroundcolor','k');
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
	hsonicsec=h(3);
	hdensitysec=h(4);
	hdensopt=h(5);
	hholealg=h(6);
	htheopt=h(7);
	ha=h(10);
	hm=h(12);
	hdone=h(13);
	hcancel=h(14);
	hname=h(15);
	hax=get(hmasterfig,'currentaxes');
	
	%get density option
	densopt=get(hdensopt,'value')-1;
	if( densopt==1 | densopt==2)
		% get the gardners coefficients
		%get a
		a=sscanf(get(ha,'string'),'%f');
		if( a<0 | isempty(a) | isnan(a) )
			set(hax,'userdata',-1);
			figure(hfig);
			set(hmsg,'string','coefficient a must be a positive real number');
			error('coefficient a must be a positive real number');
		end
	
		%get the x accelerator
		m=sscanf(get(hm,'string'),'%f');
		if( isempty(m) | m<0 | m>1 )
			set(hax,'userdata',-1);
			figure(hfig);
			set(hmsg,'string','Exponent m must be between 0 and 1');
			error('Exponent m must be between 0 and 1');
		end
	else
		a=-1;
		m=-1;
	end
	
	%get the name
	name=get(hname,'string');
	if( strcmp(name,'') | strcmp(name,' ') | length(name)>30 )
		set(hax,'userdata',-1);
		figure(hfig);
		set(hmsg,'string','Provide a non-blank name, max of 30 chars');
		error('Provide a non-blank name, max of 30 chars');
	end
	
	% get the section choices
	sonicnum=get(hsonicsec,'value');
	densitynum=get(hdensitysec,'value');
	
	%the hole algorithm
	holealg=get(hholealg,'value');
	%the theopt
	theopt=get(htheopt,'value');
	
	%store this stuff
	
	set(hax,'userdata',[sonicnum densitynum densopt holealg theopt a m abs(name)]);
	
	return;
end
%
%
%
if(strcmp(action,'setsection'))
	h=get(gcf,'userdata');
	hsection=h(3);
	hname=h(15);
	
	secnames=get(hsection,'string');
	
	secnum=get(hsection,'value');
	
	set(hname,'string',['seis__' secnames(secnum,:)]);
	
	return;
end