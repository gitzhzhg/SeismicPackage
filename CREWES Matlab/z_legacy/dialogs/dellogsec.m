function dellogsec(action,hmasterfig,hwellmenu)
% dellogsec(action,transfer,hmasterfig,hwellmenu)
%
% dellogsec is called by LOGSEC to initiate a dialog to delete logs
% or wells from the model.
%
% action ... should be the string 'init' when called from logedit
% hmasterfig ... handle of the masterfigure (usually a LOGEDIT window) in
%		control of the dialog
% hwellmenu ... list of handles of the well menus in the LOGSEC depth window
%
% Unlike most of my dialogs, this allows no transfer function. Instead it 
% is hardwired to make the following calls:
%	logsec2('dellog',wellname,logname) to delete a log
%	logsec2('delwell',wellname) ... to delete an entire well
%	logsec2('delwell',-1) ... if a cancel occurs
%
% G.F. Margrave Feb 1996
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
	fgkol=[0 0 0];
	bgkol=[0 1 1];
	%make a new figure
	hfig=figure('visible','off','menubar','none');
	pos=get(hfig,'position');
	height=20;
	figheight=8.5*height;
	
	figwidth=402;
	sep=1;
	xnow=sep;
	ynow=figheight-height-sep;
	width=figwidth - 2*sep;
	htitle=uicontrol('style','text','position',[xnow,ynow,width,height],'string',...
		'Select Well or Log for Deletion',...
		'foregroundcolor','k');
	xnow=sep;
	ynow=ynow-2*height-sep;
	msg=str2mat('WARNING: No UNDO is available.');
	
	hmsg=uicontrol('style','text','position',[xnow,ynow,width,2*height],'string',...
		msg,'backgroundcolor',[.702 .702 .702],'foregroundcolor','r');
		
	hwellmenu=flipud(hwellmenu);
	%get the wellnames
	for k=1:length(hwellmenu)
		name=get(hwellmenu(k),'label');
		if(k==1)
			wellnames=name;
		else
			wellnames=str2mat(wellnames,name);
		end
	end
	
	%get the lognames for the first well
	hkids=flipud(get(hwellmenu(1),'children'));
	for k=1:length(hkids)
		name=get(hkids(k),'label');
		if(k==1)
			lognames=name;
		else
			lognames=str2mat(lognames,name);
		end
	end
		
	xnow=sep;
	ynow=ynow-height-sep;
	width=.75*(figwidth-2*sep)/2;
	hwelllbl=uicontrol('style','text','string','Well selection -->','position',...
		[xnow,ynow,width,height]);
	xnow=xnow+width+sep;
	width=1.25*(figwidth-2*sep)/2;
	hwell=uicontrol('style','popupmenu','string',wellnames,'position',...
		[xnow,ynow,width,height],...
		'value',1,'backgroundcolor',bgkol,'foregroundcolor',fgkol,...
		'callback','dellogsec(''setwell'')');
		
	xnow=sep;
	ynow=ynow-height-sep;
	width=(figwidth-2*sep)/2;
	hloglbl=uicontrol('style','text','string','Logs for selected well -->','position',...
		[xnow,ynow,width,height]);
	xnow=xnow+width+sep;
	width=(figwidth-2*sep)/2;
	hlog=uicontrol('style','popupmenu','string',lognames,'position',...
		[xnow,ynow,width,height],...
		'value',1,'backgroundcolor',bgkol,'foregroundcolor',fgkol,...
		'callback','dellogsec(''setlog'')');
		
	%the buttons
	xnow=sep;
	ynow=ynow-height-sep;
	width=figwidth-2*sep;
	% the done button is enabled when the mode is switched to compute
	delstring = ['Delete WELL: ' wellnames(1,:)];
	hdelwell=uicontrol('style','pushbutton','string',delstring,'position',...
		[xnow,ynow,width,height],'callback','dellogsec(''delwell'');',...
		'userdata',hwellmenu,'enable','on',...
		'backgroundcolor','r','foregroundcolor','y');
		
	xnow=sep;
	ynow=ynow-height-sep;
	width=figwidth-2*sep;
	% the done button is enabled when the mode is switched to compute
	delstring = ['Delete LOG: ' lognames(1,:) ' for selected well '];
	hdellog=uicontrol('style','pushbutton','string',delstring,'position',...
		[xnow,ynow,width,height],'callback','dellogsec(''dellog'');',...
		'enable','on','backgroundcolor','r','foregroundcolor','y');
		
	xnow=sep;
	ynow=ynow-height-sep;
 width=figwidth-2*sep;
	hcancel=uicontrol('style','pushbutton','string','Cancel','position',...
		[xnow,ynow,width,height],'callback','dellogsec(''cancel'');',...
		'backgroundcolor',bgkol,'foregroundcolor',fgkol);
	set(hfig,'userdata',[hmasterfig, htitle, hmsg, hlog, hdelwell,...
		hdellog, hcancel, hwell]);
				
	set(hfig,'position',[pos(1:2) figwidth figheight]);
	set(hfig,'visible','on');
	return;
end
%
% handle the delete log button
%
if(strcmp(action,'dellog'))
	h=get(gcf,'userdata');	
	hmasterfig=h(1);
	hlog=h(4);
	hwell=h(8);
	
	hax=get(hmasterfig,'currentaxes');
	
	%get the name to delete log
	ilog=get(hlog,'value');
	lognames=get(hlog,'string');
	
	deadlog= strunpad(lognames(ilog,:));
	
	%well name
	wellnames=get(hwell,'string');
	iwell=get(hwell,'value');
	well=strunpad(wellnames(iwell,:));
	
	%remove leading blanks
	%ind=find(abs(well)~=32);
	%well=well(ind(1):length(well));
	%well=strunpad(well);
	%ind=find(abs(deadlog)~=32);
	%deadlog=deadlog(ind(1):length(deadlog));
	%deadlog=strunpad(deadlog);
	
	
	%call logsec
	hthisfig=gcf;
	figure(hmasterfig);
	close(hthisfig);
	eval(['logsec2(''dellog'',''' well ''',''' deadlog ''');']);
	
	return;
end
%
% handle the delete well button
%
if(strcmp(action,'delwell'))
	h=get(gcf,'userdata');	
	hmasterfig=h(1);
	hwell=h(8);
	
	hax=get(hmasterfig,'currentaxes');
	
	%well name
	wellnames=get(hwell,'string');
	iwell=get(hwell,'value');
	well=strunpad(wellnames(iwell,:));
	
	
	%call logsec
	hthisfig=gcf;
	figure(hmasterfig);
	close(hthisfig);
	eval(['logsec2(''delwell'',''' well ''');']);
	
	 
	return;
end
% 
% do a cancel
% 
if(strcmp(action,'cancel'))
	h=get(gcf,'userdata');	
	hmasterfig=h(1);
	
	hax=get(hmasterfig,'currentaxes');
	
	%call logsec
	hthisfig=gcf;
	figure(hmasterfig);
	eval(['logsec2(''delwell'',-1);']);
	
	close(hthisfig);
	 
	return;
end
%
% handle the setlog callback
%
if(strcmp(action,'setlog'))
	h=get(gcf,'userdata');	
	hmasterfig=h(1);
	hmsg=h(3);
	hlog=h(4);
	hdellog=h(6);
	
	hax=get(hmasterfig,'currentaxes');
	
	%get the name to delete log
	ilog=get(hlog,'value');
	lognames=get(hlog,'string');
	deadname= lognames(ilog,:);
	
	%change the string on the delete button
	set(hdellog,'string',['Delete LOG: ' deadname ...
		' for selected well']);
	
	return;
end
%
% handle the setwell callback
%
if(strcmp(action,'setwell'))
	h=get(gcf,'userdata');	
	hmasterfig=h(1);
	hmsg=h(3);
	hlog=h(4);
	hdelwell=h(5);
	hwell=h(8);
	
	hax=get(hmasterfig,'currentaxes');
	
	%get the name to delete well
	iwell=get(hwell,'value');
	wellnames=get(hwell,'string');
	
	deadwell=wellnames(iwell,:);
	
	%get the log names
	hwellmenu=get(hdelwell,'userdata');
	hkids=flipud(get(hwellmenu(iwell),'children'));
	for k=1:length(hkids)
		name=get(hkids(k),'label');
		if(k==1)
			lognames=name;
		else
			lognames=str2mat(lognames,name);
		end
	end
	set(hlog,'string',lognames);
	
	%change the string on the delwell button
	set(hdelwell,'string',['Delete WELL: ' deadwell]);
	
	dellogsec('setlog');
	
	return;
end