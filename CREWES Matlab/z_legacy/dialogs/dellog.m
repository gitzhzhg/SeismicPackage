function dellog(action,transfer,hmasterfig,lognames)
% dellog(action,transfer,hmasterfig,lognames)
%
% DELLOG is called by LOGEDIT to initiate a dialog to delete logs from
% the active LAS file
%
% action ... should be the string 'init' when called from logedit
% transfer ... transfer command to be called when the user pushes the delete
%              button. Usually this will be the string 'logedit(''dellog2'','.
%              When a log is selected for deletion, dellog will append its 
%              name to this as a second argument and call is with eval.
%              Thus if the log to be deleted has its name stored in the
%              string variable: name, then the eval statement will be:
%	eval([transfer name ');'])
% hmasterfig ... handle of the masterfigure (usually a LOGEDIT window) in
%		control of the dialog
% lognames ... list of names of logs with individual names separated by '|' 
%		such as: 'fred|sam|billy|wilma'
%
% G.F. Margrave Jan 1996
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
	hfig=figure('visible','off','menubar','none','numbertitle','off','name','Delete Log Dialog');
	pos=get(hfig,'position');
	height=20;
	figheight=5.5*height;
	
	figwidth=302;
	sep=1;
	xnow=sep;
	ynow=figheight-height-sep;
	width=figwidth - 2*sep;
	htitle=uicontrol('style','text','position',[xnow,ynow,width,height],'string',...
		'Select Log(s) for Deletion',...
		'foregroundcolor','r');
	xnow=sep;
	ynow=ynow-2*height-sep;
	msg='WARNING: No UNDO is available. Be sure before pushing delete button.';
	
	hmsg=uicontrol('style','text','position',[xnow,ynow,width,2*height],'string',...
		msg);
	xnow=sep;
	ynow=ynow-height-sep;
	width=150;
	hloglbl=uicontrol('style','text','string','Log to be deleted -->','position',...
		[xnow,ynow,width,height]);
	xnow=xnow+width+sep;
	width=150;
	hlog=uicontrol('style','popupmenu','string',lognames,'position',...
		[xnow,ynow,width,height],...
		'value',1,'backgroundcolor',bgkol,'foregroundcolor',fgkol,...
		'callback','dellog(''setlog'')');
		
	%the buttons
	xnow=sep;
	ynow=ynow-height-sep-sep-sep;
	width=150;
	% the done button is enabled when the mode is switched to compute
	iii=find(lognames=='|');
	if(~isempty(iii))
	    delstring = ['Delete ' lognames(1:iii(1)-1)];
	else
	    delstring = ['Delete ' lognames(1,:)];
	end
	hdelete=uicontrol('style','pushbutton','string',delstring,'position',...
		[xnow,ynow,width,height],'callback','dellog(''delete'');',...
		'userdata',transfer,'enable','on',...
		'backgroundcolor','r','foregroundcolor','y');
	xnow=xnow+width+sep;
 	width=150;
	hcancel=uicontrol('style','pushbutton','string','Done','position',...
		[xnow,ynow,width,height],'callback','dellog(''cancel'');',...
		'backgroundcolor',bgkol,'foregroundcolor',fgkol);
	set(hfig,'userdata',[hmasterfig, htitle, hmsg, hlog, hdelete, hcancel]);
				
	set(hfig,'position',[pos(1:2) figwidth figheight]);
	set(hfig,'visible','on');
	return;
end
%
% handle the delete button
%
if(strcmp(action,'delete'))
	h=get(gcf,'userdata');	
	hmasterfig=h(1);
	hmsg=h(3);
	hlog=h(4);
	hdelete=h(5);
	
	hax=get(hmasterfig,'currentaxes');
	
	%get the name to delete log
	ilog=get(hlog,'value');
	lognames=get(hlog,'string');
	
	deadname= strunpad(lognames(ilog,:));
	
	
	%call the transfer function
	transfer=get(hdelete,'userdata');
	figure(hmasterfig);
	eval([transfer '''' deadname '''' ');']);
	
	set(hmsg,'string',['Log ' deadname ' toasted']);
	
	lognames(ilog,:)=[];
	set(hlog,'string',lognames,'value',size(lognames,1));
	dellog('setlog');
	 
	return;
end
% 
% do a cancel
% 
if(strcmp(action,'cancel'))
	h=get(gcf,'userdata');
	hmasterfig=h(1);
	hdelete=h(5);
	transfer=get(hdelete,'userdata');
	figure(hmasterfig);
	eval([transfer '[]);']);
	
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
	hdelete=h(5);
	
	hax=get(hmasterfig,'currentaxes');
	
	%get the name to delete log
	ilog=get(hlog,'value');
	lognames=get(hlog,'string');
	
	if( ~isempty(lognames) )
		deadname= lognames(ilog,:);
	else
		dellog('cancel');
		return;
	end
	
	%change the string on the delete button
	set(hdelete,'string',['Delete ' deadname]);
	
	return;
end