function [ret1,ret2,ret3]=chlogtype(action,transfer,hmasterfig,lognames)
% chlogtype('init',transfer,hmasterfig,lognames);
% [oldname,newname,newtype]=chlogtype('fini',hfig);
%
% chlogtype is called by LOGEDIT to initiate a dialog to change log types.
%
% 'init' ... the string 'init' is used to initiate the dialog
% 'fini' ... the string 'fini' terminate the dialog
% transfer ... transfer command to be called when the user pushes the delete
%	button. Usually this will be the string 'logedit(''chlogtype2'',' .
%	When a log is selected for change, chlogtype will append its name and type to 
%	this as second and third arguments and call it with eval. Thus if the log to 
%	be change hasits name stored in the string variable: name, then the eval 
%	statement will be:
%	eval([transfer name ',' newtype ');'])
% hmasterfig ... handle of the masterfigure (usually a LOGEDIT window) in
%		control of the dialog
% lognames ... list of names of logs with individual names separated by '|' 
%		such as: 'fred|sam|billy|wilma'. The first four characters of the name
% 		should be a valid menmonic indicating type.
% hfig ... handle of the change log dialog window
%
% oldname ... string giving the old name of the log to be changed
% newname ... string giving the newname (including sequential number if needed)
% newtype ... integer indicating the new log type.
% 
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
	hfig=figure('visible','off','menubar','none','numbertitle','off','name','Change Log Type Dialog');
	pos=get(hfig,'position');
	height=20;
	figheight=7*height+14;
	
	%latest type list
	[mnems,types]=logtype2las;
	
	figwidth=402;
	sep=1;
	xnow=sep;
	ynow=figheight-height-sep;
	width=figwidth - 2*sep;
	htitle=uicontrol('style','text','position',[xnow,ynow,width,height],'string',...
		'Select Log(s) for Type Change',...
		'foregroundcolor','r');
	xnow=sep;
	ynow=ynow-height-sep;
	msg='To UNDO, simply change type back to original';
	
	hmsg=uicontrol('style','text','position',[xnow,ynow,width,height],'string',...
		msg,'userdata',lognames);
	xnow=sep;
	ynow=ynow-height-sep;
	width=150;
	uicontrol('style','text','string','Log name:','position',...
		[xnow,ynow,width,height]);
	xnow=xnow+width+sep;
	width=250;
	hlog=uicontrol('style','popupmenu','string',lognames,'position',...
		[xnow,ynow,width,height],...
		'value',1,'backgroundcolor',bgkol,'foregroundcolor',fgkol,...
		'callback','chlogtype(''setlog'')','userdata',mnems);
		
	%determine current type of first log
	itype=las2logtype(lognames(1,1:4));
	ind=itype+2;
	
	xnow=sep;
	ynow=ynow-height-sep;
	width=400;
	hcurrtype=uicontrol('style','text','string',['Current type is ' types(ind,:)]...
		,'position',[xnow,ynow,width,height],'userdata',types);
	xnow=sep;
	ynow=ynow-height-sep;
	width=150;
	uicontrol('style','text','string','New type ->'...
		,'position',[xnow,ynow,width,height]);
	xnow=xnow+width+sep;
	width=250;
	hnewtype=uicontrol('style','popupmenu','string',types,'position',...
		[xnow,ynow,width,height],...
		'value',ind,'backgroundcolor',bgkol,'foregroundcolor',fgkol,...
		'callback','chlogtype(''settype'')');
		
	%the buttons
	xnow=sep;
	ynow=ynow-height-3*sep;
	width=400;
	% the change button
	chstring = ['Change ' lognames(1,:) ' to type ' ...
		strunpad(types(ind,:)) ' (name= ' lognames(1,:) ')'];
	hchange=uicontrol('style','pushbutton','string',chstring,'position',...
		[xnow,ynow,width,height],'callback','chlogtype(''change'');',...
		'userdata',transfer,'enable','on',...
		'backgroundcolor','r','foregroundcolor','y');
	xnow=sep;
	ynow=ynow-height-3*sep;
 	width=400;
	hcancel=uicontrol('style','pushbutton','string','Cancel','position',...
		[xnow,ynow,width,height],'callback','chlogtype(''cancel'');',...
		'backgroundcolor',bgkol,'foregroundcolor',fgkol);
	set(hfig,'userdata',[hmasterfig, htitle, hmsg, hlog, hcurrtype ...
		hnewtype hchange, hcancel]);
		
	%userdata:
	% hmsg ... the original log names
	% hlog ... the mnem string matrix
	% hcurrtype ... the types string matrix
	% hchange ... the transfer function
	% hcancel ... the change info: [abs(oldname) nan abs(newname) nan abs(newtype)]
				
	set(hfig,'position',[pos(1:2) figwidth figheight]);
	set(hfig,'visible','on');
	return;
end
%
% handle the change button
%
if(strcmp(action,'change'))
	h=get(gcf,'userdata');	
	hmasterfig=h(1);
% 	hmsg=h(3);
% 	hlog=h(4);
% 	hcurrtype=h(5);
% 	hnewtype=h(6);
	hchange=h(7);
	
	%call the transfer function
	transfer=get(hchange,'userdata');
	figure(hmasterfig);
	eval([transfer '1);']);
	 
	return;
end
%
% do a fini
%
if(strcmp(action,'fini'))
	hfig=transfer;
	h=get(hfig,'userdata');	
% 	hmasterfig=h(1);
% 	hmsg=h(3);
% 	hlog=h(4);
% 	hcurrtype=h(5);
% 	hnewtype=h(6);
% 	hchange=h(7);
	hcancel=h(8);
		
	%get the change info
	info=get(hcancel,'userdata');
	
	ind=find(isnan(info));
	
	ret1=char(info(1:ind(1)-1));
	ret2=char(info(ind(1)+1:ind(2)-1));
	ret3=info(ind(2)+1);
	
	return;
end
% 
% do a cancel
% 
if(strcmp(action,'cancel'))
	h=get(gcf,'userdata');
	hmasterfig=h(1);
	hchange=h(7);
	transfer=get(hchange,'userdata');
	figure(hmasterfig);
	eval([transfer '[]);']);
	
	return;
	
end
%
% handle the setlog callback
%
if(strcmp(action,'setlog'))
	h=get(gcf,'userdata');	
% 	hmasterfig=h(1);
% 	hmsg=h(3);
	hlog=h(4);
	hcurrtype=h(5);
% 	hnewtype=h(6);
% 	hchange=h(7);
		
% 	mnems=get(hlog,'userdata');
	types=get(hcurrtype,'userdata');
	
	ithis=get(hlog,'value');
	lognames=get(hlog,'string');
	%lognames=get(hmsg,'userdata');
	thisname=lognames(ithis,:);
	
	%determine current type
	itype=las2logtype(thisname(1:4));
	ind=itype+2;
	
	%change the current type string
	set(hcurrtype,'string',['Current type is ' types(ind,:)]);
	
	chlogtype('settype');
	
	return;
end
%
% handle the settype callback
%
if(strcmp(action,'settype'))
	h=get(gcf,'userdata');	
% 	hmasterfig=h(1);
% 	hmsg=h(3);
	hlog=h(4);
	hcurrtype=h(5);
	hnewtype=h(6);
	hchange=h(7);
	hcancel=h(8);
		
	mnems=get(hlog,'userdata');
	types=get(hcurrtype,'userdata');
	
	ithis=get(hlog,'value');
	lognames=get(hlog,'string');
	%lognames=get(hmsg,'userdata');
	thisname=lognames(ithis,:);
	if(length(thisname)>6); thisname=thisname(1:6); end
	%determine current type
	itype=las2logtype(thisname(1:4));
	ind=itype+2;
	
	%determine newtype
	inew=get(hnewtype,'value');
	newtype=strunpad(types(inew,:));
	
	%determine newname
	if(ind~=inew)
		[names,newname]=repnumname(lognames,thisname,mnems(inew,:));
	else
		newname=thisname;
	end
	if(length(newname)>6); newname=newname(1:6); end
	%change the change button
	set(hchange,'string',['Change ' thisname ' to ' newtype ' (Name= ' newname ')']);
	
	%store everything needed for change in hcancel
	set(hcancel,'userdata',[abs(thisname) nan abs(newname) nan inew-2]);
	
	
	return;
end