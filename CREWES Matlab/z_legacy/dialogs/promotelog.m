function promotelog(arg1,arg2,arg3,arg4,arg5,arg6)
% promotelog(transfer,hmasterfig,wellist,wellname,logname,x)
%
% PROMOTELOG is a dialog used by LOGSEC to obtain the information
% needed to promote a propagated log to a well
%
% transfer ... transfer command to be called when the user terminates
%		the dialog
% hmasterfig ... handle of the masterfigure (usually a LOGSEC window) in
%		control of the dialog
% wellist ... list of names of the existing wells in the LOGSEC model as
%			a string matrix
% wellname ... suggested name for the new well. May be ''
% logname ... suggested name for the newlog May be ''
% x ... suggested inline coordinate for the new well. May be []
%
% G.F. Margrave
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
if(nargin==1)
	action=arg1;
else
	action='init';
	transfer=arg1;
	hmasterfig=arg2;
	wellist=arg3;
	wellname=arg4;
	logname=arg5;
	x=arg6;
end
if( strcmp(action,'init') )
	hfig=figure('visible','off','menubar','none');
	pos=get(hfig,'position');
	height=20;
	sep=1;
	figheight=6*(height+sep)+sep;
	figwidth=402;
	xnow=sep;
	ynow=figheight-height-sep;
	width=400;
	htitle=uicontrol('style','text','position',[xnow,ynow,width,height],'string',...
		'Promote Log Dialog','foregroundcolor','r');
	%popupmsg='Promote to NEW well|Promote to EXISTING well';
    popupmsg='Promote to NEW well';
	ynow=ynow-height-sep;
	width=100;
	hprolbl=uicontrol('style','text','position',[xnow,ynow,width,height],...
		'string','Promote action:');
	xnow=xnow+sep+width;
	width=300-sep;
	hpromenu=uicontrol('style','popupmenu','position',[xnow,ynow,width,height],...
		'string',popupmsg,'callback','promotelog(''pro'')','backgroundcolor','c');
	ynow=ynow-height-sep;
	xnow=sep;
	width=100;
	%do the well name
	hwnamelbl=uicontrol('style','text','position',[xnow,ynow,width,height],...
		'string','Well name:');
	xnow=xnow+width+sep;
	width=300-sep;
	hwname=uicontrol('style','edit','position',[xnow,ynow,width,height],...
		'string',wellname,'backgroundcolor','c');
	% do the well list
	xnow=sep;
	width=100;
	hlistlbl=uicontrol('style','text','position',[xnow,ynow,width,height],'string',...
		'Well list:','visible','off');
	xnow=xnow+width+sep;
	width=300-sep;
	hlistmenu=uicontrol('style','popupmenu','position',[xnow,ynow,width,height],'string',...
		wellist,'visible','off','backgroundcolor','c');
	xnow=sep;
	ynow=ynow-height-sep;
	width=100;
	hlnamelbl=uicontrol('style','text','position',[xnow,ynow,width,height],'string',...
		'Name of log:','visible','on');
	xnow=xnow+width+sep;
	width=300-sep;
	hlogname=uicontrol('style','edit','position',[xnow,ynow,width,height],'string',...
		[logname],'visible','on','backgroundcolor','c');
	%inline coord of well
	xnow=sep;
	ynow=ynow-height-sep;
	width=150;
	hinlinelbl=uicontrol('style','text','position',[xnow,ynow,width,height],...
		'string','Inline coordinate:');
	xnow=xnow+width+sep;
	width=150;
	hinline=uicontrol('style','text','position',[xnow,ynow,width,height],...
		'string',num2str(x),'backgroundcolor','c');
	% now done and cancel buttons
	xnow=sep;
	ynow=ynow-height-sep;
	width=50;
	hdone=uicontrol('style','pushbutton','string','Done','position',...
				[xnow ynow width height],'callback','promotelog(''done'')',...
				'foregroundcolor','r');
	xnow=xnow+width+sep;
	width=80;
	hcancel=uicontrol('style','pushbutton','string','Cancel','position',...
				[xnow ynow width height],'callback','promotelog(''done'')');
	%save user data
	set(gcf,'userdata',[htitle hpromenu hlistlbl hlistmenu hwnamelbl ...
				hwname hlnamelbl hlogname hinlinelbl hinline hdone hcancel]);
	set(htitle,'userdata',transfer);
	set(hdone,'userdata',hmasterfig);
	%resize the figure and popup
	set(hfig,'position',[pos(1:2) figwidth figheight]);
	set(hfig,'visible','on');
	return;
end
if(strcmp(action,'pro'))
	h=get(gcf,'userdata');
	hpromenu=h(2);
	hlistlbl=h(3);
	hlist=h(4);
	hwnamelbl=h(5);
	hwname=h(6);
	hinlinelbl=h(9);
	hinline=h(10);
	prosel=get(hpromenu,'value');
	if(prosel==1)
		set(hlistlbl,'visible','off'); 
		set(hlist,'visible','off'); 
		set(hwnamelbl,'visible','on'); 
		set(hwname,'visible','on'); 
		set(hinlinelbl,'visible','on');
		set(hinline,'visible','on');
	elseif(prosel==2)
		set(hlistlbl,'visible','on'); 
		set(hlist,'visible','on'); 
		set(hwnamelbl,'visible','off'); 
		set(hwname,'visible','off'); 
		set(hinlinelbl,'visible','off');
		set(hinline,'visible','off');
	end
	return;
end
if(strcmp(action,'done'))
	h=get(gcf,'userdata');
	htitle=h(1);
	hpromenu=h(2);
	hlistlbl=h(3);
	hlist=h(4);
	hwnamelbl=h(5);
	hwname=h(6);
	hlogname=h(8);
	hinlinelbl=h(9);
	hinline=h(10);
	hdone=h(11);
	hmasterfig=get(hdone,'userdata');
	prosel=get(hpromenu,'value');
	%check for cancel
	if( gco~=hdone )
		prosel=-1;
		wname=[];
		logname=[];
		x=[];
	else
		%get the logname
		logname=get(hlogname,'string');
		%remove blanks
		ind=find(abs(logname)~=32);
		if(~isempty(ind))
			logname=logname(min(ind):max(ind));
		else
			logname=[];
		end
		if(isempty(logname))
			set(htitle,'string','Provide a non blank logname!!');
			set(htitle,'backgroundcolor','r','foregroundcolor','y');
			return;
		end
		if(prosel==1)
			wname=get(hwname,'string');
			%remove blanks
			ind=find(abs(wname)~=32);
			if(~isempty(ind))
				wname=wname(min(ind):max(ind));
			else
				wname=[];
			end
			if(isempty(wname))
				set(htitle,'string','Provide a non blank well name!!');
				set(htitle,'backgroundcolor','r','foregroundcolor','y');
				return;
			end
			%get the inline coord
			xstr=get(hinline,'string');
			x=sscanf(xstr,'%f');
			if(isempty(x))
                set(hinline,'style','edit');
				set(htitle,'string','Provide a numeric inline coordinate!!');
				set(htitle,'backgroundcolor','r','foregroundcolor','y');
				return;
			end
		elseif(prosel==2)
			wellist=get(hlist,'string');
			isel=get(hlist,'value');
			wname=wellist(isel,:);
			ind=find(wname==1);
			if(~isempty(ind))
				wname(ind)=[];
			end
			ind=find(abs(wname)~=32);
			if(~isempty(ind))
				wname=wname(min(ind):max(ind));
			else
				wname=[];
			end
			x=[];
		end
	end
	%get the transfer function
	transfer=get(htitle,'userdata');
	%store the results
	res=[prosel x nan abs(wname) nan abs(logname)];
	hax=get(hmasterfig,'currentaxes');
	set(hax,'userdata',res);
	close(gcf);
	figure(hmasterfig);
	eval(transfer);
	return;
end