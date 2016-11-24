function importlas(transfer,logmnem,logdesc,lognums,wellid,wellname,x,...
			zunits,zstart,zend,kb,topsflag,wellnamelist)
%
%
% importlasinit(transfer,lognames,lognums,wellid,wellname,x,...
%					zunits,kb,topsflag,wellnamelist)
%
% Initiate a dialog to determine what to do with logs imported from an LAS
%	file. (READLAS has already been run.)
% transfer = a string matlab command to be evaluated with EVAL at the 
%            completion of the dialog. Usually this will re-invoke the 
%            calling program
%  logmnem = string matrix of the log mnemonics found in the LAS file. 
%            (The second return value from read LAS excluding the first row)
%  logdesc = string matrix of the log descriptions found in the LAS file. 
%	     (The third return value from read LAS excluding the first row)
%  lognums = vector of integers indicating the default choice for which logs
%            to use
%   wellid = string containing the unique well id
% wellname = string containing the well name
%        x = the default inline coordinate. Set to nan to indicate no default
%   zunits = string containing the depth units (for display only)
%   zstart = starting log depth
%     zend = ending log depth
%       kb = the kelly bushing elevation (for display only)
% topsflag = default setting for whether to use tops or not
%            -1 ... there are no tops, option not shown
%             0 ... tops present, default is don't use
%             1 ... tops present, default is use
% wellnamelist = vector of names. If present, a toggle button will be present
%		allowing the new log(s) to be associated with an existing 
%               well. If selected the namefield and inline coordinate fields
%               will disappear to be replaced by a popup menu allowing the 
%               selection of one of the names in the vector.
%               Namelist may be in the format returned by 
%               objget(object,'fieldnames') 
%		or that returned by objget(object,'namesmatrix') 
%
% G.F. Margrave May 1994, modified by Chad Hogan 2004
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

% $Id: importlas.m,v 1.5 2009/07/24 19:50:58 gary Exp $

	if( nargin < 10)
		wellnameslist=[];
	end

	if( nargin==1 )
		action=transfer;
	else
		action='init';
	end

if(strcmp(action,'init') )
	hax=gca; % get the handle of current axes in calling figure

if(topsflag>0)
	nrows=13;
else
	nrows=12;
end

if(isempty(wellnamelist))
	nrows=nrows-1;
end
        
%build the dialog box and the questions
 hdial=figure('visible','off','menu','none');
 pos=get(hdial,'position');
 %
 %
 figwidth=400;
 figheight=300;
 bgkol=[0 1 1];
 grey=[.702 .702 .702];

 sep=.005;
 height=(1/nrows)-sep;
 width=1;
 ynow=1-height;
 xnow=sep;
 hmsg=uicontrol('style','text','string','Import LAS logs dialog','units','normalized',...
 	'position',[xnow ynow width height],'foregroundcolor','r','userdata',hax);

% a toggle button
ynow=ynow-sep-height;
htoggle=uicontrol('style','checkbox','string','Use log descriptors as names',...
		'value',0,'callback','importlas(''lognames'')','units','normalized',...
		'position',[xnow,ynow,width,height]);
        	
% the log name question
 %first modify the lognames
 [nlogs,m]=size(logmnem);
 logmnem=[32*ones(nlogs,3) logmnem];
 logdesc=[32*ones(nlogs,3) logdesc];
 impstr=[];
 for k=1:nlogs
		n=int2str(k);
		logmnem(k,1:length(n))=n;
		logdesc(k,1:length(n))=n;
		impstr=[impstr ' ' n];
	end
	ynow=ynow-sep-height;
	width=.3;
		
	hlogname1=uicontrol('style','text','string','Log names:','units','normalized',...
			'position',[xnow,ynow,width,height]);

	xnow=xnow+width+2*sep;
	width=1-xnow-sep;
	hlogname2=uicontrol('style','popupmenu','string',logmnem,'units','normalized',...
			'position',[xnow,ynow+sep,width,height],'callback',...
			'importlas(''setlogname'')','userdata',logdesc,'backgroundcolor',bgkol);

	%log name to edit
	xnow=sep;
	width=.3;
	ynow=ynow-height-sep;
	hlogedit1=uicontrol('style','text','string','Edit log name:','units',...
		'normalized','position',[xnow,ynow,width,height]);

	xnow=xnow+width+sep;
	width=1-xnow-sep;
	hlogedit2=uicontrol('style','edit','string',logmnem(1,4:7),'units','normalized',...
		'position',[xnow,ynow,width,height],'userdata',1);


	% log #'s to import
	xnow=sep;
	width=.4;
	ynow=ynow-height-sep;
	hlogimp1=uicontrol('style','text','string','Log numbers to import:',...
		'units','normalized','position',[xnow,ynow,width,height]);

	xnow=xnow+width+sep;
	width=1-xnow-sep;
	hlogimp2=uicontrol('style','edit','string',impstr,'units','normalized',...
		'position',[xnow,ynow,width,height],'backgroundcolor',bgkol);

	% well name options
	xnow=sep;
	width=.3;
	ynow=ynow-height-sep;
	hwellopt1=uicontrol('style','text','string','Well name options:',...
		'units','normalized','position',[xnow,ynow,width,height]);

	xnow=xnow+width+2*sep;
	width=1-xnow-sep;
	if(isempty(wellnamelist))
		str=[char(wellname) '|' wellid];
	else
		str=[char(wellname) '|' wellid '|Existing Well'];
	end
	hwellopt2=uicontrol('style','popupmenu','string',str,'units',...
			'normalized','position',[xnow,ynow+sep,width,height],...
			'callback','importlas(''wellname'')');

% the well name
	xnow=sep;
	width=.3;
	ynow=ynow-height-sep;
	hwellname1=uicontrol('style','text','string','Well name:',...
		'units','normalized','position',[xnow,ynow,width,height]);

	xnow=xnow+width+sep;
	width=1-xnow-sep;
	hwellname2=uicontrol('style','edit','string',wellname,'units','normalized',...
		'position',[xnow,ynow,width,height],'backgroundcolor',bgkol);

	if(~isempty(wellnamelist))
		% the existing wells popup 
		xnow=sep;
		width=.4;
		hexist1=uicontrol('style','text','string','Existing Well Names:',...
			'units','normalized','position',[xnow,ynow,width,height],'visible','off');

		xnow=xnow+width+2*sep;
		width=1-xnow-sep;
		hexist2=uicontrol('style','popupmenu','string',wellnamelist,'units',...
			'normalized','position',[xnow,ynow+sep,width,height],'visible',...
			'off','backgroundcolor',bgkol);
	else
		hexist1=0;
		hexist2=0;
	end

	%inline coordinate question
	xnow=sep;
	width=.5;
	ynow=ynow-height-sep;
	hinline1=uicontrol('style','text','string','Inline coordinate:',...
		'units','normalized','position',[xnow,ynow,width,height]);

	xnow=xnow+width+sep;
	width=1-xnow-sep;
	if(isnan(x))
		xstr='';
	else
		xstr=num2str(x);
	end
	hinline2=uicontrol('style','edit','string',xstr,'units','normalized','position',...
		[xnow,ynow,width,height],'backgroundcolor',bgkol);

	%start and end depths
	xnow=sep;
	width=.23;
	ynow=ynow-height-sep;
	hzstart1=uicontrol('style','text','string','Start depth:',...
		'units','normalized','position',[xnow,ynow,width,height]);

	xnow=xnow+width+sep;
	width=.5-xnow-sep;
	hzstart2=uicontrol('style','edit','string',num2str(zstart),'units','normalized',...
		'position',[xnow,ynow,width,height],'backgroundcolor',bgkol);

	xnow=.5+sep;
	width=.23;
	hzend1=uicontrol('style','text','string','End depth:',...
		'units','normalized','position',[xnow,ynow,width,height]);

	xnow=xnow+width+sep;
	width=1-xnow-sep;
	hzend2=uicontrol('style','edit','string',num2str(zend),'units','normalized',...
		'position',[xnow,ynow,width,height],'backgroundcolor',bgkol);

	% depth units and kb elevation
	ynow=ynow-height-sep;
	xnow=sep;
	width=.5;
	hunits=uicontrol('style','text','string',['Depth units: ' zunits],...
		'units','normalized','position',[xnow,ynow,width,height]);

    % 	xnow=xnow+width+sep;
    % 	width=1-xnow-sep;
    % 	hkb=uicontrol('style','text','string',['KB elevation: ' num2str(kb)],...
    % 		'units','normalized','position',[xnow,ynow,width,height]);

    % I added this to allow the KB elevation to be editable. LOGSEC gets
    % really confused if there is no KB at all, and it seems that too many
    % LAS file lack a KB -- and those that have one can't agree on where to
    % put it. 
    % 
    % Chad Hogan, April 2004.
    
    xnow=.5+sep;
    width=.23;
    hkbtext=uicontrol('style','text','string','KB elevation: ',...
        'units','normalized','position',[xnow,ynow,width,height]);

	xnow=xnow+width+sep;
	width=1-xnow-sep;
	hkb=uicontrol('style','edit','string',num2str(kb),'units','normalized',...
		'position',[xnow,ynow,width,height],'backgroundcolor',bgkol);
    
    
	% use tops checkbox
	if( topsflag>0)
		ynow=ynow-height-sep;
		width=.5;
		xnow=sep;
		if(topsflag)
			val=1;
			kol=bgkol;
		else
			val=0;
			kol=grey;
		end
		htops=uicontrol('style','checkbox','string','Import tops','value',val,...
			'units','normalized','position',[xnow,ynow,width,height],...
			'backgroundcolor',kol,'callback','importlas(''tops'')');
	else
		htops=0;
	end


%done and cancel buttons
	ynow=ynow-sep-height;
	xnow=sep;
	width=.3;
	hdone=uicontrol('style','pushbutton','string','Done','units','normalized',...
		'position',[xnow ynow width height],'callback','importlas(''done'')',...
		'foregroundcolor','r');
	xnow=xnow+width+sep;
	width=.3;
	hcancel=uicontrol('style','pushbutton','string','Cancel','units','normalized',...
	'position',[xnow ynow width height],'callback','importlas(''done'')');
	


	% get the position of the calling figure
	hparent=get(hax,'parent');
	pospar=get(hparent,'position');

	px=pospar(1)+pospar(3)/2;
	py=pospar(2)+pospar(4)/2;

	set(hdial,'position',[px py figwidth figheight]);
 	set(hdial,'visible','on');
 	
 	set(hdone,'userdata',transfer);
        
	set(hdial,'userdata',[hmsg htoggle hlogname1 hlogname2  hlogedit1 hlogedit2...
		hlogimp1 hlogimp2 hwellopt1 hwellopt2 hwellname1 hwellname2 ...
		hexist1 hexist2 hinline1 hinline2 hzstart1 hzstart2 ...
		hzend1 hzend2 hunits hkb htops hdone ...
		hcancel]);
        
 return;
        
end

if(strcmp(action,'done'))
   % here we must:
   % 1) get the log munbers to import
   % 2) for each log, get its edited name
   % 3) get the well name
   % 4) get the depth interval
   % 5) get the x coordinate
   % 6) get the use tops flag
   
   h=get(gcf,'userdata');
   hmsg=h(1);
   hlogname2=h(4);
   hlogimp2=h(8);
	  hwellopt2=h(10);
   hwellname2=h(12);
	  hexist2=h(14);
   hinline2=h(16);
   hzstart2=h(18);
   hzend2=h(20);
   htops=h(23);
   hdone=h(24);
	  hkb=h(22);
	  hunits=h(21);
   
   flag=get(gco,'string');
   if( strcmp(flag,'Cancel') )
   	hax=get(hmsg,'userdata');
		set(hax,'userdata',-1);
   	% call the transfer expression
		transfer=get(hdone,'userdata');
		close(gcf);
		eval(transfer);
		return;
	  end
   
   %save the edited name
		importlas('setlogname');
   
   % get the lognames
   lognames=get(hlogname2,'string');
   [nlogs,nc]=size(lognames);
   
   %get the log numbers to import
   str=get(hlogimp2,'string');
   lognums=sscanf(str,'%d');
   
   test=find(diff(lognums)==0);
   
   if(isempty(lognums) | max(lognums)> nlogs | min(lognums)<1 | ~isempty(test))
   	set(hmsg,'string','Log numbers are not sensible!');
		set(hmsg,'backgroundcolor','r','foregroundcolor','y');
   	return;
   end
   
   lognames=lognames(:,4:nc);
   
   %get the well name
	  test=get(hwellopt2,'value');
	  if(test~=3)
		wellname=get(hwellname2,'string');
		ind=find(abs(wellname)~=32);
		if( isempty(wellname(ind)) )
			set(hmsg,'string','Well name must be non-blank');
			set(hmsg,'backgroundcolor','r','foregroundcolor','y');
			return;
		end
	  else
		wellnum=get(hexist2,'value');
		wellist=get(hexist2,'string');
		wellname=strunpad(wellist(wellnum,:));
	  end
	  %if the name was typed in, then we make sure it differs from
	  %existing wells
	  if(test~=3 & hexist2)
		wellnames=get(hexist2,'string');
		[numnames,ncols]=size(wellnames);
		for k=1:numnames
			thisname=wellnames(k,:);
			ind=find(thisname~=0);
			thisname=thisname(ind);
			if(strcmp(thisname,wellname))
				set(hmsg,'string','Wellname must differ from existing wells');
				set(hmsg,'backgroundcolor','r','foregroundcolor','y');
				return;
			end
		end
	  end
   
   %get the depth interval
   str=get(hzstart2,'string');
   zstart=sscanf(str,'%f');
   str=get(hzend2,'string');
   zend=sscanf(str,'%f');
   
   if(isempty(zstart) | isempty(zend) | zend<=zstart)
   	set(hmsg,'string','Depth interval is mis-specified');
		set(hmsg,'backgroundcolor','r','foregroundcolor','y');
   	return;
   end
   
   %get the inline coordinate
   if( strcmp(get(hinline2,'visible'),'on'))
   	str=get(hinline2,'string');
   	x=sscanf(str,'%f');
   	
   	if(isempty(x))
   		set(hmsg,'string','Specify the inline coordinate!');
        set(hmsg,'backgroundcolor','r','foregroundcolor','y');
        return;
    end
   else
   	x=inf;
   end
   
   %get the topsflag
   if(htops)
   	topsflag=get(htops,'value');
   else
   	topsflag=htops;
   end
   
	hax=get(hmsg,'userdata');

    % get the kb and the units
    str=get(hkb,'string');
    kb=sscanf(str,'%*s%*s%f');
    if(isempty(kb)) kb=str2num(str);end  % If that failed, try another way.
    % if it still didn't work, kick back to the user.
    if(isempty(kb))
        set(hmsg, 'string', 'Set a valid KB elevation!');
        set(hmsg, 'backgroundcolor', 'r', 'foregroundcolor', 'y');
        return;
    end
    units=get(hunits,'string');
    
	
	[m,n]=size(lognames);
	lognames=lognames(:)';
	set(hax,'userdata',[lognums' nan m n abs(lognames) nan abs(wellname)...
		 nan double(units) nan x zstart zend topsflag kb]);
   		        
 % call the transfer expression
	transfer=get(hdone,'userdata');
	%******NOTE*********
	% If I delete the las dialog figure prior to calling the transfer function
	% I tend to get matlab segmentation faults. This should work because I 
	% truly have no further use for the figure; however, I've found it necessary
	% to hide the figure, make the calling figure current, call the transfer
	% function, and then delete the hidden dialog figure. So the next few
	% lines should work but are commented out:
	% Should work:
	%close(gcf);
	%eval(transfer);
	hfig=gcf;
	set(hfig,'visible','off');
	hmasterfig=get(hax,'parent');
	figure(hmasterfig);
	eval(transfer);
	close(hfig);
        
	return;
        	
end

	
if(strcmp(action,'lognames') )
	h=get(gcf,'userdata');
	hlogname2=h(4);
	hlogedit2=h(6);

	%save the edited name
	importlas('setlogname');

	%swap userdata and string in hlognames
	tmp=get(hlogname2,'userdata');

	set(hlogname2,'userdata',get(hlogname2,'string'));
	set(hlogname2,'string',tmp);

	%set the string in heditlog2
	[nr,nc]=size(tmp);
	set(hlogedit2,'string',tmp(1,4:nc));
	set(hlogedit2,'userdata',1);

	return;

end

% save an edited log name
if(strcmp(action,'setlogname'))
	h=get(gcf,'userdata');
	hlogname2=h(4);
	hlogedit2=h(6);

	n=get(hlogedit2,'userdata');
	name=get(hlogedit2,'string');

	%remove any blanks
	ind=find(abs(name)~=32);
	name=name(min(ind):max(ind));

	lognames=get(hlogname2,'string');
	[nr,nc]=size(lognames);

	newnames=lognames(1:n-1,:);
	newnames=strmat(newnames,[lognames(n,1:3) name]);
	newnames=strmat(newnames,lognames(n+1:nr,:));
	[nr,nc]=size(newnames);

	val=get(hlogname2,'value');

	set(hlogname2,'string',newnames,'value',val);
	set(hlogedit2,'string',newnames(val,4:nc));
	set(hlogedit2,'userdata',val);

	return;
end

if(strcmp(action,'wellname'))
	h=get(gcf,'userdata');
	hwellopt2=h(10);
	hwellname1=h(11);
	hwellname2=h(12);
	hexist1=h(13);
	hexist2=h(14);
	hinline1=h(15);
	hinline2=h(16);

	val=get(hwellopt2,'value');
	if( val<3 )
		set(hwellname1,'visible','on');
		set(hwellname2,'visible','on');
		if(hexist1)
			set(hexist1,'visible','off');
			set(hexist2,'visible','off');
		end
		set(hinline1,'visible','on');
		set(hinline2,'visible','on');

		names=get(hwellopt2,'string');
		set( hwellname2,'string',names(val,:));

	else
		set(hwellname1,'visible','off');
		set(hwellname2,'visible','off');
		set(hexist1,'visible','on');
		set(hexist2,'visible','on');
		set(hinline1,'visible','off');
		set(hinline2,'visible','off');
	end

	return;

end

if(strcmp(action,'tops'))
	htop=gco;
 bgkol=[0 1 1];
 grey=[.702 .702 .702];

	val=get(htop,'value');

	if(val==1)
		set(gco,'backgroundcolor',bgkol);
	else
		set(gco,'backgroundcolor',grey);
	end

	return;
end