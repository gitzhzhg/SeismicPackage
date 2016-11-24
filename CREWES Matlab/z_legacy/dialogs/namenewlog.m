function namenewlog(action)
% by G.F. Margrave, November 1993
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
if(strcmp(action,'init') )
	hax=gca;
   qs=get(hax,'userdata');
   zstart=abs(qs(10,1));
   zend=abs(qs(10,2));
	  topsflag=qs(10,3);
   qs=setstr(qs(1:9,:));
	[m,n]=size(qs);
	bgkol=[0 1 1];
	if(topsflag<0)
		nrows=10;
	else
		nrows=11;
	end
        
	%build the dialog box and the questions
	hdial=figure('visible','off','menubar','none');
	pos=get(hdial,'position');
	sep=1;
	  %
	  % assume 6 chars in 50 pixels
	  %
	charpix=6;
	[r,c]=size(qs);
	q0='Please supply this information:';
	width=50*ceil(length(q0)/charpix);
	height=20;
	figheight=(height+sep)*(nrows-1);
	maxwidth=width;
	 % figwidth=2*(width+sep);
	ynow=figheight-height;
	xnow=sep;
	hmsg=uicontrol('style','text','string',q0,...
	'position',[xnow ynow width height]);
        	
	% the toggle button
	q=qs(8,:);
	ind=find(q==1);
	if(~isempty(ind))
		q=q(1:ind(1)-1);
	end
	width=50*ceil(length(q)/charpix);
	if(xnow+width>maxwidth)
		maxwidth=xnow+width;
	end
	ynow=ynow-sep-height;
	hswitch=uicontrol('style','checkbox','string',q,'position',...
		[xnow,ynow,width,height],'callback','namenewlog(''switch'')');
 
        % the log name question
		ynow=ynow-sep-height;
		q=qs(1,:);
		v1=qs(2,:);
		ind=find(q==1);
		if(~isempty(ind))
			q=q(1:ind(1)-1);
		end
		ind=find(v1==1);
		 if(~isempty(ind))
					v1=v1(1:ind(1)-1);
		 end
		width=50*ceil(length(q)/charpix);
        	
		hq1=uicontrol('style','text','string',q,'position',...
        		[xnow,ynow,width,height]);
		xnow=xnow+width+sep;
		wa=50*ceil(length(v1)/charpix);
		width=max([width wa]);
		ha1=uicontrol('style','edit','string',v1,'position',...
        		[xnow,ynow,width,height],'backgroundcolor',bgkol);
		if(xnow+width>maxwidth)
        		maxwidth=xnow+width;
		end
	%the log type popup
	ynow=ynow-sep-height;
	xnow=sep;
	htype1=uicontrol('style','text','string','New log type:',...
		'position',[xnow,ynow,width,height]);
	xnow=xnow+width+sep;
	logtypes=strmat('p wave sonic','neutron density');
	logtypes=strmat(logtypes,'formation density');
	logtypes=strmat(logtypes,'gamma ray');
	logtypes=strmat(logtypes,'sp');
	logtypes=strmat(logtypes,'caliper');
	logtypes=strmat(logtypes,'s wave sonic');
	logtypes=strmat(logtypes,'other');
	htype2=uicontrol('style','popupmenu','string',logtypes,'position',...
		[xnow,ynow,width,height],'backgroundcolor',bgkol);
	% the popup menu
	ynow=ynow-sep-height;
	xnow=sep;
	namelist=qs(9,:);
	ind=find(namelist==1);
	if(~isempty(ind))
		namelist=namelist(1:ind(1)-1);
	end
	hmenu=uicontrol('style','popupmenu','string',namelist,'position',...
		[xnow,ynow,2*width,height],'visible','off','backgroundcolor',bgkol);
	ynow=ynow+sep+height;
	% the well name question
	q=qs(3,:);
	v=qs(5,:);
	ind=find(q==1);
	if(~isempty(ind))
		q=q(1:ind(1)-1);
	end
	ind=find(v==1);
 if(~isempty(ind))
		v=v(1:ind(1)-1);
 end
	xnow=sep;
	ynow=ynow-height-sep;
	width=50*ceil(length(q)/charpix);
	hq2=uicontrol('style','text','string',q,'position',...
		[xnow,ynow,width,height]);
	xnow=xnow+width+sep;
	wa=50*ceil(length(v)/charpix);
	width=max([width wa]);
	ha2=uicontrol('style','edit','string',v,'position',...
		[xnow,ynow,width,height],'backgroundcolor',bgkol);
	if(xnow+width>maxwidth)
		maxwidth=xnow+width;
	end
	
	% the start and end depths
	xnow=sep;
	ynow=ynow-height-sep;
	q='Start depth:';
	width=50*ceil(length(q)/charpix);
	hzs1=uicontrol('style','text','string',q,'position',...
		[xnow,ynow,width,height]);
	xnow=xnow+sep+width;
	v=sprintf('%7.1f',floor(zstart));
	w=width;
	hzs2=uicontrol('style','edit','string',v,'position',...
		[xnow,ynow,w,height],'backgroundcolor',bgkol);
	ynow=ynow-height-sep;
	xnow=sep;
	q='End depth:';
	hze1=uicontrol('style','text','string',q,'position',...
		[xnow,ynow,width,height]);
	xnow=xnow+sep+width;
	v=sprintf('%7.1f',ceil(zend));
	hze2=uicontrol('style','edit','string',v,'position',...
		[xnow,ynow,w,height],'backgroundcolor',bgkol);
	
	% the well coordinate question
	q=qs(6,:);
	v=qs(7,:);
	ind=find(q==1);
	if(~isempty(ind))
		q=q(1:ind(1)-1);
	end
	ind=find(v==1);
 if(~isempty(ind))
			v=v(1:ind(1)-1);
 end
	xnow=sep;
	ynow=ynow-height-sep;
	width=1.1*50*ceil(length(q)/(charpix+3));
	hq3=uicontrol('style','text','string',q,'position',...
		[xnow,ynow,width,height]);
	xnow=xnow+width+sep;
	width=70;
	ha3=uicontrol('style','edit','string',v,'position',...
		[xnow,ynow,width,height],'backgroundcolor',bgkol);
	if(xnow+width>maxwidth)
		maxwidth=xnow+width;
	end
	%topsflag question
	if(topsflag>0)
		xnow=sep;
		ynow=ynow-height-sep;
		q='Import Tops';
		width=50*ceil((length(q))/charpix);
		het=uicontrol('style','checkbox','string',q,'value',topsflag,...
			'backgroundcolor',bgkol,'position',[xnow ynow width height]);
	else
		het=0;
	end
        
	ynow=ynow-sep-height;
	xnow=sep;
	width=70;
	hdone=uicontrol('style','pushbutton','string','Done','position',...
        	[xnow ynow width height],'callback','namenewlog(''done'')',...
        	'foregroundcolor','r');
	xnow=xnow+width+sep;
	width=70;
	hcancel=uicontrol('style','pushbutton','string','Cancel','position',...
        	[xnow ynow width height],'callback','namenewlog(''done'')');
	% get the position of the calling figure
	hparent=get(hax,'parent');
	pospar=get(hparent,'position');
	px=pospar(1)+pospar(3)/2;
	py=pospar(2)+pospar(4)/2;
	figwidth=maxwidth;
        set(hdial,'position',[px py figwidth figheight]);
        set(hdial,'visible','on');
        
        transfer=qs(4,:);
        ind=find(transfer==1);
        if(~isempty(ind))
        	transfer=transfer(1:ind(1)-1);
        end
        
         
        set(hdial,'userdata',[ha1 ha2 ha3 hmenu hq2 hq3 hax ...
					hzs2 hze2 hmsg het htype1 htype2...
        		nan abs(transfer)] );
        
        return;
        
end
if(strcmp(action,'done'))
   % get the answers and put them in gca userdata
   
   % see if the menu is visible
   dat=get(gcf,'userdata');
   hmenu=dat(4);
   hzs2=dat(8);
   hze2=dat(9);
   hmsg=dat(10);
		het=dat(11);
		htype2=dat(13);
   flag=get(hmenu,'visible');
   
   %test for cancel
   if( strcmp(get(gco,'string'),'Cancel'))
   	hax=dat(7);
		set(hax,'userdata',-1);
		close(gcf); 
		% call the transfer expression
		transfer=setstr(dat(find(isnan(dat))+1:length(dat)));  
		eval(transfer);    
		return;
	end
   	
   if( strcmp(flag,'off') )
   		ha=dat(1);
   		logname=get(ha,'string');% get the name
			if(isempty(logname))
				set(hmsg,'string','Provide a non null logname please',...
				'backgroundcolor','r','foregroundcolor','y');
				return;
			end
   		ha=dat(2);
   		wellname=get(ha,'string');% get the well name
			if(isempty(wellname))
				set(hmsg,'string','Provide a non null wellname please',...
				'backgroundcolor','r','foregroundcolor','y');
				return;
			end
			%make sure the name is different from existing wells
			wellnames=get(hmenu,'string');
			[numnames,ncols]=size(wellnames);
			for k=1:numnames
				thisname=wellnames(k,:);
				ind=find(thisname~=0);
				thisname=thisname(ind);
				if( strcmp(thisname,wellname) )
					set(hmsg,'string','Wellname must differ from existing wells',...
					'backgroundcolor','r','foregroundcolor','y');
					return;
				end
			end
   		ha=dat(3);
   		a3=get(ha,'string');% get the coordinate
			inlinecoord=sscanf(a3,'%f');
			if(isempty(inlinecoord))
				set(hmsg,'string','Provide a non null inlinecoord please',...
				'backgroundcolor','r','foregroundcolor','y');
				return;
			end
   else
   
   		ha=dat(1);
   		logname=get(ha,'string');% get the name
   		wellname=get(hmenu,'value');% an integer flag
   		hstore=dat(2);
			inlinecoord=0;
   end
		%get the logtype
		typeflag=get(htype2,'value');
		typeflag=typeflag-1;
		if(typeflag==7) typeflag=-1; end
		%see namenewlogfini.m for a descrition of the meaning of this
   
   %get the start and end depths
   str=get(hzs2,'string');
   zstart=sscanf(str,'%f');
   str=get(hze2,'string');
   zend=sscanf(str,'%f');
		if( het )
			topsflag=get(het,'value');
		else
			topsflag=-1;
		end
   
   if(isempty(zstart) | isempty(zend))
   	set(hmsg,'string','Incorrect values for start &/or end depths',...
   	'backgroundcolor','r','foregroundcolor','y');
   	return;
   end
   if(zstart>=zend)
   	set(hmsg,'string','Incorrect values for start &/or end depths',...
   	'backgroundcolor','r','foregroundcolor','y');
   	return;
   end
	hax=dat(7);
	set(hax,'userdata',[abs(logname) nan abs(wellname) nan ...
		inlinecoord zstart zend topsflag typeflag]);
   		
        
	close(gcf);
        
	% call the transfer expression
	transfer=setstr(dat(find(isnan(dat))+1:length(dat)));
        
	eval(transfer);
        
	return;
        	
end
if(strcmp(action,'switch'))
	h=get(gcf,'userdata');
	hmenu=h(4);
	hq2=h(5);
	hq3=h(6);
	ha2=h(2);
	ha3=h(3);
	
	flag=get(hmenu,'visible');
	if( strcmp(flag,'off') )
	
		set(ha2,'visible','off');
		set(ha3,'visible','off');
		set(hq2,'visible','off');
		set(hq3,'visible','off');
	
		set(hmenu,'visible','on');
	else
	
		set(ha2,'visible','on');
		set(ha3,'visible','on');
		set(hq2,'visible','on');
		set(hq3,'visible','on');
	
		set(hmenu,'visible','off');
	end
	
	return;
	
end
	