function slicemaster(arg1,arg2,arg3)
% SLICEMASTER launches slicetool children from a container object of 
% slice container objects
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
%userdata assignments
%
%	figure user data: 
%	[htitle hslicelabel hslice hlaunch hsaveall hquit hsavefile...
%		hsaveas]);
%
% htitle ... the master slice object (a container of slice containers
% hslicelabel ... not used
% hslice ... not used
% hlaunch ... the handles of the child windows and the numbers of the slices
%		displayed in each. Matrix size is [2,nchildren] where row 1 has the window
%		handles and row 2 has the child number.
% hsaveall ... flag telling if a saveall is in progress
% hquit ... not used
% hsavefile ... not used
% hsaveas ... not used
%
if(isstr(arg1))
	action=arg1;
else
	object=arg1;
	action='init';
end
if(strcmp(action,'init'))
	% make a new figure
	hfig=figure('visible','off','menubar','none');
	pos=get(hfig,'position');
	figheight=100;
	figwidth=302;
	pos=[pos(1:2) figwidth figheight];
	sep=1;
	height=20;
	width=figwidth;
	xnow=sep;
	ynow=figheight-sep-height;
	htitle=uicontrol('style','text','string','Slice Master','position',...
		[xnow,ynow,width,height]);
		
	%get the name of the current save file
	fileobj=objget(object,'file');
	if(isempty(fileobj))
		fileobj=contobj('file','prvt');
		fileobj=objset(fileobj,'filename','undefined');
		fileobj=objset(fileobj,'pathname',' ');
		object=objset(object,'file',fileobj);
	end
	filename=objget(fileobj,'filename');
	pathname=objget(fileobj,'pathname');
	fullfilename=[pathname filename];
	
	% assume 6 chars in 50 pixels, then we need 8 for a label and therefore can have
	% a fullfilename no longer than
	nchars=round( (figwidth*6/50)-8 );
	n=length(fullfilename);
	if (n>nchars)
		label=['Savefile:...' fullfilename(n-nchars+3:n)];
	else
		label=['Savefile:' fullfilename];
	end
	
	ynow=ynow-height-sep;
	hsavefile=uicontrol('style','text','string',label,'position',...
		[xnow,ynow,width,height]);
	xnow=sep;
	ynow=ynow-height-sep;
	width=120;
	hslicelabel=uicontrol('style','text','string','Slice to launch->','position',...
		[xnow,ynow,width,height]);
	xnow=xnow+width+sep;
	width=figwidth-width-3*sep;
	slicenames=objget(object,'fieldnames','slce');
	if( ~isempty(slicenames) )
	  hslice=uicontrol('style','popupmenu','string',slicenames,'position',...
		[xnow,ynow,width,height]);
	else
	  hslice=[];
	end
 xnow=sep;
 ynow=ynow-height-sep;
 width=60;
 hlaunch=uicontrol('style','pushbutton','string','Launch','position',...
		[xnow,ynow,width,height],'callback','slicemaster(''launch'')');
		
xnow=xnow+width+sep;
width=60;
hsaveall=uicontrol('style','pushbutton','string','Save','position',...
	[xnow,ynow,width,height],'callback','slicemaster(''saveall'')');
	
xnow=xnow+width+sep;
hsaveas=uicontrol('style','pushbutton','string','Save As','position',...
	[xnow,ynow,width,height],'callback','slicemaster(''saveallas'')');
	
xnow=xnow+width+sep;
width=60;
	
hquit=uicontrol('style','pushbutton','string','Quit','position',...
	[xnow,ynow,width,height],'callback','slicemaster(''quit'')');
	set(htitle,'userdata',object);
	set(gcf,'userdata',[htitle hslicelabel hslice hlaunch hsaveall hquit hsavefile...
		hsaveas]);
	set(gcf,'position',pos);
	set(gca,'visible','off');
	set(gcf,'visible','on');
	return;
end
% launch a slicetool
if( strcmp(action,'launch') )
	hfig=gcf;
	h=get(hfig,'userdata');
	htitle=h(1);
	hslice=h(3);
	hlaunch=h(4);
	slicenum=get(hslice,'value');
	object=get(htitle,'userdata');
	%slice=objget(object,slicenum,'slce');
	slicetool(object,hfig);
	%the current figure is now the slicetool window, get its handle
	hfig=gcf;
	childinfo=get(hlaunch,'userdata');
	set(hlaunch,'userdata',[childinfo [hfig;slicenum] ]);
	
	set(htitle,'string','Slice Master');
	return;
end
% save a slice
if( strcmp(action,'saveslice') )
	hfig=arg3;
	hchildfig=gcf;
	sliceobj=arg2;
	
	h=get(hfig,'userdata');
	htitle=h(1);
	hlaunch=h(4);
	hsaveall=h(5);
	masterobj=get(htitle,'userdata');
	childinfo=get(hlaunch,'userdata');
	
	ind=find(childinfo(1,:)==hchildfig);
	slicenum=childinfo(2,ind);
	
	% put the slice in the master object
	masterobj=objset(masterobj,slicenum,sliceobj,'slce');
	
	%put the master object back in htitle userdata
	set(htitle,'userdata',masterobj);
	
	% see if we are in save-all mode
	flag=get(hsaveall,'userdata');
	
	if(flag)
		return;
	else
		% set the SLICEMASTER window as current fig
		figure(hfig);
		slicemaster('savetodisk');
	end
	
	return;
end
if(strcmp(action,'savetodisk') | strcmp(action,'savetodiskas'))
	hfig=gcf;
	
	h=get(hfig,'userdata');
	htitle=h(1);
	hlaunch=h(4);
	hsaveall=h(5);
	hsavefile=h(7);
	masterobj=get(htitle,'userdata');
	
	% see if we need a file dialog box
	fileobj=objget(masterobj,'file');
	
	filename=objget(fileobj,'filename');
	pathname=objget(fileobj,'pathname');
	
	if( strcmp(filename,'undefined') | strcmp(action,'savetodiskas'))
		
		%put up the dialog box
		% get the output file name
		pos=get(hfig,'position');
		%xpopup=pos(1)+pos(3)/2;
		%ypopup=pos(2)+pos(4);
		[filename,path]=uiputfile('*.mat','Output File Selection');
		if( isempty(filename) )
			set(htitle,'string','Slice Master: Output aborted: no file name given');
			return;
		end
		if( filename==0 )
			set(htitle,'string','Slice Master: Output aborted');
			return;
		end
		
		%update the fileobj
		fileobj=objset(fileobj,'filename',filename);
		fileobj=objset(fileobj,'pathname',pathname);
		
		masterobj=objset(masterobj,'file',fileobj);
		set(htitle,'userdata',masterobj);
		
		% update the savefile display
		% assume 7 chars in 50 pixels, then we need 8 for a label and therefore can have
		% a fullfilename no longer than
		fullfilename = [path filename]; figwidth=302;
		nchars=round( (figwidth*7/50)-8 );
		n=length(fullfilename);
		if (n>nchars)
			label=['Savefile:...' fullfilename(n-nchars+3:n)];
		else
			label=['Savefile:' fullfilename];
		end
		set(hsavefile,'string',label)
	end
	ind = findstr(filename,'.mat');
	if( length(ind)>0 ) filename=filename(1:ind-1); end
	%ind = findstr(filename,'.dat');
	%if( length(ind)>0 ) filename=filename(1:ind-1); end
	fullfilename = [path filename];
		
	%copy the object into a variable whose name is the filename
	eval([filename '=masterobj;']);
	% ok, now write it out
	if( strcmp(computer,'MAC2') )
		eval(['save ' filename ' ' filename]);
	else
		eval(['save ' fullfilename ' ' filename]);
	end
	
	str=get(htitle,'string');
	i1=findstr(str,'Again');
	i2=findstr(str,'Successful');
	if(isempty(i2) | (~isempty(i2) & ~isempty(i1)))
		set(htitle,'string','Slice Master: Output Successful');
	else
		set(htitle,'string','Slice Master: Output Successful Again');
	end
	
	return;
	
end
%
% The SAVEALL action causes SLICEMASTER to tell each of its child SLICETOOLS to
% to save themselves
%
if(strcmp(action,'saveall') | strcmp(action,'saveallas'))
	hfig=gcf;
	h=get(hfig,'userdata');
	htitle=h(1);
	hlaunch=h(4);
	hsaveall=h(5);
	masterobj=get(htitle,'userdata');
	childinfo=get(hlaunch,'userdata');
	newchildinfo=childinfo;
	
	[r,numkids]=size(childinfo);
	
	%get the active figure
	fignos=figs;
	
	% set the saveall flag
	set(hsaveall,'userdata',1);
	
	%loop over numkids
	for k=1:numkids
		hkidfig=childinfo(1,k);
		% see if the figure is still active
		ind=find(fignos==hkidfig);
		
		if(~isempty(ind))
			%see if it is a Slicetool figure
			h=get(hkidfig,'userdata');
			go=1;
			if( length(h) < 26 )
				go=0;
			else
				%make sure h(2) is a child of the figure and h(26) is a child of
				%h(2)
				hfigkids=get(hkidfig,'children');
				ind=find(hfigkids==h(2));
				if(isempty(ind))
					go=0;
				else
					hkids=get(h(2),'children');
					ind=find(hkids==h(26));
					if(isempty(ind))
						go=0;
					else
						test=get(h(26),'userdata');
						if( ~strcmp(test,'slicetool') )
							go=0;
						end
					end
				end
			end
		else
			go=0;
		end	
		if(go)
			
			% make the slicetool figure current
			figure(hkidfig);
			
			% send the save message
			slicetool('save');
			
		else
			%delete the entry from childinfo for next time
			ind=find(newchildinfo(1,:)==hkidfig);
			newchildinfo(:,ind)=[];
		end
		
	end
	
	%ok, each child has been loaded into the master object,
	% Set the slicemaster figure current and savetodisk
	
	figure(hfig);
	if( strcmp(action,'saveallas'))
		slicemaster('savetodiskas');
	else
		slicemaster('savetodisk');
	end
	
	set(hsaveall,'userdata',0);
	
	return;
	
end
if(strcmp(action,'quit')|strcmp(action,'quit2'))
	if(strcmp(action,'quit'))
		%put up a save first dialog
		yesnoinit('slicemaster(''quit2'')','Save changes first?');
		return;
	elseif(strcmp(action,'quit2'))
		h=get(gcf,'userdata');
		hlaunch=h(4);
		reply=yesnofini;
		
		if(reply==1)
			slicemaster('saveall');
		end
		%kill the kids
		kidinfo=get(hlaunch,'userdata');
		kids=kidinfo(1,:);
		for k=1:length(kids)
			figures=figs;
			ind=find(figures==kids(k));
			if(~isempty(ind))
				hkidfig=kids(k);
				%see if it is a Slicetool figure
				h=get(hkidfig,'userdata');
				go=1;
				if( length(h) < 26 )
					go=0;
				else
					%make sure h(2) is a child of the figure and h(26) is a child of
					%h(2)
					hfigkids=get(hkidfig,'children');
					ind=find(hfigkids==h(2));
					if(isempty(ind))
						go=0;
					else
						hkids=get(h(2),'children');
						ind=find(hkids==h(26));
						if(isempty(ind))
							go=0;
						else
							test=get(h(26),'userdata');
							if( ~strcmp(test,'slicetool') )
								go=0;
							end
						end
					end
				end
			else
				go=0;
			end	
			if(go)
				close(kids(k));
			end
			
		end
		
		%commit suicide
		close(gcf);
	end
end