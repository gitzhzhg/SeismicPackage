function mapview(arg1,arg2,arg3,arg4,arg5)
% mapview(x,y,z,xr,yr)
% mapview(x,y,z)
% mapview(obj)
% mapview(obj,xr,yr)
%
% function creates a color map of gridded data using the Matlab
% routine pcolor but sets shading to flat. The ability to superinmpose
% a set of arbitrary, randomly placed points is also provided.
%
% 	 x = a grid or vector of x coordinates. if a vector, then its
%	     length must be the same as the number of columns in z.
%	 y = a grid or vector of y coordinates. If a vector, then its 
%	     length must be the same as the number of rows of z.
%	 z = the grid of z values. This is the data of the "map".
%	xr = a vector of x coordinates of randomly distributed points to
%	     be plotted on top of the map.
%	yr = a vector of y coordinates of randomly distributed points to
%            be plotted on top of the map. yr and xr must be the same length.
%
% G.F. Margrave, October, 1993 
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
% parse the input arguments
%
	obj=-999;x=-999;y=-999;z=-999;xr=-999;yr=-999;
	if( nargin == 1)
		if( isstr(arg1) )
			action = arg1;
		else
			action = 'initialize';
			obj = arg1;
		end
	end
	if( nargin == 2)
		error(' incorrect arguments for mapview ');
	end
	if( nargin > 2 ) action = 'initialize'; end
if( strcmp( action, 'initialize' ) )
	if( nargin == 3)
		if( samesize(arg1,arg2) )
			x=arg1;
			y=arg2;
			z=arg3;
		else 
			[m,n]=size(arg3);
			if( (length(arg1)==n)&(length(arg2)==m) )
				x=arg1;
				y=arg2;
				z=arg3;
			else
				obj = arg1;
				xr = arg2;
				yr = arg3;
			end
		end
	end
	if( nargin == 5 )
		x = arg1;
		y = arg2;
		z = arg3;
		xr = arg4;
		yr = arg5;
	end
	if( ~isearthobj(obj) )
	% build an object
		if( min(size(x))>1 )
			delx = x(1,2)-x(1,1);
			xnot = x(1,1);
			dely = y(2,1)-y(1,1);
			ynot = y(1,1);
		else
			delx = x(2)-x(1);
			xnot = x(1);
			dely=y(2)-y(1);
			ynot=y(1);
		end
		obj = gridobj(' Mapview Data',z,'z data',delx,dely,xnot,ynot);
		x = [];
		%y = [];
		z = [];
	end
	if ( xr == -999 )
		xr = [];
		yr = [];
	end
	
	% free up memory
	x=[];
	%y=[];
	z=[];
	arg1=[];
	arg2=[];
	arg3=[];
	arg4=[];
	arg5=[];
% user data assignments
% current figure ... handles of the uicontrols 
%		[hdoit, hcolor, hdataLabel, hdata,...
%	 hassoc, hcolorfig, hquit, hoptionsmenu, hfliplr, hflipud, hassocColor,
%		hcontour, hcontfig]
% current axis... [himage, zmin, zmax] the handle of the current image
%		and its min and max values before scaling
% doit button (hdoit) ... not used
% color button (hcolor) ... not used
% data label (hdataLabel) ... the data object
% data popup (hdata) ... the grid number of the current display
% associated values checkbox (hassoc) ... the associated data
% quit button (hquit) ... not used
% options menu (hoptionsmenu) ... not used
% fliplr (hfliplr) ... not used
% flipud (hflipud) ... not used
% hassocColor ... an integer giving the color of the associated data, and a vector
%		of submenus
% hcontour 	... a boolean flag indicating whether contouring is on or not, 
%		the handles of the three submenus, an integer giving the number of defined
%		 contour levels, and a row vector of the contour levels themselves
%		[flag, hcontdef, hcontshow, hcontlabel, numconts, levelvector]
% hcontfig ... the handle of figure 1, the handle of the edit text containing the
%		number of contour levels, and the handle of the edit text containing the 
%		contour levels
% make a new figure;
	figure('menubar','none');
% make the doit button
	sep = 2;
	xnow = sep;
	ynow = sep;
	width = 50;
	height = 30;
	hdoit = uicontrol('style','pushbutton','string','Doit','position',...
		[xnow,ynow,width,height],'callback','mapview(''plot'')');
% attach a color button
	 xnow = xnow+width+sep;
 	hcolor = uicontrol('style','pushbutton','string','Color',...
   	'position',[xnow,ynow,width,height],'callback','mapview(''color'')');
% the data label
 	xnow = xnow+width+sep;
 	hdataLabel= uicontrol('style','text','string','Data:','position',...
		[xnow,ynow,width,height],'userdata',obj);
% the data popup
	 names = objget(obj,'fieldnames');
 	xnow = xnow+width+sep;
	width = 100;
 	hdata = uicontrol('style','popupmenu','string',names,'value',1,...
		'position',[xnow,ynow,width,height],'userdata',1);
% the quit button
	xnow = xnow+width+sep;
	width = 50;
	hquit = uicontrol('style','pushbutton','string','Quit','position',...
		[xnow,ynow,width,height],'callback','mapview(''quit'')');
		
% make the options menu
hoptionsmenu = uimenu(gcf,'label','Options');
	hfliplr = uimenu(hoptionsmenu,'label','Flip LtoR','callback',...
		'mapview(''fliplr'')','checked','off');
	hflipud = uimenu(hoptionsmenu,'label','Flip UtoD','callback',...
		'mapview(''flipud'')','checked','off');
		
	flag = 'on';
	if(isempty(xr)) flag = 'off'; end
	hassoc = uimenu( hoptionsmenu,'label','Show Assoc. Data','callback',...
		'mapview(''assoc'')','userdata',[xr(:),yr(:)],...
		'checked',flag);
	hassocColor = uimenu( hoptionsmenu,'label','Assoc. Data Color');
	hassClr=zeros(1,8);
	hassClr(1) = uimenu( hassocColor, 'Label','yellow','callback',...
		'mapview(''asscolor'')','userdata',1,'checked','off');
	hassClr(2) = uimenu( hassocColor, 'Label','magenta','callback',...
		'mapview(''asscolor'')','userdata',2,'checked','off');
	hassClr(3) = uimenu( hassocColor, 'Label','cyan','callback',...
		'mapview(''asscolor'')','userdata',3,'checked','off');
	hassClr(4) = uimenu( hassocColor, 'Label','red','callback',...
		'mapview(''asscolor'')','userdata',4,'checked','off');
	hassClr(5) = uimenu( hassocColor, 'Label','green','callback',...
		'mapview(''asscolor'')','userdata',5,'checked','off');
	hassClr(6) = uimenu( hassocColor, 'Label','blue','callback',...
		'mapview(''asscolor'')','userdata',6,'checked','off');
	hassClr(7) = uimenu( hassocColor, 'Label','white','callback',...
		'mapview(''asscolor'')','userdata',7,'checked','off');
	hassClr(8) = uimenu( hassocColor, 'Label','black','callback',...
		'mapview(''asscolor'')','userdata',8,'checked','on');
	set(hassocColor,'userdata',[8 hassClr]);
	
	% contouring 
	hcontour = uimenu( hoptionsmenu,'Label','Contours');
	hcontdef = uimenu(hcontour,'Label','Define Contours','callback',...
		'mapview(''contour'')');
	hcontshow = uimenu(hcontour,'Label','Show Contours','callback',...
		'mapview(''contour'')','enable','off');
	hcontlabel = uimenu(hcontour,'label','Label Contours','callback',...
		'mapview(''contour'')','enable','off');
	  
	% set user data on hcontour. This will be: a boolean flag indicating whether
	% contouring is on or not, the handles of the three submenus, an integer
	% giving the number of defined contour levels, and a row vector of the
	% contour levels themselves
	set(hcontour,'userdata',[0 , hcontdef, hcontshow, hcontlabel, 0]);
% store the uicontrol handles
	hcolorfig = -999;
	hcontfig = -999;
	set(gcf,'userdata',[hdoit, hcolor, hdataLabel, hdata,...
		 hassoc, hcolorfig, hquit, hoptionsmenu, hfliplr, hflipud, hassocColor,...
		 hcontour, hcontfig]);
		 
% install the default colormap
	clrmap = jet;
 	set(gcf,'colormap',clrmap);
% plot the grid
	mapview('plot');
	return;
end
%plotting section
if( strcmp(action,'plot' ) )
	% get the uicontrol handles
	h = get(gcf,'userdata');
	hcolor = h(2);
	hdataLabel = h(3);
	hdata = h(4);
	hassoc = h(5);
	hcolorfig = h(6);
	hfliplr = h(9);
	hflipud = h(10);
	hassocColor = h(11);
	hcontour=h(12);
	hcontfig = h(13);
% get the data object
	obj = get(hdataLabel,'userdata');
	
% get the object name
	objname = objget(obj,'name');
% determine which grid
	oldgridno = get(hdata,'userdata');
	gridno = get(hdata,'value');
	set(hdata,'userdata',gridno);
	
% see if we must reset contour information
	if( oldgridno~=gridno)
		contdata = get(hcontour,'userdata');
		numlvls = contdata(5);
		if( numlvls>0 ) % if explicit values were contoured then toss the contour info
			contdata(1)=0;
			contdata(5)=0;
			set(contdata(3),'enable','off');
			set(contdata(4),'enable','off');
			set(hcontour,'userdata',contdata);
			hinfo = get(hcontfig,'userdata');
			set(hinfo(3),'string','');
		end
	end
	
% get the grid name
	names = objget(obj,'namesmatrix');
	gridname = names(gridno,:);
% get the data
	z = objget(obj,gridno);
	x = objget(obj,'x');
	y = objget(obj,'y');
	
% clear the current axes
	cla;
	
% scale the data for indexing into the colormap
 	clrmap = get(gcf,'colormap');
 	n=length(clrmap);
	ind=~isnan(z);
	zmax = max(max(z(ind)));
 	zmin = min(min(z(ind)));
 	z = (n-1)*(z-zmin)/(zmax-zmin) +1;
 	
% get the flip flags
	fud =get(hflipud,'checked');
	flr =get(hfliplr,'checked');
	
% do the flipping % OBSOLETE: image function change in version 5
%	if( strcmp(fud,'on') ) % normal state is flipped for vertical
%		y=flipud(y);
%		z=flipud(z);
%	end
%	if( strcmp(flr,'on') ) 
%		x=fliplr(x);
%		z=fliplr(z);
%	end
% the image plot
% use flipud to get proper map orientation with the origin in the 
% lower left
	himage=image(x,y,z);
	ytick = linspace(min(y(:)), max(y(:)), 5);
	xtick = linspace(min(x(:)), max(x(:)), 5);
	xtick = round(xtick*10.)/10. ;
	ytick = round(ytick*10.)/10. ;
	set(gca,'xtick',xtick,'ytick',ytick);
 	set(gca,'userdata',[himage,zmin,zmax]);
 
% flip
	if( strcmp(fud,'off') )
		flipy;
		set(gca,'ytick',ytick);
		z=flipud(z);
	end
	if( strcmp(flr,'on') )
		flipx;
		set(gca,'xtick',xtick);
		z=fliplr(z);
	end
	set(himage,'cdata',z);
	
% now the distributed points
	flag = get(hassoc,'checked');
	if( strcmp(flag,'on') )
		c = get(hassocColor,'userdata');
		c = c(1);
		if    (c==1) code = 'y*'; 
		elseif(c==2) code = 'm*';
		elseif(c==3) code = 'c*';
		elseif(c==4) code = 'r*';
		elseif(c==5) code = 'g*';
		elseif(c==6) code = 'b*';
		elseif(c==7) code = 'w*';
		elseif(c==8) code = 'k*';
		end
		xryr= get(hassoc,'userdata');
		
		hold on;
		plot(xryr(:,1),xryr(:,2),code);
		hold off;
	end
	
% do contours if required
	contdata = get(hcontour,'userdata');
	c=[];
	if( contdata(1) )
		z = (z-1)*(zmax-zmin)/(n-1)+zmin; % unscale the data
		numlvls = contdata(5);
		% use the associate data color for contours
		c = get(hassocColor,'userdata');
		c=c(1);
		if    (c==1) code = 'y'; 
		elseif(c==2) code = 'm';
		elseif(c==3) code = 'c';
		elseif(c==4) code = 'r';
		elseif(c==5) code = 'g';
		elseif(c==6) code = 'b';
		elseif(c==7) code = 'w';
		elseif(c==8) code = 'k';
		end
		if( numlvls>0 )
			lvls=contdata(6:6+numlvls-1);
			hold on;
			c=contour(x,y,z,lvls,code);
			hold off;
		else
			hold on;
			c=contour(x,y,z,abs(numlvls),code);
			hold off
		end
		% enable contour labeling
		set(contdata(4),'enable','on','userdata',c);
	end
	
% add a title 
	title([objname ': ' gridname]);
% see if we need to remake the color figure
	if( hcolorfig > 0 )
		if( oldgridno ~= gridno )
			hfig1=gcf;
			vis = get(hcolorfig,'visible');
			close( hcolorfig);
			h(6)=-999;
			set(hfig1,'userdata',h);
			if( strcmp(vis,'on') )
				mapview('color');
			end
		else
			colorview('refresh',himage,hcolorfig);
		end
	end
  return;
end
% color figure popup
if( strcmp(action,'color') )
	hfig1 = gcf;
	h = get(hfig1,'userdata');
	hcolorfig = h(6);
	if( hcolorfig > 0 )
		set(hcolorfig,'visible','on');
		figure(hcolorfig);
	else
	
		d=get(gca,'userdata');
		hcolorfig = colorview(gca,d(1),d(2),d(3));
		h(6)=hcolorfig;
		set(hfig1,'userdata',h);
	end
	return;
end
% quit callback
if( strcmp(action,'quit') )
	h=get(gcf,'userdata');
	if( h(6) > 0) close(h(6)); end %close the color figure
	if( h(13)>0) close(h(13)); end % close the contour figure
	close(gcf);
end
% set the fliplr option
if( strcmp(action,'fliplr') )
	hfliplr = gcbo;
	flr=get(hfliplr,'checked');
	if( strcmp(flr,'on') )
		set(hfliplr,'checked','off');
	else
		set(hfliplr,'checked','on');
	end
	return;
end
% set the flipud option
if( strcmp(action,'flipud') )
	hflipud = gcbo;
	fud=get(hflipud,'checked');
	if( strcmp(fud,'on') )
		set(hflipud,'checked','off');
	else
		set(hflipud,'checked','on');
	end
	return;
end
% toggle the associated data option
if( strcmp(action,'assoc') )
		hassoc = gcbo;
	opt=get(hassoc,'checked');
	if( strcmp(opt,'on') )
		set(hassoc,'checked','off');
	else
		set(hassoc,'checked','on');
	end
	return;
end
% set the associated data color
if( strcmp(action,'asscolor') )
	hassClr = gcbo;
	h=get(gcf,'userdata');
	hassocColor = h(11);
	dat = get(hassocColor,'userdata');
	set( dat(dat(1)+1),'checked','off');
	set(hassClr,'checked','on');
	c=get(hassClr,'userdata');
	set(hassocColor,'userdata',[c dat(2:9)] );
	return;
end
% contouring
 if( strcmp(action,'contour') )
 	hcontopt = gcbo;
 	h= get(gcf,'userdata');
 	hcontour = h(12);
 	hcontfig = h(13);
 	hasscolor = h(11);
 	hdoit = h(1);
 	flag = get(hcontopt,'label');
 	contdata = get(hcontour,'userdata');
 	if( strcmp(flag,'Define Contours') ) % define contour levels
 		if( hcontfig < 0 ) % see if we need to build figure
 			hfig1=gcf;
 			pos = get( hdoit,'position');
			pos1=get(gcf,'position');
 			figheight = 100;
 			figwidth = 400;
 			hcontfig = figure('position',[pos(1)+pos1(1),pos(2)+pos1(2),...
 						figwidth,figheight]);
 			h(13)=hcontfig;
 			set(hfig1,'userdata',h);
 			sep=2;
 			xnow=sep;
 			height = 20;
 			ynow = figheight-sep-height;
 			width = figwidth;
 			hinstruc = uicontrol('style','text','string',...
 				'Enter number of contours OR a vector of contour levels',...
 				'position',[xnow,ynow,width,height]);
 			ynow = ynow-height-sep;
 			width = 150;
 			hnumlabel = uicontrol('style','text','string','Number of Contours:',...
 				'position',[xnow,ynow,width,height]);
 			xnow = xnow+width+sep;
 			width = 50;
 			hnum = uicontrol('style','edit','position',[xnow,ynow,width,height]);
 			xnow = sep;
 			ynow = ynow-height-sep;
 			width = 70;
 			hlevelslabel = uicontrol('style','text','string','Levels:',...
 				'position',[xnow,ynow,width,height]);
 			xnow=xnow+sep+width;
 			width = figwidth - xnow-sep;
 			hlevels = uicontrol('style','edit','position',[xnow,ynow,width,height]);
 			xnow=sep;
 			ynow=ynow-height-sep;
 			width = 50;
 			hdone = uicontrol('style','pushbutton','string','Done','position',...
 				[xnow,ynow,width,height],'callback','mapview(''contdef'')');
 			xnow=xnow+sep+width;
 			width = 50;
 			hcancel = uicontrol('style','pushbutton','string','Cancel','position',...
 				[xnow,ynow,width,height],'callback','mapview(''contdef'')');
 				
 			% set userdata on the contour figure
 			set(gcf,'userdata',[hfig1, hnum, hlevels]);
 		else
 			set( hcontfig,'visible','on');
 		end
 		% enable the show contours menu option & turn on contouring
 		set(contdata(3),'enable','on','checked','on');
 		contdata(1)=1;
 		set(hcontour,'userdata',contdata);
 	elseif( strcmp(flag,'Show Contours') ) % set contour show flag to on or off
 		show = get(hcontopt,'checked');
 		if( strcmp(show,'on') ) % turn them off
 			set( hcontopt,'checked','off');
 			contdata(1)=0;
 			set(hcontour,'userdata',contdata);
 		else % turn them on
			set( hcontopt,'checked','on');
 			contdata(1)=1;
 			set(hcontour,'userdata',contdata);
 		end
 	elseif( strcmp(flag,'Label Contours') ) % label the contours
 		% put the labels on in the associated data color
 		c = get(hasscolor,'userdata');
		c=c(1);
		if    (c==1) code = 'y'; 
		elseif(c==2) code = 'm';
		elseif(c==3) code = 'c';
		elseif(c==4) code = 'r';
		elseif(c==5) code = 'g';
		elseif(c==6) code = 'b';
		elseif(c==7) code = 'w';
		elseif(c==8) code = 'k';
		end
 		%get the C matrix
 		c=get(hcontopt,'userdata');
 		myclabel(c,'manual',code);
 	end
 	return;
 end
 
 % define the contour levels
 if( strcmp(action,'contdef') )
 	hbutton = get(gcf,'currentobject');
 	act = get(hbutton,'string');
 	if( strcmp(act,'Cancel') ) % see if the cancel button was pushed
 		set(gcf,'visible','off');
 		return;
 	end
 	h=get(gcf,'userdata');
 	hfig1=h(1);
 	hnum=h(2);
 	hlevels=h(3);
 	string1 = get(hnum,'string');
 	string2 = get(hlevels,'string');
 	if( length(string2>0) ) % define levels
 		lvlstr=sprintf('%s%s%s','lvls=[',string2,'];');
 		eval(lvlstr);
 		numlvls=length(lvls);
 	else
 		numstr=sprintf('%s%s%s','numlvls=',string1,';');
 		eval(numstr);
 		numlvls=-numlvls; % use - sign as a flag that there is no lvls vector
 	end
 	% now store the contour info in the hcontour menu in figure1
 	h=get(hfig1,'userdata');
 	hcontour = h(12);
 	contdata = get(hcontour,'userdata');
 	if( numlvls>0 )
 		contdata = [contdata(1:4) numlvls lvls];
 	else
 		contdata = [contdata(1:4) numlvls];
 	end
 	set(hcontour,'userdata',contdata);
 	set(gcf,'visible','off');
 	return;
 	
 end
 		 
 	
 				
 				