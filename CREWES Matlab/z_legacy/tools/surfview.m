function surfview( arg1, arg2, arg3, arg4, arg5, arg6)
%
% surfview(object)
% surfview(object, xassoc, yassoc)
% surfview( xgrid,ygrid,zgrid )
% surfview( xgrid,ygrid,zgrid, xassoc, yassoc )
%
% SURFVIEW provides an interactive, point&click interface to viewing and
% manipulating 3-D surfaces.  The input can either be an Earth Object
% (as created by gridobj) or a set of x,y, and z values specifying the
% grid. If an Earth Object is input it must contain at least one 2-D grid
% and may contain many more. Only a single grid may be input when using
% the direct (x,y,z) specification. Additionally, a set of (x,y) values,
% called the associated data, may be input for display on the surface. The
% associated values may be randomly distributed but must lie within the
% bounds of the grid to be seen. SURFVIEW provides the ability to display
% any grid in the object as either a 3-D mesh (wire frame) or solid surface
% and to rotate interactivly the azimuth and elevation of the view. Two
% grids may be viewd simultaneously by using one to determine the 3-D
% structure and the other to provide color.
%
%   object = an input Earth Object containing any number of grids to be
%            viewed as surfaces
%   xassoc = a vector of the x coordinates of the associated data
%            **** default = [] ****
%   yassoc = a vector of the y coordinates of the associated data
%            **** default = [] ****
%   xgrid = a grid or vector providing the x coordinates of the grid
%           If a vector, then the length of xgrid must be the same as the
%           number of columns of zgrid.
%   ygrid = a grid or vector providing the y coordinates of the grid
%           If a vector, then the length of ygrid must be the same as the
%           number of rows of zgrid.
%   zgrid = a grid of the z values to be displayed as a surface
%
% G.F. Margrave, October 1993
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
% determine input mode
 xassoc = [];
 yassoc = [];
 if( nargin==1 & isstr(arg1) )
		action = arg1;
 elseif( nargin==1 & ~isstr(arg1) )
		action = 'initialize';
		obj = arg1;
 end
 if( nargin > 1)
		action = 'initialize';
 	[m,n]=size(arg3);
 	l1 = length(arg1); l2 = length(arg2);
 	test = 0;
 	if( ~samesize(arg1,arg2) & l1~=m ) test=1; end
 	if( nargin >= 3 & test )
		action = 'initialize';
		obj = arg1;
		xassoc = arg2;
		yassoc = arg3;
 	end
 	if( nargin >= 3 & ~test )
		x = arg1;
		y = arg2;
		z = arg3;
		if( nargin > 3)
			xassoc = arg4;
			yassoc = arg5;
		end
		if( isvec(x) ) delx = x(2)-x(1); xnot = x(1);
		else delx = x(1,2)-x(1,1); xnot = x(1,1); end
		if( isvec(y) ) dely = y(2)-y(1); ynot = y(1);
		else dely = y(2,1)-y(1,1); ynot = y(1,1); end
		obj = gridobj('Surface Viewer',z,'Grid',delx,dely,xnot,ynot);
	end
 	
end
% free up memory
x=[];
y=[];
z=[];
arg1=[];
arg2=[];
arg3=[];
arg4=[];
arg5=[];
arg6=[];
% user data assignments
%
% figure... the handles of all of the uicontrols:
%	[hcolor,hdoit,hazLabel,haz,helLabel,hel,...
%		hstrucmenu,hcolormenu,hcolorfig,hquit,hoptionsmenu,...
%		hsurfopt,hassoc,hassocColor,hshading,hfigurecolor]);
%
% hstrucmenu ... the number of the grid determining the structure and
% 	the handles of all of the submenus: [number hsgrids] where
%	 hsgrids is a vector of the handles of all of the submenus
%
% hsgrids ... (see above) each hsgrid(i) has an integer giving the grid
%       number that it references
%
% hcolormenu ... the number of the grid determining the color and the
%       handles of all of the submenus: [number hcgrids] where hcgrids
%       is a vector of the handles of all of the submenus
%
% hcgrids ... (see above) each hcgrid(i) has an integer giving the grid
%       number that it references
%
% hdoit ... the input grid(s) stored as a gridobj object
%
% hcolor ... not used
%
% hazLabel ... not used
%
% haz ... not used
%
% helLabel ... not used
%
% hel ... not used
%
% hquit ... grid number of the previous structure grid
%
% hoptionsmenu ... not used
%
% hsurfopt ... not used
%
% hassoc ... [xassoc(:), yassoc(:), zassoc(:)]
%
% hassocColor ... integer indicating color of associated data and vector of
%		submenus [int vector]
%
% hshading ... current shading option and vector of shading option handles
%				[ opt vector]
%
% hfacted ... integer flag (1)
% hflat   ... integer flag (2)
% hflatlevel ... integer flag (3)
% hinterp ... integer flag (4)
%
% initialize the viewer
if( strcmp(action,'initialize') )
% initial default azimuth
	az = -38;
% default elevation
	el = 30;
	hfig1 = figure('menubar','none');
	
	% make some controls
	sep = 2;
	xnow = sep;
	ynow = sep;
	width = 50;
	height = 30;
% store the input grid in the doit button
	hdoit = uicontrol('style','pushbutton','string','Doit',...
		'position', [xnow,ynow,width,height],'callback',...
		'surfview(''plot'')', 'userdata',obj);
	xnow = xnow+sep+width;
	hcolor = uicontrol('style','pushbutton','string','Color',...
		'position',[xnow,ynow,width,height],'callback',...
		'surfview(''color'')');
	xnow = xnow+sep+width;
	width = 75;
	hazLabel = uicontrol('style','text','position',...
		[xnow,ynow,width,height]);
	
	xnow = xnow+sep+width;
	width = 90; height = 20;
	haz = uicontrol('style','slider','min',-180,'max',180,...
	'position',[xnow,ynow,width,height],'callback',...
	'surfview(''az'')','value',az);
 
	xnow = xnow+sep+width;
	width = 75;
	height = 30;
	helLabel = uicontrol('style','text','position',...
		[xnow,ynow,width,height]);
	
	xnow = xnow+sep+width;
	width = 90;
	height = 20;
	hel = uicontrol('style','slider','min',0,'max',90,...
	'position',[xnow,ynow,width,height],'callback',...
	'surfview(''el'')','value',el);
	xnow = xnow+width+sep;
	width = 50;
	height = 30;
	hquit = uicontrol('style','pushbutton','string','Quit','position',...
		[xnow,ynow,width,height],'callback','surfview(''quit'')');
		
	% make an options menu
	
	hoptionsmenu = uimenu(gcf,'label','Options');
	
	hsurfopt = uimenu(hoptionsmenu,'label','Solid Surfaces','callback',...
		'surfview(''solid'')');
		
	hshading = uimenu(hoptionsmenu,'label','Shading','enable','off');
	hfaceted = uimenu(hshading,'label','faceted','userdata',1,...
				'checked','on','callback','surfview(''shading'')');
	hflat = uimenu(hshading,'label','flat','userdata',2,...
				'checked','off','callback','surfview(''shading'')');
	hflatlevel = uimenu(hshading,'label','flat & level lines','userdata',3,...
				'checked','off','callback','surfview(''shading'')');
	hinterp = uimenu(hshading,'label','interpolated','userdata',4,...
				'checked','off','callback','surfview(''shading'')');
	hinterplevel = uimenu(hshading,'label','interpolated & level lines','userdata',5,...
				'checked','off','callback','surfview(''shading'')');
				
	set(hshading,'userdata',[1 hfaceted hflat hflatlevel hinterp hinterplevel]);
		
	zassoc=zeros(size(xassoc));
	flag = 'on';
	if(isempty(xassoc)) flag = 'off'; end
	hassoc = uimenu( hoptionsmenu,'label','Show Assoc. Data','callback',...
		'surfview(''assoc'')','userdata',[xassoc(:),yassoc(:),zassoc(:)],...
		'checked',flag);
	hassocColor = uimenu( hoptionsmenu,'label','Assoc. Data Color',...
		'userdata',3);
	hassClr=zeros(1,8);
	hassClr(1) = uimenu( hassocColor, 'Label','yellow','callback',...
		'surfview(''asscolor'')','userdata',1,'checked','off');
	hassClr(2) = uimenu( hassocColor, 'Label','magenta','callback',...
		'surfview(''asscolor'')','userdata',2,'checked','off');
	hassClr(3) = uimenu( hassocColor, 'Label','cyan','callback',...
		'surfview(''asscolor'')','userdata',3,'checked','off');
	hassClr(4) = uimenu( hassocColor, 'Label','red','callback',...
		'surfview(''asscolor'')','userdata',4,'checked','on');
	hassClr(5) = uimenu( hassocColor, 'Label','green','callback',...
		'surfview(''asscolor'')','userdata',5,'checked','off');
	hassClr(6) = uimenu( hassocColor, 'Label','blue','callback',...
		'surfview(''asscolor'')','userdata',6,'checked','off');
	hassClr(7) = uimenu( hassocColor, 'Label','white','callback',...
		'surfview(''asscolor'')','userdata',7,'checked','off');
	hassClr(8) = uimenu( hassocColor, 'Label','black','callback',...
		'surfview(''asscolor'')','userdata',8,'checked','off');
	set(hassocColor,'userdata',[4 hassClr]);
	hfigurecolor=uimenu(hoptionsmenu,'label','Figure Color');
	hblackfig = uimenu(hfigurecolor, 'label','Black','userdata',1,...
		'callback','surfview(''figcolor'')','checked','on');
	hwhitefig = uimenu(hfigurecolor, 'label','White','userdata',2,...
		'callback','surfview(''figcolor'')','checked','off');
	set(hfigurecolor,'userdata',[1 hblackfig hwhitefig]);
		
	% make a menubar to define the grids
	hstrucmenu = uimenu(gcf,'label','Structure_Grid');
	hcolormenu = uimenu(gcf,'label','Color_Grid');
	% loop over number of grids in object and load up their names
	[m,n]=size(obj.data);
	names = objget(obj,'namesmatrix');
	hsgrids = zeros(1,n); hcgrids = zeros(1,n);
	for k=1:n
		hsgrids(k) = uimenu(hstrucmenu,'label',names(k,:),...
		'userdata',k,'callback','surfview(''strucgrid'')');
		hcgrids(k) = uimenu(hcolormenu,'label',names(k,:),...
		'userdata',k,'callback','surfview(''colorgrid'')');
	end
	% set the menu defaults
	set(hstrucmenu,'userdata',[1 hsgrids]);
	set(hcolormenu,'userdata',[1 hcgrids]);
	set(hsgrids(1),'checked','on');
	set(hcgrids(1),'checked','on');
% store the uicontrol handles in the figure
	hcolorfig = -999;
	set(gcf,'userdata',[hcolor,hdoit,hazLabel,haz,helLabel,hel,...
		hstrucmenu,hcolormenu,hcolorfig,hquit,hoptionsmenu,...
		hsurfopt,hassoc,hassocColor,hshading,hfigurecolor]);
	% set the slider labels
	surfview('az');
	surfview('el');
% make an axes
	axes('position',[.1,.2,.8,.7],'view',[az,el]);
	
% install the default colormap
	set(gcf,'colormap',jet);
% make the perspective plot
	surfview('plot');
	return;
end
% plot the 3-D view
if( strcmp(action,'plot') )
	% get the handles
	hfig1=gcf;
	h = get(hfig1,'userdata');
	hdoit = h(2);
	hstrucmenu = h(7);
	hcolormenu = h(8);
	haz = h(4);
	hel = h(6);
	hcolorfig = h(9);
	hsurfopt = h(12);
	hquit = h(10);
	hassoc = h(13);
	hassocColor = h(14);
	hshading = h(15);
	hfigurecolor=h(16);
	
	if( hcolorfig > 0)
		close(hcolorfig); % close because data's changed
		h(9)=-999;
		set(hfig1,'userdata',h);
	end
	az = get(haz,'value');
	el = get(hel,'value');
	sgrid = get(hstrucmenu,'userdata');
	sgrid=sgrid(1);
	oldgridno = get(hquit,'userdata');
	set( hquit,'userdata',sgrid);
	cgrid = get(hcolormenu,'userdata');
	cgrid=cgrid(1);
	obj = get(hdoit,'userdata');
	x = objget(obj,'x');
	y = objget(obj,'y');
	zs = objget(obj,sgrid);
	zc = objget(obj,cgrid);
	name = objget(obj,'name');
	namesmatrix = objget(obj,'namesmatrix');
	sname = namesmatrix(sgrid,:);
	cname = namesmatrix(cgrid,:);
	
	
% check the solid surface option
	surfopt = get(hsurfopt,'checked');
	
	cla;
	if( strcmp(surfopt,'off') )
		mesh(x,y,zs,zc);
	else
		surf(x,y,zs,zc);
		% check the shading option
		flag = get(hshading,'userdata');
		flag = flag(1);
		if( flag == 2)
			shading flat;
		elseif( flag ==3 );
			shading flat;
			hold on
			contour3(x,y,zs,10,'k');
			hold off
		elseif( flag ==4 );
			shading interp;
		elseif( flag == 5 );
			shading interp;
			hold on
			contour3(x,y,zs,10,'k');
			hold off
		end
	end
	set(gca,'view',[az,el]);
	
	% see if we need to plot assoc data
	flag = get(hassoc,'checked');
	if( strcmp(flag,'on') )
		ass=get(hassoc,'userdata');
		xass = ass(:,1);
		yass = ass(:,2);
		zass = ass(:,3);
		code = get(hsurfopt,'userdata');
		if( (~isempty(sgrid) & isempty(oldgridno)) | sgrid~=oldgridno | sum(zass)==0.0 )
			% interpolate
			zass = ungrid(x,y,zs,xass,yass);
			set(hassoc,'userdata',[xass,yass,zass]);
		end
		% get the associated data color
		c=get(hassocColor,'userdata');
		c=c(1);
		if    (c==1) code = 'y*';,code2='y'; 
		elseif(c==2) code = 'm*';,code2='m';
		elseif(c==3) code = 'c*';,code2='c';
		elseif(c==4) code = 'r*';,code2='r';
		elseif(c==5) code = 'g*';,code2='g';
		elseif(c==6) code = 'b*';,code2='b';
		elseif(c==7) code = 'w*';,code2='w';
		elseif(c==8) code = 'k*';,code2='k';
		end
		hold on;
		xass = xass*ones(1,2);
		yass = yass*ones(1,2);
		zass = zass*ones(1,2);
		fudge=.05*(max(zs(:))-min(zs(:)));
		zass(:,2) = zass(:,2)+fudge;
		plot3(xass(:,2),yass(:,2),zass(:,2),code);
		plot3(xass',yass',zass',code2);
		hold off;
	end
	
	% now the title
	if( sgrid == cgrid )
		title([name ': ' sname]);
	else
		title( [name ': ' 'struc= ' sname ' color= ' cname ]);
	end
	% set the figure color
	flag=get(hfigurecolor,'userdata');
	if( flag(1)==1 )
		blackfig;
	else
		whitefig;
	end
	return;
end
% set the structure grid
if( strcmp(action,'strucgrid') )
	h = get(gcf,'userdata');
	hstrucmenu = h(7);
	% get the menu infor
	menuinfo = get(hstrucmenu,'userdata');
	% uncheck the previous item
	set(menuinfo(menuinfo(1)+1),'checked','off');
	%get the menu that triggered this call
	hgridmenu =gcbo;
	% the grid number is stored in user data
	gridno = get(hgridmenu,'userdata');
	% set the number into the gridinfo
	menuinfo(1)=gridno;
	set(hstrucmenu,'userdata',menuinfo);
	set(hgridmenu,'checked','on');
	return;
end
% set the color grid
if( strcmp(action,'colorgrid') )
	h = get(gcf,'userdata');
	hcolormenu = h(8);
	% get the menu ifno
	menuinfo = get(hcolormenu,'userdata');
	%uncheck the previous item
	set(menuinfo(menuinfo(1)+1),'checked','off');
	% get the menu item that triggered this call
	hgridmenu =gcbo;
	% the gridno is stored in user data
	gridno = get(hgridmenu,'userdata');
	% set the gridno into the user data
	menuinfo(1)=gridno;
	set(hcolormenu,'userdata',menuinfo);
	set(hgridmenu,'checked','on');
	return;
end
% popup the color thingy
if( strcmp(action,'color') )
	hfig1=gcf;
	h = get(hfig1,'userdata');
% see if this already exists
	hcolorfig = h(9);
	if( hcolorfig>0)
		set(hcolorfig,'visible','on');
		figure(hcolorfig,'menubar','none');
		return;
	end
% ok, make a new one
	hcolormenu = h(8);
	cgridno = get(hcolormenu,'userdata');
	cgridno=cgridno(1);
	hdoit = h(2);
	obj = get(hdoit,'userdata');
	z = objget(obj,cgridno);
	ind=isnan(z);
	zmin = min(min(z(~ind)));
	zmax = max(max(z(~ind)));
	hcolorfig = pcolorview(gca,zmin,zmax);
	h(9)=hcolorfig;
	set(hfig1,'userdata',h);
end
% the doit button
if( strcmp( action,'doit') )
	h = get(gcf,'userdata');
	haz = h(4);
	hel = h(6);
	az = get(haz,'value');
	el = get(hel,'value');
	view(az,el);
end
% change the azimuth display
if( strcmp(action,'az') )
	h = get(gcf,'userdata');
	hazLabel = h(3);
	haz=h(4);
	az = get(haz,'value');
	if( abs(az) > 180. )
		az = ax-360.;
	end
	az = round(az);
	azLabel = sprintf('Az= %d',az);
	set( hazLabel,'string',azLabel);
end
% change the elevation display
if( strcmp(action,'el') )
	h = get(gcf,'userdata');
	helLabel = h(5);
	hel=h(6);
	el = get(hel,'value');
	el = round(el);
	elLabel = sprintf('Elev= %d',el);
	set( helLabel,'string',elLabel);
end
% the quit callback
if( strcmp(action,'quit') )
        h=get(gcf,'userdata');
        if( h(9) > 0) close(h(9)); end
        close(gcf);
end
% set the solid surface option
if( strcmp(action,'solid') )
	hsurfopt = gcbo;
	opt=get(hsurfopt,'checked');
	h=get(gcf,'userdata');
	hshading = h(15);
	if( strcmp(opt,'on') )
		set(hsurfopt,'checked','off');
		set(hshading,'enable','off');
	else
		set(hsurfopt,'checked','on');
		set(hshading,'enable','on');
	end
	return;
end
% set the associated data option
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
	hassocColor = h(14);
	dat = get(hassocColor,'userdata');
	set( dat(dat(1)+1),'checked','off');
	set(hassClr,'checked','on');
	c=get(hassClr,'userdata');
	set(hassocColor,'userdata',[c dat(2:length(dat))] );
	return;
end
% set the shading option
if( strcmp(action,'shading') )
	hshadeopt = gcbo;
	h=get(gcf,'userdata');
	hshading = h(15);
	dat = get(hshading,'userdata');
	set(dat(1+dat(1)),'checked','off');
	set(hshadeopt,'checked','on');
	flag=get(hshadeopt,'userdata');
	set(hshading,'userdata',[flag dat(2:length(dat))]);
	return;
end
% set the figure color option
if( strcmp(action,'figcolor') )
	hfigcoloropt = gcbo;
	h=get(gcf,'userdata');
	hfigurecolor = h(16);
	dat = get(hfigurecolor,'userdata');
	set(dat(1+dat(1)),'checked','off');
	set(hfigcoloropt,'checked','on');
	dat(1)=get(hfigcoloropt,'userdata');
	set(hfigurecolor,'userdata', dat);
	return;
end