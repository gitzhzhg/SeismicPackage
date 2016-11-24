function gridtool(arg1,arg2,arg3,arg4,arg5)
% gridtool(x,y,z,xr,yr)
% gridtool(x,y,z)
% gridtool(obj,xr,yr)
% gridtool(obj,wells)
% gridtool(obj)
%
% GRIDTOOL presents an interactive interface to Matlab facilities for examining
% and modifying gridded data. The easiest and preferred way to bring data into 
% GRIDTOOL is to load the data into an Earth Object and then load that
% object into this program. (For further information on Earth Objects, type:
% help earthobjects)
% This allows any number of grids to be input simultaneously and examined
% collectively.
% GRIDTOOL provides facilities for:
%   - displaying grids or their 2-D Fourier transforms as colormapped images
%   - altering the colormaps of such images
%   - zooming arbitrary portions of the grids
%   - slicing through the grids along arbitrary piecewise linear trajectories
%   - displaying the positions of associated data on top of the grids
%   - contouring the grids and displaying the contours on top of the
%     colormapped images
%   - outputting any grid or grid slice to disk as an Earth Object
%
%  A number of interesting variants of these techniques are possible
%  including the ability to slice all grids simultaneously thereby producing
%  a crossection through the entire collection of grids. Any slices are
%  automatically packaged as Earth Objects and displayed in either SLICETOOL
%  or LOGSEC
%
%  See also: SLICETOOL, MAPVIEW, SURFVIEW, EARTHOBJECTS, READZMAP, GRIDOBJ,
%            RANDOBJ, LOGSEC, CONTOBJ
%
% 	x = a grid or vector of x coordinates. if a vector, then its
%		length must be the same as the number of columns in z.
%	y = a grid or vector of y coordinates. If a vector, then its 
%		length must be the same as the number of rows of z.
%	z = the grid of z values. This is the data of the "map".
%	xr = a vector of x coordinates of randomly distributed points to
%		be plotted on top of the map.
%	yr = a vector of y coordinates of randomly distributed points to
%               be plotted on top of the map. yr and xr must be the same length.
%	obj   = Earth Object containing the grid(s) to be worked with
%	wells = Container object containing any number of Well objects which
%               are related to the grid object. Each well should also be a
%               container as described in wellspecs.txt
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
	wells=-999;
	if( nargin == 1)
		if( isstr(arg1) )
			action = arg1;
		else
			action = 'initialize';
			obj = arg1;
		end
	end
	if( nargin > 1 ) action = 'initialize'; end
if( strcmp( action, 'initialize' ) )
	if( nargin == 2)
		obj=arg1;
		wells=arg2;
	end
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
		y = [];
		z = [];
	end
	if( xr==-999 & wells~=-999 )
	% a well set has been input. Load up vectors xr and yr with the well locations
	% get the well names
		names=objget(wells,'namesmatrix','well');
		[nwells,r]=size(names);
		xr=zeros(nwells,1);
		yr=zeros(nwells,1);
		for k=1:nwells
			well=objget(wells,k,'well');
			loc=objget(well,'location');
			xr(k)=loc(1);
			yr(k)=loc(2);
		end
	end
	if ( xr == -999 )
		xr = [];
		yr = [];
	end
	if( wells==-999 ) wells=[]; end
	
	% set things to null to free up memory
	x=[];
	y=[];
	z=[];
	arg1=[];
	arg2=[];
	arg3=[];
	arg4=[];
	arg5=[];
% user data assignments
% current figure ... handles of the uicontrols 
%		[hdoit, hcolor, hreset,...
%		 hassoc, hcolorfig, hquit, hoptionsmenu, hfliplr, hflipud,...
%		 hassocColor, haction, hmode, hdata, hdecibel, hpts,...
%		hscaling,hslice,hcontour,hcontfig,htransoptions,houtput,...
%		houtopts,hmessage]
% current axis... used in conjunction with selbox.m to draw selection boxes
% doit button (hdoit) ... the current grid and coordinates
% color button (hcolor) ... not used
% reset button ... the data object
% associatedata menu (hassoc) ... the associated data locations
% quit button (hquit) ... [himage, zmin, zmax] the handle of the current 
%               and its min and max values before scaling
% options menu (hoptionsmenu) ... not used
% fliplr (hfliplr) ... grid size of the current grid in normal coordinates
% flipud (hflipud) ... the current zoom window and associated graphics handles
%	[p1x p1y p2x p2y hbox hcorners]
% associated color menu (hassocColor) ... integer indicating color and vector of
%		submenu handles [int vector]
% action menu (haction) ... integer indicating the current action state and a
%       vector of the submenu handles [int vector]
% mode menu (hmode) ... integer indicating the current mode and a vector of the
%		2 submenu handles  [int(1 or 2)  vector]
% data menu (hdatamenu) ... grid number of the current grid, the previous grid
%		 and vector of submenus : [currentNum prevNum vector]
% decibel menu (hdecibel) ... not used
% storage bin for mouse selected points (hpts) ... vector of points [x1 y1 x2 y2 ....]
% scaling menu (hscaling) ... integer indicating current state and vector of
%		submenu handles [int vector]
% storage bin for slice information (hslice) ... integer indicating the number of points
%	defining the slice, then the x coordinates of the points, and then the
%	y coordinates of all the points, and then the handles of the line and corners
% Contour menu (hcontour) ... A boolean flag indicating whether contouring is on, the
%	handles of the three submenus, and a row vector of the cntour levels
%		[flag hcontdef hcontshow hcontlabel levelvector]
% Handle of the Contouring popup (hcontfig) ... the handle of the main figure and the
%	handles of the edit text objects containing the number of contours and the contour
%	levels: [hfig1 hnum hlevels]
% Transform options menu (htransoptions) ... The handles of the submenus [hzeromean and
%	hdecibel]
% Output menu (houtput) ... the latest slice object
% Output options menu (houtopts) ... the handles of the output options submenus
%		[houtgrid,houtobj,hmatfile,hascii]
% Message area (hmessage) ... the wells object
%
% make a new figure;
	figure('menubar','none');
% make the doit button
	sep = 2;
	xnow = sep;
	ynow = sep;
	width = 40;
	height = 20;
	hdoit = uicontrol('style','pushbutton','string','Doit','position',...
		[xnow,ynow,width,height],'callback','gridtool(''doit'')');
% attach a color button
	 xnow = xnow+width+sep;
 	hcolor = uicontrol('style','pushbutton','string','Color',...
   	'position',[xnow,ynow,width,height],'callback','gridtool(''color'')');
 
% the quit button
	xnow = xnow+width+sep;
	hquit = uicontrol('style','pushbutton','string','Quit','position',...
		[xnow,ynow,width,height],'callback','gridtool(''quit'')');
		
% a reset button
% This is currently used only for storage. Make it invisible for now
	xnow=xnow+width+sep;
	hreset = uicontrol('style','pushbutton','string','Reset','position',...
		[xnow,ynow,width,height],'callback','gridtool(''reset'')',...
		'userdata',obj,'visible','off');
% make a message field
	pos=get(gcf,'position');
	width=pos(3)-4*sep-xnow;
	height=20;
	hmessage=uicontrol('style','edit','position',[xnow,ynow,width,height]);
	% put the wells object in the message userdata
	set(hmessage,'userdata',wells);
% make an output menu
	houtput = uimenu(gcf,'label','Output');
	hcurrgrid = uimenu(houtput,'Label','Current Grid','callback',...
		'gridtool(''out'')');
	hallgrids = uimenu(houtput,'Label','All Grids','callback',...
		'gridtool(''out'')');
	hcurrslice = uimenu(houtput,'Label','Current Slice','callback',...
		'gridtool(''out'')');
	houtopts = uimenu(houtput,'label','Output Options');
	houtgrid = uimenu(houtopts,'label','Output as Simple Grid','callback',...
		'gridtool(''exboolean'')','checked','off');
	houtobj = uimenu(houtopts,'label','Output as Object','callback',...
		'gridtool(''exboolean'')','checked','on','userdata',...);
		[2 houtgrid]);
	set(houtgrid,'userdata',[1 houtobj]);
	hmatfile = uimenu(houtopts,'label','In Matlab File Format','callback',...
		'gridtool(''exboolean'')','checked','on');
	hascii = uimenu(houtopts,'label','In ASCII File Format','callback',...
		'gridtool(''exboolean'')','checked','off','userdata',...
		[2 hmatfile]);
	set(hmatfile,'userdata',[1 hascii]);
	set(houtopts,'userdata',[houtgrid,houtobj,hmatfile,hascii]);
% make the options menu
	hoptionsmenu = uimenu(gcf,'label','Options');
	hfliplr = uimenu(hoptionsmenu,'label','Flip LtoR','callback',...
		'gridtool(''boolean'')','checked','off');
	hflipud = uimenu(hoptionsmenu,'label','Flip UtoD','callback',...
		'gridtool(''boolean'')','checked','off');
	hscaling = uimenu(hoptionsmenu,'label','Scaling');
	hlocal = uimenu(hscaling,'label','Local','checked','off','callback',...
		'gridtool(''scaling'')','userdata',1);
	hglobal = uimenu(hscaling,'label','Global','checked','on','callback',...
		'gridtool(''scaling'')','userdata',2);
	set(hscaling,'userdata',[2 hlocal hglobal]);
		
	flag = 'on';
	if(isempty(xr)) flag = 'off'; end
	hassoc = uimenu( hoptionsmenu,'label','Show Assoc. Data','callback',...
		'gridtool(''boolean'')','userdata',[xr(:),yr(:)],...
		'checked',flag);
	hassocColor = uimenu( hoptionsmenu,'label','Assoc. Data Color',...
		'userdata',8);
	hassClr=zeros(1,8);
	hassClr(1) = uimenu( hassocColor, 'Label','yellow','callback',...
		'gridtool(''asscolor'')','userdata',1,'checked','off');
	hassClr(2) = uimenu( hassocColor, 'Label','magenta','callback',...
		'gridtool(''asscolor'')','userdata',2,'checked','off');
	hassClr(3) = uimenu( hassocColor, 'Label','cyan','callback',...
		'gridtool(''asscolor'')','userdata',3,'checked','off');
	hassClr(4) = uimenu( hassocColor, 'Label','red','callback',...
		'gridtool(''asscolor'')','userdata',4,'checked','off');
	hassClr(5) = uimenu( hassocColor, 'Label','green','callback',...
		'gridtool(''asscolor'')','userdata',5,'checked','off');
	hassClr(6) = uimenu( hassocColor, 'Label','blue','callback',...
		'gridtool(''asscolor'')','userdata',6,'checked','off');
	hassClr(7) = uimenu( hassocColor, 'Label','white','callback',...
		'gridtool(''asscolor'')','userdata',7,'checked','off');
	hassClr(8) = uimenu( hassocColor, 'Label','black','callback',...
		'gridtool(''asscolor'')','userdata',8,'checked','on');
	set(hassocColor,'userdata',[8 hassClr]);
% make the actions menu
	haction = uimenu(gcf,'label','Actions');
	hplot = uimenu(haction,'label','Plot','callback',...
		'gridtool(''action'')','checked','on','userdata',1);
	hzoom = uimenu(haction,'label','Zoom','callback',...
		'gridtool(''action'')','checked','off','userdata',2);
	hunzoom = uimenu(haction,'label','unZoom','callback',...
		'gridtool(''action'')','checked','off','userdata',3);
	hslicemenu = uimenu(haction,'label','Slice','callback',...
		'gridtool(''action'')','checked','off','userdata',4);
	hrotate = uimenu(haction,'label','Rotate','callback',...
		'gridtool(''action'')','checked','off','userdata',5,...
		'enable','off');
	hmode = uimenu(haction,'label','Mode','userdata',1);
	hnormal = uimenu(hmode,'label','Normal','userdata',1,'callback',...
		'gridtool(''mode'')','checked','on');
	htransform = uimenu(hmode,'label','Transform','userdata',2,'callback',...
		'gridtool(''mode'')','checked','off');
	htransoptions=uimenu(hmode,'label','Transform Options');
	hzeromean=uimenu(htransoptions,'label','Remove Mean','checked','on',...
		'callback','gridtool(''boolean'')','userdata',0.);
	hdecibel = uimenu(htransoptions,'label','Decibel Spectra','callback',...
		'gridtool(''boolean'')','checked','on');
	set(htransoptions,'userdata',[hzeromean,hdecibel]);
        % contouring
        hcontour = uimenu( haction,'Label','Contours');
        hcontdef = uimenu(hcontour,'Label','Define Contours','callback',...
                'gridtool(''contour'')');
        hcontshow = uimenu(hcontour,'Label','Show Contours','callback',...
                'gridtool(''contour'')','enable','off');
        hcontlabel = uimenu(hcontour,'label','Label Contours','callback',...
                'gridtool(''contour'')','enable','off');
	% slicing options
	hsliceopt=uimenu(haction,'label','Slicing Options');
	hsliceone=uimenu(hsliceopt,'label','Slice Current Grid',...
		'callback','gridtool(''exboolean'')','checked','on');
	hsliceall=uimenu(hsliceopt,'label','Slice All Grids',...
		'callback','gridtool(''exboolean'')','checked','off');
	set(hsliceopt,'userdata',[hsliceone hsliceall]);
	set(hsliceone,'userdata',[1 hsliceall]);
	set(hsliceall,'userdata',[2 hsliceone]);
         
        % set user data on hcontour. This will be: a boolean flag indicating whether
        % contouring is on or not, the handles of the three submenus, an integer
        % giving the number of defined contour levels, and a row vector of the
        % contour levels themselves
        set(hcontour,'userdata',[0 , hcontdef, hcontshow, hcontlabel, 0]);
% make the data menu
	hdata = uimenu(gcf,'label','Data');
	 % loop over number of grids in object and load up their names
        [m,n]=size(obj.data);
        names = objget(obj,'namesmatrix');
        hgrids = zeros(1,n);
        for k=1:n
                hgrids(k) = uimenu(hdata,'label',names(k,:),...
                'userdata',k,'callback','gridtool(''data'')');
	end
% set the menu default and the userdata
	set(hgrids(1),'checked','on');
	set(hdata,'userdata',[1 0 hgrids]);
% make an invisible storage bin for the mouse points
	hpts=uicontrol('style','text','visible','off','userdata',[]);
% make an invisible storage bin for the slice information
	hslice=uicontrol('style','text','visible','off','userdata',[]);
% store the uicontrol handles
	hcolorfig = -999;
	hcontfig = -999;
	set(gcf,'userdata',[hdoit, hcolor, hreset,...
		hassoc, hcolorfig, hquit, hoptionsmenu, hfliplr, hflipud,...
		hassocColor, haction, hmode, hdata, hdecibel, hpts,...
		hscaling,hslice,hcontour,hcontfig,htransoptions,houtput,...
		houtopts,hmessage]);
	set(haction,'userdata',[1 hplot hzoom hunzoom hslicemenu hrotate...
		 hmode hsliceopt]);
	set(hmode,'userdata',[1 hnormal htransform]);
		 
% install the default colormap
	clrmap = jet;
 	set(gcf,'colormap',clrmap);
% plot the grid
	gridtool('plot');
	return;
end
%plotting section
if( strcmp(action,'plot' ) )
	% get the uicontrol handles
	h = get(gcf,'userdata');
% h = [hdoit, hcolor, hreset,...
%		 hassoc, hcolorfig, hquit, hoptionsmenu, hfliplr, hflipud,...
%		 hassocColor, haction, hmode, hdata, hdecibel]
	hdoit = h(1);
	hcolor = h(2);
	hreset = h(3);
	hassoc = h(4);
	hcolorfig = h(5);
	hquit=h(6);
	hoptionsmenu = h(7);
	hfliplr = h(8);
	hflipud = h(9);
	hassocColor = h(10);
	hdata = h(13);
	hmode = h(12);
	hdecibel=h(14);
	hscaling = h(16);
	hslice = h(17);
	hcontour=h(18);
	hcontfig=h(19);
% toss any preexisting slice info
	sliceinfo = get(hslice,'userdata');
	if( length(sliceinfo) > 0 )
		set(hslice,'userdata',[]);
	end
	
% determine transform flag values
	flag = get(hmode,'userdata');
	if( flag(1)==1 )
		transform = 0;
	else
		transform = 1;
	end
	
	flag = get( hdecibel,'checked');
	if( strcmp(flag,'off') )
		decibel = 0;
	else
		decibel =1;
	end
	
% get the data object
	obj = get(hreset,'userdata');
	
% get the object name
	objname = objget(obj,'name');
% determine which grid
	dat = get(hdata,'userdata');
	gridno = dat(1);
	oldgridno = dat(2);
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
	
% set the data into the doit button if it has changed
	if( oldgridno ~= gridno)
		z=objget(obj,gridno);
		x=objget(obj,'x');
		y=objget(obj,'y');
		gridname = objget(obj,'dataname',gridno);
		currgrid = gridobj('normal',z,gridname,x(2)-x(1),y(2)-y(1),x(1),y(1));
		set(hdoit,'userdata',currgrid);
		dat(2)=dat(1);
		set(hdata,'userdata',dat);
	end
	
% get the grid name
	names = objget(obj,'namesmatrix');
	gridname = names(gridno,:);
% get the data grid
	currgrid = get(hdoit,'userdata');
	z = objget(currgrid,1);
	x = objget(currgrid,'x');
	y = objget(currgrid,'y');
	ind=~isnan(z);
	zmaxglobal = max(max(z(ind)));
	zminglobal = min(min(z(ind)));
% clear the current axes
	cla;
% examine the zoom information
	zoomstuff = get(hflipud,'userdata');
	if(~isempty(zoomstuff))
		p1x=zoomstuff(1); p1y=zoomstuff(2);
		p2x=zoomstuff(3); p2y=zoomstuff(4);
		set(hflipud,'userdata',zoomstuff(1:4));
		xzm1 = min([p1x,p2x]);
		xzm2 = max([p1x,p2x]);
		yzm1 = min([p1y,p2y]);
		yzm2 = max([p1y,p2y]);
	else
		xzm1 = min(x);
		xzm2 = max(x);
		yzm1 = min(y);
		yzm2 = max(y);
	end
% subset the info with the zoom window
	indx = find( ( x>= xzm1 )&( x<=xzm2 ) );
	indy = find( ( y>= yzm1 )&( y<=yzm2 ) );
	x=x(indx);
	y=y(indy);
	z=z(indy,indx);
% get the grid state
	state = objget(currgrid,'name');
% transform if need be
	if( transform & strcmp(state,'normal') )
		gridtool('transform');
		% reget the data
		currgrid = get(hdoit,'userdata');
		z = objget(currgrid,1);
		x = objget(currgrid,'x');
		y = objget(currgrid,'y');
		ind=~isnan(z);
		zmaxglobal = max(max(z(ind)));
		zminglobal = min(min(z(ind)));
	elseif( ~transform & strcmp(state,'transform') )
		gridtool('invtransform');
		% reget the data
		currgrid = get(hdoit,'userdata');
		z = objget(currgrid,1);
		x = objget(currgrid,'x');
		y = objget(currgrid,'y');
		ind=~isnan(z);
		zmaxglobal = max(max(z(ind)));
		zminglobal = min(min(z(ind)));
	end
	
	% compute the amplitude spectrum of the transform
	if(transform)
		% convert to decibels if needed
		z=abs(z);
		if(decibel)
		    mxz=max(z(:));
			z=20*log10(z/mxz);
		end
		zmaxglobal = max(z(:));
		zminglobal = min(z(:));
	end
	
% scale the data for indexing into the colormap
	scaleopt = get(hscaling,'userdata');
	scaleopt = scaleopt(1);
 	clrmap = get(gcf,'colormap');
 	n=length(clrmap);
	if( scaleopt == 1)
		ind=~isnan(z);
		zmax = max(max(z(ind)));
 		zmin = min(min(z(ind)));
	elseif( scaleopt ==2)
		zmax = zmaxglobal;
		zmin = zminglobal;
	end
 	z = (n-1)*(z-zmin)/(zmax-zmin) +1;
 	
% get the flip flags
	fud =get(hflipud,'checked');
	flr =get(hfliplr,'checked');
	
% do the flipping
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
	xtick = round(xtick*10.)/10.;
	ytick = round(ytick*10.)/10.;
	set(gca,'xtick',xtick,'ytick',ytick);
 	set(hquit,'userdata',[himage,zmin,zmax]);
 	
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
			hfig1=gcf;
			vis = get(hcolorfig,'visible');
			pos=get(hcolorfig,'position');
			close( hcolorfig);
			h(5)=-999;
			set(hfig1,'userdata',h);
			if( strcmp(vis,'on') )
				gridtool('color');
				h=get(hfig1,'userdata');
				set(h(5),'position',pos);
			end
	end
  return;
end
% color figure popup
if( strcmp(action,'color') )
	hfig1 = gcf;
	h = get(hfig1,'userdata');
	hquit = h(6);
	hcolorfig = h(5);
	if( hcolorfig > 0 )
		set(hcolorfig,'visible','on');
		figure(hcolorfig);
	else
	
		d=get(hquit,'userdata');
		hcolorfig = colorview(gca,d(1),d(2),d(3));
		h(5)=hcolorfig;
		set(hfig1,'userdata',h);
	end
	return;
end
% quit callback
if( strcmp(action,'quit') )
	h=get(gcf,'userdata');
	if( h(5) > 0) close(h(5)); end
	close(gcf);
end
% set any boolean option
if( strcmp(action,'boolean') )
	hflag = gcbo;
	flag=get(hflag,'checked');
	if( strcmp(flag,'on') )
		set(hflag,'checked','off');
	else
		set(hflag,'checked','on');
	end
	return;
end
% set any exclusive boolean option
% the userdata of such an option is assumed to consist of
% an integer flag (ignored by this action) followed by a list of 
% handles which are to have the opposite boolean setting
if( strcmp(action,'exboolean') )
	hflag = gcbo;
	flag=get(hflag,'checked');
	list=get(hflag,'userdata');
	if( length(list)>1 )
		list=list(2:length(list));
	else
		list=[];
	end
	if( strcmp(flag,'on') )
		set(hflag,'checked','off');
		if( length(list)>0 )
			for k=1:length(list)
				set(list(k),'checked','on');
			end
		end
	else
		set(hflag,'checked','on');
		if( length(list)>0 )
			for k=1:length(list)
				set(list(k),'checked','off');
			end
		end
	end
	return;
end
% set the associated data color
if( strcmp(action,'asscolor') )
	hassClr = gcbo;
	h=get(gcf,'userdata');
	hassocColor = h(10);
	dat = get(hassocColor,'userdata');
	set( dat(dat(1)+1),'checked','off');
	set(hassClr,'checked','on');
	c=get(hassClr,'userdata');
	set(hassocColor,'userdata',[c dat(2:9)] );
	return;
end
% set the action
if( strcmp(action,'action') )
	hthisaction = gcbo;
	h=get(gcf,'userdata');
	haction=h(11);
	flag = get(hthisaction,'userdata');
	dat = get(haction,'userdata');
	set(hthisaction,'checked','on');
	set(dat(dat(1)+1),'checked','off');
	dat(1)=flag;
	set(haction,'userdata',dat);
% turn off all window buttons for starters
 set(gcf,'windowbuttondownfcn','');
 set(gcf,'windowbuttonupfcn','');
 set(gcf,'windowbuttonmotionfcn','');
% set the window button commands
	if( flag == 1) % a plot
		set(gcf,'WindowButtonUpFcn','');
		% enable the doit button
		set(h(1),'enable','on');
	elseif( flag == 3 ) % an unzoom
		 set(gcf,'WindowButtonUpFcn','');
                % enable the doit button
                set(h(1),'enable','on');
	elseif(flag ==2)  % a zoom, slice or rotate
		selboxinit('gridtool(''zoom'')');
		%set(gcf,'windowbuttondownfcn','selboxinit');
	 	%set(gcf,'WindowButtonUpFcn','selboxfini;gridtool(''zoom'')');
		% disable the doit button
		set(h(1),'enable','off');
	else %slice or rotate
		set(gcf,'windowbuttonupfcn','gridtool(''mouseup'')');
		%disable doit
		set(h(1),'enable','off');
		% delete any preexisting line & reset the slice info to null
		hslice=h(17);
		        sliceinfo = get(hslice,'userdata');
        		if( length(sliceinfo) > 0 )
                		npts = sliceinfo(1);
                		if( length(sliceinfo)>2*npts+1 )
                			% delete the line and corners
                        		delete(sliceinfo(2*npts+2));
                        		delete(sliceinfo(2*npts+3));
                		end
                		sliceinfo = [];
                		set(hslice,'userdata',sliceinfo);
        		end
	end
	return;
end
% do a mouseup action
if(strcmp(action,'mouseup'))
	% determine the type of action
	h=get(gcf,'userdata');
	hpts=h(15);
	haction = h(11);
	act = get(haction,'userdata');
	act=act(1);
		
	if( act== 4) % do a slice
		% get the point
		pt = get(gca,'CurrentPoint');
		pt = pt(1,1:2);
		% store the point
		pts=get(hpts,'userdata');
		pts = [pts pt];
		set(hpts,'userdata',pts);
		gridtool('slice');
	elseif(act == 5) % define a rotation
		gridtool('rotate');
	end
	
	return;
end
% set the data grid
if( strcmp(action,'data') )
        h = get(gcf,'userdata');
        hdata = h(13);
        % get the menu infor
        menuinfo = get(hdata,'userdata');
        % uncheck the previous item
        set(menuinfo(menuinfo(1)+2),'checked','off');
        %get the menu that triggered this call
        hgridmenu =gcbo;
        % the grid number is stored in user data
        gridno = get(hgridmenu,'userdata');
        % set the number into the gridinfo
		menuinfo(2)=menuinfo(1); % remember the old grid
        menuinfo(1)=gridno;
        set(hdata,'userdata',menuinfo);
        set(hgridmenu,'checked','on');
        return;
end
% set the mode
if( strcmp(action,'mode') )
	h = get(gcf,'userdata');
	hthismode = gcbo;
	hmode = h(12);
	haction=h(11);
	actdata=get(haction,'userdata');
	hsliceopt=actdata(8);
	sliceopts=get(hsliceopt,'userdata');
	dat = get(hmode,'userdata');
	set(dat(dat(1)+1),'checked','off');
	set(hthismode,'checked','on');
	dat(1)=get(hthismode,'userdata');
	set(hmode,'userdata',dat);
	
	% disable multiple slicing if mode=transform
	if(dat(1)==1) %this is normal mode
		set(sliceopts(2),'enable','on'); % enable multiple slicing
	else
		set(sliceopts(2),'enable','off','checked','off'); % disable
		set(sliceopts(1),'checked','on');
	end
	
	return;
end
% do a forward 2D transform of the grid
if(strcmp(action,'transform') )
	   h = get(gcf,'userdata');
	   hdoit = h(1);
	   htransoptions=h(20);
	   transdata=get(htransoptions,'userdata');
	   hzeromean=transdata(1);
	   zeromean=0;
	   if( strcmp(get(hzeromean,'checked'),'on') )
	   		zeromean=1;
	   end
	   currgrid = get(hdoit,'userdata');
	   x=objget(currgrid,'x');
	   y=objget(currgrid,'y');
	   z=objget(currgrid,1);
	   gridname = objget(currgrid,'dataname',1);
	% we remove the mean here rather than letting fftgrid do it
	% so that we know what it is and can put it back in on an inverse
	% transform
	   if(zeromean)
	   		zmean=mean(z(:));
	   		z=z-zmean;
	   		set(hzeromean,'userdata',zmean);
	  end
	   	
	   [Z, X, Y] = fftgrid(z,x,y,1,20,0);% 20pct pad hardwired
	   % save the original grid size in the user data of fliplr
	   hfliplr = h(8);
	   set(hfliplr,'UserData',size(z));
	   x=[];
	   y=[];
	   z=[];
	   currgrid = gridobj('transform',Z,gridname,X(2)-X(1),Y(2)-Y(1),X(1),...
			Y(1));
 	   set(hdoit,'UserData',currgrid);
 	   
 	   return;
 end
% do an inverse 2-D transform of the grid
if(strcmp(action,'invtransform') )
	h=get(gcf,'userdata');
	hdoit=h(1);
	currgrid=get(hdoit,'userdata');
	X=objget(currgrid,'x');
    Y=objget(currgrid,'y');
    Z=objget(currgrid,1);
    htransoptions=h(20);
	   transdata=get(htransoptions,'userdata');
	   hzeromean=transdata(1);
	   zeromean=0;
	   if( strcmp(get(hzeromean,'checked'),'on') )
	   		zeromean=1;
	   end
    gridname = objget(currgrid,'dataname',1);
	% get original grid size
	hfliplr=h(8);
	sz=get(hfliplr,'userdata');
	% the transform
        [z, x, y] = ifftgrid(Z,X,Y,sz(2),sz(1),20);% 20pct pad hardwired
        X=[];
        Y=[];
        Z=[];
    if(zeromean)
    	zmean=get(hzeromean,'userdata');
    	z=z+zmean;
    	set(hzeromean,'userdata',0.0);
    end
    
	currgrid = gridobj('normal',z,gridname,x(2)-x(1),y(2)-y(1),x(1),...
                 y(1));
        set(hdoit,'UserData',currgrid);
	return;
end
if( strcmp(action,'zoom') )
	% get the zoom box
	box=selboxfini;
	
	% get the hflipud handle
	h=get(gcf,'userdata');
	hflipud = h(9);
	% store the zoom box information
	set( hflipud,'userdata',box{1});
	% enable the doit button
	hdoit = h(1);
	set(hdoit,'enable','on');
	return;
end
% define a slice trajectory
if(strcmp(action,'slice') )
	% get the latest point
	h=get(gcf,'userdata');
	hpts=h(15);
	hdoit = h(1);
	hslice = h(17);
	% determine if the selection process is to be reset. This is indicated by 
	% a mouse click with the third button
	test = get(gcf,'selectiontype');
	if( strcmp(test,'alt') )
		% delete any preexisting line & reset the slice info with this point
		        sliceinfo = get(hslice,'userdata');
        		if( length(sliceinfo) > 0 )
                		npts = sliceinfo(1);
                		if( length(sliceinfo)>2*npts+1 )
                			% delete the line and corners
                        		delete(sliceinfo(2*npts+2));
                        		delete(sliceinfo(2*npts+3));
                		end
                		sliceinfo = [];
                		set(hslice,'userdata',sliceinfo);
        		end
	end
    thispt=get(hpts,'userdata'); % get the current point
    set(hpts,'userdata',[]); % set the pts vector to null
    
    % get the slice info
    sliceinfo = get(hslice,'userdata');
    if( length(sliceinfo)>0 )
    	npts=sliceinfo(1);
    	x=sliceinfo(2:npts+1);
    	y=sliceinfo(npts+2:2*npts+1);
    	hline = sliceinfo(2*npts+2);
    	hcorner = sliceinfo(2*npts+3);
    	% delete existing line and corners
    	delete(hline);
    	delete(hcorner);
    else
    	npts=0;
    	x=[];
    	y=[];
    end;
    
    %include the current point
    npts=npts+1;
    sliceinfo=[];
    sliceinfo=npts;
    x = [x thispt(1)];
    y = [y thispt(2)];  
    sliceinfo=[sliceinfo x];
    sliceinfo=[sliceinfo y];
    
% plot the slice trajectory
	hline = line(x,y,'erasemode','xor','color',[ .5 .5 .5],...
		'linewidth',1);
	hcorners = line(x,y,'markers','*','erasemode','xor','color',...
		[.5 .5 .5]);
	% store the slice information
	sliceinfo = [sliceinfo hline hcorners];
	set(hslice,'userdata',sliceinfo);
    % enable the doit button
    set(hdoit,'enable','on');
	return;
end
% here is the doit button callback. This is basically a switchboard
if( strcmp(action,'doit') )
	h=get(gcf,'userdata');
	% determine the action state
	haction = h(11);
	actdat = get(haction,'userdata');
	flag = actdat(1);
	if( flag == 1 | flag == 2 ) % plot & zoom call the same thing
		gridtool('plot');
	elseif (flag == 3) % an unzoom
		gridtool('unzoom');
	elseif ( flag == 4) % a slice
		hsliceopt=actdat(8);
		opts=get(hsliceopt,'userdata');
		hsliceone=opts(1);
		if( strcmp(get(hsliceone,'checked'),'on'))
			gridtool('doslice');
		else
			gridtool('allslice');
		end
	elseif( flag == 5) % a rotate
		gridtool('dorotate');
	end
	% make sure windowbutton functions are off
	set(gcf,'windowbuttondownfcn','');
	set(gcf,'windowbuttonupfcn','');
	
	% reset action to plot
	set(actdat(flag+1),'checked','off');
	set(actdat(2),'checked','on');
	actdat(1)=1;
	set(haction,'userdata',actdat);	
	return;
end
% here is the unzoom callback
if(strcmp(action,'unzoom') )
	h=get(gcf,'userdata');
	hflipud = h(9);
	zoominfo = get(hflipud,'userdata');
	
	if( length(zoominfo)>4 )
		delete(zoominfo(5));
		set(gca,'userdata',[]);
	end
	set(hflipud,'userdata',[]);
	gridtool('plot');
end
% set the scaling option
if( strcmp(action,'scaling') )
	h = get(gcf,'userdata');
	hthisopt = gcbo;
	hscaling = h(16);
	dat = get(hscaling,'userdata');
	set(dat(dat(1)+1),'checked','off');
	set(hthisopt,'checked','on');
	dat(1)=get(hthisopt,'userdata');
	set(hscaling,'userdata',dat);
	return;
end
% compute a slice through the grid
if( strcmp(action,'doslice') )
	set(gcf,'pointer','watch');
	% get the slice information
	h=get(gcf,'userdata');
	hslice = h(17);
	hmode = h(12);
	hdecibel=h(14);
	sliceinfo = get(hslice,'userdata');
	npts = sliceinfo(1);
	xpts = sliceinfo(2:npts+1);
	ypts = sliceinfo(npts+2:2*npts+1);
	% get the current grid
	hdoit = h(1);
	currgrid = get(hdoit,'userdata');
	z=objget(currgrid,1);
	x=objget(currgrid,'x');
	y=objget(currgrid,'y');
	state = objget(currgrid,'name');
	gridname = objget(currgrid,'dataname',1);
	
		
% determine transform flag values
	
	flag = get( hdecibel,'checked');
	if( strcmp(flag,'off') )
		decibel = 0;
	else
		decibel =1;
	end
	
	%if(strcmp(state,'transform') ) % see if we are slicing a transform
		% convert to decibels if needed
	%	z=abs(z);
	%	if(decibel)
	%	    mxz=max(z(:));
	%		z=20*log10(z/mxz);
	%	end
	% end
	
	dx=x(2)-x(1);
	dy=y(2)-y(1);
	
	% loop over pts. Assume linear slice path between pts
	xslice=[];
	yslice=[];
	zslice=[];
	slice=[];
	precise = 100.*eps;
	
	for k=2:npts
		%slice sample rate
		deltax = xpts(k)-xpts(k-1);
		deltay = ypts(k)-ypts(k-1);
		theta = atan2(deltay,deltax);
		ds = abs(dx*cos(theta))+abs(dy*sin(theta)); % varies with angle
		
		% determine interpolation coordinates
		s = sqrt(deltax*deltax+deltay*deltay);% length of the segment
		ns = round(s/abs(ds)); % number of samples in the slice
		if( k==npts ) ns=ns+1; end % get the last point
		xinc=ds*cos(theta);yinc = ds*sin(theta);% x and y increments
		if( abs(xinc)<precise) xinc = 0.0; end
		if( abs(yinc)<precise) yinc = 0.0; end
		% seg = xcoord(0.,ds,ns);
		if (xinc == 0) xseg = xpts(k-1)*ones(1,ns);
		else xseg= xcoord(xpts(k-1),xinc,ns); end
		if (yinc == 0) yseg = ypts(k-1)*ones(1,ns);
		else yseg= xcoord(ypts(k-1),yinc,ns); end
		% interpolate
		zseg = ungrid(x,y,z,xseg,yseg);
		
		% accumulate
		xslice = [xslice xseg];
		yslice = [yslice yseg];
		zslice = [zslice zseg];
		% slice = [slice seg];
	end
	% compute inline distance
	idist=cumsum(diff(xslice).^2 + diff(yslice).^2);
	slice = [0 idist];
	% put the slice information in a random object
	slicename=gridname;
	if( strcmp(state,'transform') ) slicename = [state ' of ' gridname]; end
	robj = randobj('Gridtool Slice',zslice,slicename,xslice,yslice);
	robj = objset(robj,'inline distance',slice);
	
	% display the slice in the slice viewer
	hfig=gcf;
	slicetool(robj);
	set(hfig,'pointer','arrow');
	% save the slice in houtput
	houtput=h(21);
	set(houtput,'userdata',robj);
	return;
end
% compute a slice through all grids
if( strcmp(action,'allslice') )
	set(gcf,'pointer','watch');
	% get the slice information
	h=get(gcf,'userdata');
	hslice = h(17);
	hmode = h(12);
	hdecibel=h(14);
	sliceinfo = get(hslice,'userdata');
	npts = sliceinfo(1);
	xpts = sliceinfo(2:npts+1);
	ypts = sliceinfo(npts+2:2*npts+1);
	% get the data object
	hreset=h(3);
	object=get(hreset,'userdata');
	% determine how many grids
	[rows,cols]=size(object);
	ngrids=cols-1;
	% loop over grids
	for kgrid=1:ngrids
	% get the current grid
	z=objget(object,kgrid);
	gridname = objget(object,'dataname',kgrid);
	if(kgrid==1)
		x=objget(object,'x');
		y=objget(object,'y');
		objname = objget(object,'name');
		dx=x(2)-x(1);
		dy=y(2)-y(1);
		precise = 100.*eps;
	end
	
	
	% loop over pts. Assume linear slice path between pts
	xslice=[];
	yslice=[];
	zslice=[];
	slice=[];
	
	for k=2:npts
		%slice sample rate
		deltax = xpts(k)-xpts(k-1);
		deltay = ypts(k)-ypts(k-1);
		theta = atan2(deltay,deltax);
		ds = abs(dx*cos(theta))+abs(dy*sin(theta)); % varies with angle
		
		% determine interpolation coordinates
		s = sqrt(deltax*deltax+deltay*deltay);% length of the segment
		ns = round(s/abs(ds)); % number of samples in the slice
		if( k==npts ) ns=ns+1; end % get the last point
		xinc=ds*cos(theta);yinc = ds*sin(theta);% x and y increments
		if( abs(xinc)<precise) xinc = 0.0; end
		if( abs(yinc)<precise) yinc = 0.0; end
		% seg = xcoord(0.,ds,ns);
		if (xinc == 0) xseg = xpts(k-1)*ones(1,ns);
		else xseg= xcoord(xpts(k-1),xinc,ns); end
		if (yinc == 0) yseg = ypts(k-1)*ones(1,ns);
		else yseg= xcoord(ypts(k-1),yinc,ns); end
		% interpolate
		zseg = ungrid(x,y,z,xseg,yseg);
		
		% accumulate
		xslice = [xslice xseg];
		yslice = [yslice yseg];
		zslice = [zslice zseg];
		% slice = [slice seg];
	end
	if( kgrid == 1 )
	% compute inline distance
		id=cumsum(diff(xslice).^2+diff(yslice).^2);
		slice=[0 id];
		%slice = sqrt( (xslice-xslice(1)).^2 + (yslice-yslice(1)).^2 );
	% put the slice information in a random object
		
		robj = randobj(['Gridtool Slice of ' objname],zslice,...
			gridname,xslice,yslice);
		robj = objset(robj,'inline distance',slice);
	else
		robj=objset(robj,gridname,zslice);
	end
	
	% end of loop over grids
	end
	% display the slice in the slice viewer
	hfig=gcf;
	slicetool(robj);
	set(hfig,'pointer','arrow');
	% save the slice in houtput
	houtput=h(21);
	set(houtput,'userdata',robj);
	return;
end
% contouring
 if( strcmp(action,'contour') )
 	hcontopt = gcbo;
 	h= get(gcf,'userdata');
 	hcontour = h(18);
 	hcontfig = h(19);
 	hasscolor = h(10);
		hmsg=h(23);
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
 			h(19)=hcontfig;
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
 				[xnow,ynow,width,height],'callback','gridtool(''contdef'')');
 			xnow=xnow+sep+width;
 			width = 50;
 			hcancel = uicontrol('style','pushbutton','string','Cancel','position',...
 				[xnow,ynow,width,height],'callback','gridtool(''contdef'')');
 				
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
			set(hmsg,'string','MB1 click: label contour ... RETURN to stop labeling');
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
 	hcontour = h(18);
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
% output section
if( strcmp(action,'out') )
	h=get(gcf,'userdata');
	houtput=h(21);
	houtopts=h(22);
	hmsg=h(23);
	set(hmsg,'string','');
	hout=gcbo;
	outmode=get(hout,'label');
% get the handles of the options submenus
% houtopts userdata = [houtgrid,houtobj,hmatfile,hascii]
	
	dat = get(houtopts,'userdata');
	houtgrid=dat(1);
	hmatfile=dat(3);
	flag=get(houtgrid,'checked');
	if( strcmp(flag,'on') )
		gridout = 1;
		objout=0;
	else
		objout=1;
		gridout=0;
	end
	flag=get(hmatfile,'checked');
	if( strcmp(flag,'on') )
		matout = 1;
		asciiout=0;
		fname='*.mat';
	else
		asciiout=1;
		matout=0;
		fname='*.dat';
	end
% get the output file name
	pos=get(gcf,'position');
	xpopup=pos(1)+pos(3)/2;
	ypopup=pos(2)+pos(4);
	[filename,path]=uiputfile(fname,'Output File Selection',xpopup,ypopup);
	if( isempty(filename) )
		set(hmsg,'string','Output aborted: no file name given');
		return;
	end
	if( filename==0 )
		set(hmsg,'string','Output aborted');
		return;
	end
	ind = findstr(filename,'.mat');
	if( length(ind)>0 ) filename=filename(1:ind-1); end
	ind = findstr(filename,'.dat');
	if( length(ind)>0 ) filename=filename(1:ind-1); end
	fullfilename = [path filename];
% now, a different case for each possible output
%
% First Output the CURRENT GRID
%
	if(strcmp(outmode,'Current Grid') )
	% get the current grid from the doit button
	
		hdoit=h(1);
		% put the data in a variable whose name=filename
		eval([filename '=get(hdoit,''userdata'');']);
		if( objout )
			if( matout )
				eval(['save ' fullfilename ' ' filename]);
			elseif( asciiout )
				eval(['save ' fullfilename ' ' filename ' -ascii']);
			end
		elseif( gridout )
			eval([filename '=objget(filename,1);']);
			if( matout )
				eval(['save ' fullfilename ' ' filename]);
			elseif( asciiout )
				eval(['save ' fullfilename ' ' filename '-ascii']);
			end
		end
%
% Next Output ALL GRIDS
%
	elseif( strcmp(outmode,'All Grids') )
	% get the data object from the reset button
		hreset=h(3);
		% put the data in a variable whose name=filename
		eval([filename '=get(hreset,''userdata'');']);
		
		if( objout )
			if(matout)
				eval(['save ' fullfilename ' ' filename]);
               		 elseif( asciiout )
				eval(['save ' fullfilename ' ' filename ' -ascii']);
               		 end
        elseif( gridout )
		% print a message saying this is not working
			set(hmsg,'string','ERROR: All Grids must be output as an OBJECT');
			return;
        end
%
% Next Output the CURRENT SLICE
%
	elseif( strcmp(outmode,'Current Slice') )	
	% get the slice from the output menu
		% put the data in a variable whose name=filename
		eval([filename '=get(houtput,''userdata'');']);
	
		if( objout )
			if(matout )
				eval(['save ' fullfilename ' ' filename]);
			elseif(asciiout)
				eval(['save ' fullfilename ' ' filename ' -ascii']);
			end
		elseif( gridout )
		% print a an error message
			set(hmsg,'string','ERROR: Slices must be output as OBJECTS');
			return;
		end
	end
	set(hmsg,'string','Output was successful');
	return;
end