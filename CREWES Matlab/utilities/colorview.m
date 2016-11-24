function hfig=colorview(arg,himage,datamin,datamax,dismissMode) 
% COLORVIEW: puts up and interactive widget to manipulate colormaps
%
% hfig=colorview(haxes,himage,datamin,datamax,dismissMode)
% hfig=colorview(haxes,himage,datamin,datamax)
% colorview('refresh',himage,hcolorfig)
% bool=colorview('iscolor',hfig)
% hcoloraxes = colorview('coloraxes',hmasterfig,hcolorfig)
% vals= colorview('getvalues',hcolorfig)
%
% COLORVIEW is designed to be used by a parent figure window which has
% an image object (or objects) displayed and needs to give the user
% interactive control over the colormap and other color features.
% (If more than 1 image object, then the first is used to determine
% scaling and data range so it is best if they are similar. This
% is mainly intended to allow control of a color bar in the main 
% figure.)
% If the parent object has a pseudocolor display, then it should use
% PCOLORVIEW instead to achieve a similar functionality.
% COLORVIEW takes a handle to an axes object as its first argument
% and displays another figure window showing the colormap and
% color axis active in the window containing the axes.
% Facilities are provided to adjust, or change entirly, 
% the colormap and install it in the first window.
% The first two calling modes are used to initialize a COLORVIEW
% object and differ in that one always assumes the figure will
% simply become invisible when dismissed while the other offer
% the ability to kill the figure when dismissed. Both return
% the handle of the color figure so that the parent window
% can keep track of it. The third mode of calling is intended
% to be used by the parent window when the handle of the image
% object has changed (e.g. it has been redrawn) and an existing
% color figure needs to be notified. (This is only intended
% for situations in which the data has not changed.
% If the data has changed then the
% color figure should be closed and rebuilt)
%
% The fourth calling mode will determine whether the figure whose 
% handle is hfig is a colorview figure or not and return a boolean
% flag indicating this.
%
% The fifth mode is used to create a color axes in the masterfigure.
% One created, colorview will update it as well as the main image.
% If the handle of the created coloraxes is hcax, then calling
% colorview('deletecolor',hcax) will delete it. (Note this makes
% the colorview window inconsistent so you should kill it too when
% doing this)
%
% The sixth mode is used to determine the color scaling values for
% scaling an image in accordance with the colorview display. Normally
% colorview does this for you but there may be circumstances where
% it is necessary to do it yourself. In which case the scaling
% equation is
%	imagedata=(imagedata-vals(1))/(vals(2)-vals(1))*(nkols-1) +1;
%	where nkols is the length of the current colormap
%
%	haxes = handle to an axes object
%   himage = vector of handles of the image object in the
%		parent figure whose color is to be controlled
% 	datamin = true minumum of the data being displayed in the axes
% 	datamax = true maximum of the data being displayed in the axes 
%   dismissMode = 1 ... dismiss button sets figure's visible property
%						to 'off'
%		        = 0 ... dismiss button kills figure window
%	********** default = 1 ************
%   hcolorfig = handle of the color figure
%
%	hfig = handle of the color figure window
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

% user data assignments:
% figure = all of the graphics handles
%         [hcmaxLabel,hcmax,hcminLabel,hcmin,hbrighten,hdarken,
%         hinstall,hdismiss,hgrid,hax,hfigure,hmaps, hmapsLabel]
% axes = z vaues of fake grid
% hcmaxLabel = vector of handles of the images to be updated
% hcmax = handle of a second image that is also to be updated
% hcminLabel = handle of the fake color grid in the color window
% hcmin = [datamin datamax]
% hbrighten = none
% hdarken = none
% hinstall = none
% hauto = none
% hdismiss = dismissMode
% hmaps = original colormap
% hmapsLabel = none

% determine the type of argument passed
if( isstr(arg) )
	action = arg;
else
	haxes=arg;
	action = 'initialize';
end

if( strcmp(action,'initialize') )
	if( nargin < 5 ) dismissMode = 1; end
% get the figure window and the colormap
	hfigure = get(haxes,'Parent');
	clrmap = get(hfigure,'ColorMap'); 
% get the color axis limits
	clim = get(haxes,'Clim');
% open a new figure window
	pos=get(hfigure,'Position');
	units = get(hfigure,'Units');
	figwidth = 150;% in pixels
	figheight = 350;
 	hfig=figure('Units',units,'Position',[pos(1),pos(2),figwidth,figheight],...
		'ColorMap',clrmap,'menubar','none');
% make some data to show the colors
	x=[1 2 3 4 5];
	[m,n]=size(colormap);
 imagedata = get(himage(1),'cdata');
 ind=~isnan(imagedata);
 immin = floor(min(min(imagedata(ind))));
 immax = ceil(max(max(imagedata(ind))));
 z = (immax:-1.:immin)';
	y=linspace(datamax,datamin,length(z));
	y=y';
	z=z*ones(1,5);
% make some controls
	sep = 2;
	width = 70;
	height = 20;
	xnow= sep;
	ynow = figheight-height;
	hcmaxLabel = uicontrol('Style','Text','String','Color Max:',...
		'Position',[xnow,ynow,width,height],'userdata',himage);
	xnow = xnow+width+sep;
 maxsetting = m-abs( (m-1)*(immax-m)/(immax-immin) );
	hcmax = uicontrol('Style','slider','Position',[xnow,ynow,...
		width,height],'Min',1,'Max',m,...
		'Value',maxsetting,'Callback','colorview(''cmax'')');
	ynow=ynow-sep-height;
	xnow=sep;
	hcminLabel = uicontrol('Style','Text','String','Color Min:',...
		'Position',[xnow,ynow,width,height]);
	xnow = xnow+width+sep;
 minsetting = 1+abs( (m-1)*(1-immin)/(immax-immin) );
	hcmin = uicontrol('Style','slider','Position',[xnow,ynow,...
		width,height],'Min',1,'Max',m,...
		'Value',minsetting,'Callback','colorview(''cmin'')',...
		'userdata',[datamin datamax]);
	ynow=ynow-sep-height;
	xnow=sep;
hmapsLabel = uicontrol('style','text','string','Color Maps:',...
  'Position',[xnow,ynow,width,height]);
 xnow = xnow+width+sep;
 hmaps = uicontrol('style','popupmenu','string',...
  'original|hsv|gray|hot|cool|bone|copper|pink|jet|alpine',...
  'userdata',clrmap,'position',[xnow,ynow,width,height],...
  'callback','colorview(''maps'')');
	ynow=ynow-sep-height;
	xnow=sep;
	hbrighten=uicontrol('Style','pushbutton','String','Brighten',...
		'Position',[xnow,ynow,width,height],'Callback',...
		'colorview(''brighten'')');
	xnow=xnow+sep+width;
	hdarken=uicontrol('Style','pushbutton','String','Darken',...
		'Position',[xnow,ynow,width,height],'Callback',...
		'colorview(''darken'')');
	ynow=ynow-sep-height;
	xnow=sep;
	hinstall=uicontrol('Style','pushbutton','String','Doit',...
		'Position',[xnow,ynow,width,height],'Callback',...
		'colorview(''install'')');
	xnow=xnow+sep+width;
	hauto=uicontrol('Style','pushbutton','String','Auto Scale',...
		'Position',[xnow,ynow,width,height],'Callback',...
		'colorview(''autoscale'')');
	ynow=ynow-sep-height;
	xnow=sep;
	hdismiss=uicontrol('Style','pushbutton','String','Dismiss',...
		'Position',[xnow,ynow,width,height],'Callback',...
		'colorview(''dismiss'')','userdata',dismissMode);
	
% plot a grid
	hgrid=image(x,flipud(y),flipud(z));
	hax=get(hgrid,'Parent');
	%set(hax,'Units','pixels');
 	ytick = linspace(min(y),max(y),8);
 	fact = floor(log10(ytick(8)-ytick(1)));
 	fact = 10^(fact-2);
	ytick = fact*floor(ytick/fact); 
	set(hax,'XTick',[],'DataAspectRatiomode','auto','ytick',ytick,...
		'Position',[.4,.05,.4,.45],'Clim',clim,'userdata',z,'ydir',...
		'normal');

% store all of the graphics handles in the figure
	set(gcf,'UserData',[hcmaxLabel,hcmax,hcminLabel,hcmin,...
		hbrighten,hdarken,hinstall,hdismiss,hgrid,hax,hfigure,...
  hmaps, hmapsLabel]);
  return;
end

if( strcmp(action,'cmin')|strcmp(action,'cmax') )
% get the handles
	h=get(gcf,'UserData');
	hcmin=h(4);
 hcmax=h(2);
% get the new min & max from the sliders
	valmin = get(hcmin,'Value');
 valmax = get(hcmax,'value');

%get the colormap
clrmap = get(gcf,'colormap');
n=length(clrmap);

% get the data and scale it
 hgrid = h(9);
 z = get(hgrid,'cdata');
 zmax=max(z(:));zmin=min(z(:));
 z = (n-1)*(z-zmin)/(zmax-zmin)+1;
 z = (n-1)*(z-valmin)/(valmax-valmin)+1;

% reinstall the data
	set(hgrid,'cdata',z);
	return;
end

if( strcmp(action,'brighten') )
	brighten(.5);
	return;
end

if( strcmp(action,'darken') )
	brighten(-.5);
	return;
end

if( strcmp(action,'install') )
	h=get(gcf,'UserData');
% get the figure
	hfigure=h(11);
% get the image handle
 himage = get(h(1),'userdata');
% get the axes
	haxes=get(hfigure,'CurrentAxes'); %axes of the image
	hax=h(10);% axes of the fake grid
% get the new min & max from the sliders
	hcmin=h(4);
 hcmax=h(2);
	valmin = get(hcmin,'Value');
 valmax = get(hcmax,'value');
 % get the colormap from the fake grid
 clrmap = get(gcf,'colormap');
% rescale the image data
 n=length(clrmap);
 for k=1:length(himage)
		% get the image data
		 imagedata = get(himage(k),'cdata');
		% scale the data into the range of the colormap
		 ind=~isnan(imagedata);
		 dmax=max(max(imagedata(ind)));
		 dmin=min(min(imagedata(ind)));
		 imagedata = (n-1)*(imagedata-dmin)/(dmax-dmin)+1;
		% scale data into the range determined by the sliders
		 imagedata = (n-1)*(imagedata-valmin)/(valmax-valmin)+1;
		% install the image data
		 %hax=get(himage(k),'parent');
		 %xdir=get(hax,'xdir');
		 %ydir=get(hax,'ydir');
		 set(himage(k),'cdata',imagedata);
		 %set(hax,'xdir',xdir,'ydir',ydir);

	end
% install the colormap
	set(hfigure,'ColorMap',clrmap);
	%set(hax,'xdir',xdir,'ydir',ydir);

	return;
end

if( strcmp(action,'autoscale') )
	h=get(gcf,'UserData');
% get the sliders and set them to maximum
 hcmin = h(4);
 hcmax= h(2);
 val = get(hcmin,'min');
 set(hcmin,'value',val);
 val = get(hcmax,'max');
 set(hcmax,'value',val);

% call the slider refresh
 colorview('cmin');

 % install
 % colorview('install');
	return;
end

if( strcmp(action,'dismiss') )
	h = get(gcf,'userdata');
	hdismiss = h(8);
	dismissMode = get(hdismiss,'userdata');
	if( dismissMode )
		set(gcf,'visible','off');
	else
		close(gcf);
	end
	return;
end	

if( strcmp(action,'maps') )
% order of maps in popup is:
%'original|hsv|gray|hot|cool|bone|copper|pink|jet|alpine'
	h=get(gcf,'UserData');
	hmaps=h(12);
 flag = get( hmaps,'value');
 if( flag == 1) % original colormap
   clrmap = get(hmaps,'userdata');
   colormap(clrmap);
 elseif( flag == 2) % hsv colormap
   colormap(hsv);
 elseif( flag == 3) % gray
   colormap(gray);
 elseif( flag == 4 ) % hot
   colormap(hot);
 elseif( flag == 5) % cool
   colormap(cool);
 elseif( flag == 6) % bone
   colormap( bone );
 elseif( flag == 7) % copper
   colormap(copper);
 elseif( flag== 8 ) % pink
   colormap(pink);
 elseif( flag == 9) % jet
   colormap(jet);
 elseif( flag == 10) % alpine
   colormap(alpine);
% the following is ineffective because contrast needs the actual amplitude
% distribution of the data in the parent window, not the fake data
% created here
% elseif( flag == 10) % contrast
%   z = get(gca, 'userdata');
%  colormap(contrast(z));
 end
 return;
end

% refresh is called by the parent display if the image has changed in order to
% give the colorfigure the new image handle
if( strcmp(action,'refresh') )
	hcolorfig = datamin;
	h = get(hcolorfig, 'userdata');
	hcmaxLabel = h(1);
	set(hcmaxLabel,'userdata',himage);
	return;
end
	
%
% determine if its a color window
%
if(strcmp(action,'iscolor'))
 htest=himage;
 hfigs=figs;
 hfig=0;
 if( ~isempty(htest) )
 	ind=find(hfigs==htest);
        if(isempty(ind))
		h=get(htest,'userdata');

	if(length(h)>0)
		hkids=get(htest,'children');
		ind=find(hkids==h(1));
		if(~isempty(ind))
			if(strcmp(get(h(1),'type'),'uicontrol'))
				if(strcmp(get(h(1),'string'),'Color Max:'))
					hfig=1;
				end
			end
		 end
	end
	end

	return;
end
end

%
% delete a coloraxes in the main figure
%
if(strcmp(action,'deletecolor'))
	hcax=himage;
	hmasterfig=get(hcax,'parent');
	hax=get(hmasterfig,'currentaxes');

	pos=get(hax,'position');
	set(hax,'position',[pos(1)-.05 pos(2) pos(3)+.05 pos(4)]);

	delete(hcax);

	return;
end

%
% make a second color axes in the main window showing
% color values
%
if(strcmp(action,'coloraxes'))
	hmasterfig=himage;
	hcolorfig=datamin;

	if(~colorview('iscolor',hcolorfig))
		hfig=[];
		return;
	end

	h=get(hcolorfig,'userdata');
	hcmaxlabel=h(1);
	hcmin=h(4);
	dat=get(hcmin,'userdata');
	datamin=dat(1);
	datamax=dat(2);
	himage=get(hcmaxlabel,'userdata');
	clrmap=get(hcolorfig,'colormap');
	hax=get(hmasterfig,'currentaxes');

% make some data to show the colors
	x=[1 2 3 4 5];
	[m,n]=size(colormap);
 imagedata = get(himage(1),'cdata');
 ind=~isnan(imagedata);
 immin = floor(min(min(imagedata(ind))));
 immax = ceil(max(max(imagedata(ind))));
 z = (immax:-1.:immin)';
	y=linspace(datamax,datamin,length(z));
	y=y';
	z=z*ones(1,5);

	%make an axes just to the left of the current axes
	%in the masterfig
	figure(hmasterfig);
	pos=get(hax,'position');

	haxnew=axes('position',[pos(1)-.05 pos(2) .05 pos(4)/4]);
	set(hax,'position',[pos(1)+.05 pos(2) pos(3)-.05 pos(4)]);

 hax=get(hmasterfig,'currentaxes');
	set(hmasterfig,'currentaxes',haxnew);

	%make the image plot
	hgrid=image(x,flipud(y),flipud(z));
	ytick = linspace(min(y),max(y),8);
	fact = floor(log10(ytick(8)-ytick(1)));
	fact = 10^(fact-2);
	ytick = fact*floor(ytick/fact);
	set(haxnew,'XTick',[],'DataAspectRatiomode','auto','ytick',ytick,...
		'ydir','normal','xlabel',text(0,0,'colorscale'));

	set(hcmaxlabel,'userdata',[himage hgrid]);

 set(hmasterfig,'currentaxes',hax);

 hfig=haxnew;

 return;
end

%
% return the color scaling values
%
if(strcmp(action,'getvalues'))
	hcolorfig=himage;
	if( ~colorview('iscolor',hcolorfig))
		hfig=[];
		return;
	end

	h=get(hcolorfig,'userdata');
% get the new min & max from the sliders
	hcmin=h(4);
 hcmax=h(2);
	valmin = get(hcmin,'Value');
 valmax = get(hcmax,'value');

 hfig=[valmin valmax];
 return;
end