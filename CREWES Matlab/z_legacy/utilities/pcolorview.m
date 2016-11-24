function hfig = pcolorview(arg,datamin,datamax,dismissMode) 
% hfig = pcolorview(haxes,datamin,datamax,dismissMode)
% hfig = pcolorview(haxes,datamin,datamax)
%
% pcolorview takes a handle to an axes object as its first argument
% and displays another figure window showing the colormap and
% color axis active in the window containing the axes.
% Facilities are provided to adjust, or change entirly, 
% the colormap and install it in the first window.
% Use pcolorview in association with any of Matlab's pseudocolor 
% displays such as pcolor, mesh, etc... For image plots, use colorview.
%
% haxes = handle to an axes object
% datamin = true minumum of the data being displayed in the axes
% datamax = true maximum of the data being displayed in the axes 
% dismissMode = 0 ... close the figure when dismissing
%               1 ... set figure's 'visible' property to 'off' when dismissing
%               ****** default = 1 *****
% hfig = handle of the figure window created by this routine
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
% hcmaxLabel = none
% hcmax = none
% hcminLabel = none
% hcmin = none
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
	if( nargin < 4) dismissMode = 1; end
end
if( strcmp(action,'initialize') )
% get the figure window and the colormap
	hfigure = get(haxes,'Parent');
	clrmap = get(hfigure,'ColorMap'); 
% get the color axis limits
	clim = get(haxes,'Clim');
% open a new figure window
	pos=get(hfigure,'Position');
	units = get(hfigure,'Units');
	figwidth = 150;% in pixels
	figheight = 300;
	hfig=figure('Position',[pos(1),pos(2),figwidth,figheight],...
		'ColorMap',clrmap,'Units',units);
% make some data to show the colors
	x=[1 2 3 4 5];
	[m,n]=size(colormap);
	y=linspace(datamin,datamax,m);
	y=y';
	z=y*ones(1,5);
% make some controls
	sep = 2;
	width = 70;
	height = 20;
	xnow= sep;
	ynow = figheight-height;
	hcmaxLabel = uicontrol('Style','Text','String','Color Max:',...
		'Position',[xnow,ynow,width,height]);
	xnow = xnow+width+sep;
	hcmax = uicontrol('Style','slider','Position',[xnow,ynow,...
		width,height],'Min',y(1),'Max',y(m),...
		'Value',clim(2),'Callback','pcolorview(''cmax'')');
	ynow=ynow-sep-height;
	xnow=sep;
	hcminLabel = uicontrol('Style','Text','String','Color Min:',...
		'Position',[xnow,ynow,width,height]);
	xnow = xnow+width+sep;
	hcmin = uicontrol('Style','slider','Position',[xnow,ynow,...
		width,height],'Min',y(1),'Max',y(m),...
		'Value',clim(1),'Callback','pcolorview(''cmin'')');
	ynow=ynow-sep-height;
	xnow=sep;
hmapsLabel = uicontrol('style','text','string','Color Maps:',...
  'Position',[xnow,ynow,width,height]);
 xnow = xnow+width+sep;
 hmaps = uicontrol('style','popupmenu','string',...
  'original|hsv|gray|hot|cool|bone|copper|pink|jet|alpine|terrain',...
  'userdata',clrmap,'position',[xnow,ynow,width,height],...
  'callback','pcolorview(''maps'')');
	ynow=ynow-sep-height;
	xnow=sep;
	hbrighten=uicontrol('Style','pushbutton','String','Brighten',...
		'Position',[xnow,ynow,width,height],'Callback',...
		'pcolorview(''brighten'')');
	xnow=xnow+sep+width;
	hdarken=uicontrol('Style','pushbutton','String','Darken',...
		'Position',[xnow,ynow,width,height],'Callback',...
		'pcolorview(''darken'')');
	ynow=ynow-sep-height;
	xnow=sep;
	hinstall=uicontrol('Style','pushbutton','String','Doit',...
		'Position',[xnow,ynow,width,height],'Callback',...
		'pcolorview(''install'')');
	xnow=xnow+sep+width;
	hauto=uicontrol('Style','pushbutton','String','Auto Scale',...
		'Position',[xnow,ynow,width,height],'Callback',...
		'pcolorview(''autoscale'')');
	ynow=ynow-sep-height;
	xnow=sep;
	hdismiss=uicontrol('Style','pushbutton','String','Dismiss',...
		'Position',[xnow,ynow,width,height],...
		'userdata',dismissMode,...
		'callback','pcolorview(''dismiss'')');
	
% plot a grid
	hgrid=pcolor(x,y,z);
	hax=get(hgrid,'Parent');
	%set(hax,'Units','pixels');
	set(hax,'XTick',[],'DataAspectRatioMode','auto',...
		'Position',[0,.05,1,.5],'Clim',clim,'userdata',z);
		%'Position',[5,5,figwidth,figheight-ynow]);
	shading flat;
% store all of the graphics handles in the figure
	set(gcf,'UserData',[hcmaxLabel,hcmax,hcminLabel,hcmin,...
		hbrighten,hdarken,hinstall,hdismiss,hgrid,hax,hfigure,...
  hmaps, hmapsLabel]);
end
if( strcmp(action,'cmax') )
% get the handles
	h=get(gcf,'UserData');
	hcmax=h(2);
% get the new maximum from the slider
	val = get(hcmax,'Value');	
% get the cmin slider
%	hcmin=h(4);
% compute a new size for the cmin slider
%	pos=get(hcmin,'Position');
%	width=pos(3);
%	minval=get(hcmin,'Min');
%	maxval=get(hcmin,'Max');
%	newWidth=width*(val-minval)/(maxval-minval);
% set the maximum and the width on the cmin slider
%	set(hcmin,'Max',val,'Position',[pos(1),pos(2),newWidth,pos(4)]);
	hax=h(10);
	clim=get(hax,'Clim');
	set(hax,'Clim',[clim(1),val]);
	return;
end
if( strcmp(action,'cmin') )
% get the handles
	h=get(gcf,'UserData');
	hcmin=h(4);
% get the new minimum from the slider
	val = get(hcmin,'Value');	
% get the cmax slider
%	hcmax=h(2);
% compute a new size for the cmax slider
%	pos=get(hcmax,'Position');
%	width=pos(3);
%	minval=get(hcmax,'Min');
%	maxval=get(hcmax,'Max');
%	newWidth=width*(val-minval)/(maxval-minval);
% set the minimum and the width on the cmax slider
%	set(hcmax,'Min',val,'Position',[pos(1),pos(2),newWidth,pos(4)]);
	hax=h(10);
	clim=get(hax,'Clim');
	set(hax,'Clim',[val,clim(2)]);
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
	hfigure=h(11);
	haxes=get(hfigure,'CurrentAxes');
	hax=h(10);
	clim=get(hax,'Clim');
	set(haxes,'Clim',clim);
	clrmap=get(gcf,'Colormap');
	set(hfigure,'ColorMap',clrmap);
	return;
end
if( strcmp(action,'autoscale') )
	h=get(gcf,'UserData');
	hfigure=h(11);
	hcmax = h(2);
	hcmin = h(4);
	val = get(hcmax,'max');
	set(hcmax,'value',val);
	val=get(hcmin,'min');
	set(hcmin,'value',val);
	haxes=get(hfigure,'CurrentAxes');
	set(haxes,'CLimMode','auto');
	return;
end
if( strcmp(action,'dismiss') )
	h = get(gcf,'userdata');
	hdismiss = h(8);
	if( hdismiss )
		set(gcf,'visible','off');
	else
		close(gcf);
	end
end	
if( strcmp(action,'maps') )
% order of maps in popup is:
%'original|hsv|gray|hot|cool|bone|copper|pink|jet'
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
 elseif( flag == 11) %terrain
   colormap(terrain);
% the following is ineffective because contrast needs the actual amplitude
% distribution of the data in the parent window, not the fake data
% created here
% elseif( flag == 10) % contrast
%   z = get(gca, 'userdata');
%  colormap(contrast(z));
 end
end