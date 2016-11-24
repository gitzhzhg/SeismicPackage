function fold(arg)
% Computes depth of detachment with least
% squares fit to x,y points, where x is the
% depth of datum relative to polygon and y
% is the area of the polygon.
% Polygons are digitized in seisline, and
%  input to fold via extract_vector.        
%
%  T. N. BISHOP,  OCTOBER 1993,
%  G. F. MARGRAVE,  OCTOBER 1993,
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
% parse the input arguments
	if( nargin < 1 )
		action = 'initialize';
	else
		action = arg;
	end
if( strcmp( action, 'initialize' ) )
	% make a new figure
	hfig = figure;
	
	% make a dummy handle for the second and third figure
	hfig2=-1;
	hfig3=-1;
	%  get the figure's size
	pos = get( hfig,'Position');
	figx=pos(1);figy=pos(2);
	figwidth=400;figheight=150;
	% reset the size
	set( hfig,'position',[figx,figy,figwidth,figheight]);
	% put a title at the top with a blank string for now
	% all absolute positions and sizes are in pixels
	height = 30;
	sep = 2;
	xnow=5;
	width = figwidth-2*xnow;
	ynow = figheight - height - sep;
	
	htitle = uicontrol('style','text','string','New Analysis','position',...
		[xnow,ynow,width,height]);
	% an action popup
	xnow=5;
	ynow=ynow-height-sep;
	width=50;
	height=30;
	hactionLabel = uicontrol('style','text','string','Action:','position',...
		[xnow,ynow,width,height]);
	xnow=xnow+width+sep;
	width=150;
	haction=uicontrol('style','popupmenu','string',...
		'Read from Disk| Pick Regionals | Edit Horizons','position',...
		[xnow,ynow,width,height']);
	% a horizons popup
	xnow=xnow+width+sep;
	width=70;
	height=30;
	hhorizonLabel = uicontrol('style','text','string','Horizons:','position',...
		[xnow,ynow,width,height]);
	xnow=xnow+width+sep;
	width=150;
	hhorizon=uicontrol('style','popupmenu','string',...
		'No Horizons','position',...
		[xnow,ynow,width,height']);
	% a file name entry field
	xnow=5;
	ynow=ynow-height-sep;
	width=65;
	hfileLabel=uicontrol('style','text','string','Filename:','Position',...
		[xnow,ynow,width,height]);
	xnow=xnow+width+sep;
	width = figwidth-3*sep-2*width;
	hfile=uicontrol('style','edit','string','???.dat','Position',...
		[xnow,ynow,width,height],'callback','fold(''input'')');
	
	% a graph button
	xnow= 5;
	width=50;
	ynow=ynow-height-sep;
	hgraph = uicontrol('style','pushbutton','string','Graph','position',...
		[xnow,ynow,width,height],'callback','fold(''graph'')');	
	% a quit button
	xnow = xnow+width+sep;
	hquit = uicontrol('style','pushbutton','string','Quit','position',...
		[xnow,ynow,width,height],'callback','fold(''quit'')');
	% now store all handles in the figures userdata
	set(hfig,'userdata',[htitle,hactionLabel,haction,hhorizonLabel,...
		hhorizon,hfileLabel, hfile, hgraph, hquit, hfig2, hfig3]);
end
% read information from disk
if( strcmp(action,'input') )
	% get the disk file name
	h=get(gcf,'userdata');
	hfile=h(7);
	fileName = get(hfile,'string');
	
	% read the file
	obj = readSeislineVec(fileName);
	% see if the fielname was valid
	if(isempty(obj))
		set(hfile,'string','File Not Found');
		return;
	end
        
        % compute initial datum
        
	[m,n]=size(obj);
	nhor = (n-1)/2;
        smin=Inf; smax=-smin;
        tmin=Inf; tmax=-tmin;
        for k=1:nhor
          horizon=objget(obj,2*k);
          sdist=objget(obj,2*k-1);
	  ind = ~isnan(horizon);
	  horizon = horizon(ind);
	  sdist = sdist(ind);
          tmin = min([horizon(:);tmin]);
          tmax = max([horizon(:);tmax]);
          smin = min([sdist(:);smin]);
          smax = max([sdist(:);smax]);
        end
	
	% enlarge window by making datum deeper & wider
        dels = smax-smin;
        td = tmin - .2 * (tmax-tmin);
	smin = smin - .2 * dels;
	smax = smax + .2 * dels;
        % store datum in horizon label
	hhorlabel = h(4);
	set(hhorlabel,'userdata',[smin, td, smax, td]);
        
	% load up the horizon popup
	
	hhorizon = h(5);
	horNames = 'horizon 1|';
	for k=2:nhor
		temp = sprintf('horizon %d|',k);
		horNames = setstr([horNames temp]);
	end
	
	set(hhorizon, 'string',horNames);
	% store the object as user data of the title object
	h=get(gcf,'userdata');	
	htitle = h(1);
	set(htitle,'userdata',obj);
	set(htitle,'string',objget(obj,'name'));
	fold('plot');
	fold('area');
	return;
end
if( strcmp(action,'area') )
% get the vector of uicontrol handles
	hfig1=gcf;
	h=get(hfig1,'userdata');
	if( length(h) < 8)
		hfig1 = h(1);
		h = get(hfig1,'userdata');
	end
% get the data object from the title uicontrol
	htitle=h(1);
	obj = get(htitle,'userdata');
% get the datum from the horizon label
	hhorizonLabel=h(4);
	datum = get(hhorizonLabel,'userdata');
	[nrow,ncol]=size(obj);
	nhors = (ncol-1)/2;
        area = zeros(1,nhors);
        dist = zeros(1,nhors);
	for k=1:nhors
		x = objget(obj,2*k-1);
		y = objget(obj,2*k);
		ind = ~isnan(x);
		x=x(ind);
		y=y(ind);
%
%  	plot with correct color
	    	if(rem(k,6) == 1), icol='y'; end
	    	if(rem(k,6) == 2), icol='m'; end
	    	if(rem(k,6) == 3), icol='c'; end
	    	if(rem(k,6) == 4), icol='r'; end
	    	if(rem(k,6) == 5), icol='g'; end
	    	if(rem(k,6) == 6), icol='b'; end
		icolsym = [icol,'*'];
		dist(k) = polydist( x,y,datum,icol);
		area(k) = polyarea( x,y,20,icolsym);
		if( k==1 ) hold; end
                fprintf('dist= %10.1f   area= %12.1f\n',dist(k),area(k));
	end
        fprintf('finished with distance and area calculations\n');
        gca;
        set(gca,'xlabel',text(0,0,'INLINE DISTANCE'));
        set(gca,'ylabel',text(0,0,'DEPTH'));
	% set the horizon labels user data
	set(hhorizonLabel,'userdata',[datum(1:4) area dist]);
end
if( strcmp(action,'graph') )
% get the vector of uicontrol handles
	hfig1=gcf;
	h=get(hfig1,'userdata');
	if( length(h) < 8)
		hfig1 = h(1);
		h = get(hfig1,'userdata');
	end
% get the data object from the title uicontrol
	htitle=h(1);
	obj = get(htitle,'userdata');
% get the area and distances from the horizon label
	[nrow,ncol]=size(obj);
	nhors = (ncol-1)/2;
        area = zeros(1,nhors);
        dist = zeros(1,nhors);
	hhorizonLabel=h(4);
	temp = get(hhorizonLabel,'userdata');
        datum = temp(1:4);
	area = temp(5:nhors+4);
	dist = temp(nhors+5:2*nhors+4);
       
% get the vector of uicontrol handles
	hfig1=gcf;
	h=get(hfig1,'userdata');
% make the plot window active
	hfig3 = h(11);
	if( hfig3 < 0)   %figure 3 window not open yet
		hfig3=figure;
		h(11)=hfig3;
		set(hfig1,'userdata',h);
		set(hfig3,'userdata',hfig1);
	else
		figure(hfig3);
	end
% plot the dist vs area
        p = polyfit(dist,area,1);
        xint = -p(2)/p(1);
        slope = p(1);
        fprintf('true detachment depth (datum = 0) = %10.1f \n',xint);
        fprintf('displacement on lower detachment  = %10.1f \n',slope);
        xhat = [xint  max(dist)];
        yhat = polyval(p,xhat);
% best fit line is y = m*x + b  where m = p(1), b = p(2)
% where x-intercept = (-b/m,0) = -p(2)/p(1) = true detachment depth
% where slope = m = p(1) = displacement on the lower detachment
        pltxmin = min(xint, min(0, min(dist)));
        pltxmax = max(dist);
        pltymin = min(yhat(1), 0);
        pltymax = max(yhat(2), max(area));
        axis([pltxmin pltxmax pltymin pltymax]);
	dist = ones(2,1)*dist;
	area = ones(2,1)*area;
        plot(dist,area,'*',xhat,yhat,'-.w')
        gca;
        set(gca,'xlabel',text(0,0,'DEPTH'));
        set(gca,'ylabel',text(0,0,'EXCESS AREA'));
% make the plot window active
        hfig2 = h(10);
	figure(hfig2);
      
        detach = datum;
%         detachment (relative to datum) = xintercept
	detach(2) = detach(2) + xint;
	detach(4) = detach(4) + xint;
        xind = [1 3];
        yind = [2 4];
        plot(detach(xind),detach(yind),'w')
	ss  = '                   detachment';
	h=text(detach(1),detach(2),ss,'VerticalAlignment',...
                'bottom','Color','w');
	ss2 = '                 reference level'; 
	h2=text(datum(1),datum(2),ss2,'VerticalAlignment',...
                'bottom','Color','y');
%         detachment displacement is slope
	detach(1) = detach(3) - abs(slope); 
        plot(detach(xind),detach(yind),'g')
        plot(detach(xind),detach(yind),'gx')
        ss3 = ['D = ',num2str(slope)];
	h=text(detach(1),detach(2),ss3,'VerticalAlignment',...
                'bottom','Color','g');
end
if( strcmp(action,'plot') )
% get the vector of uicontrol handles
	hfig1=gcf;
	h=get(hfig1,'userdata');
% make the plot window active
	hfig2 = h(10);
	if( hfig2 < 0)
		hfig2=figure;
		h(10)=hfig2;
		set(hfig1,'userdata',h);
		set(hfig2,'userdata',hfig1);
	else
		figure(hfig2);
	end
% get the datum from the horizon label
	hhorizonLabel=h(4);
	datum = get(hhorizonLabel,'userdata');
        indx = [1 3];
        indy = [2 4];
	plot(datum(indx),datum(indy))
% get the data object from the title uicontrol
	htitle=h(1);
	obj = get(htitle,'userdata');
	[nrow,ncol]=size(obj);
	nhors = (ncol-1)/2;
	nr = objget(obj,'datarows');
	nc = objget(obj,'datacols');
	m =max([nr nc]);
	x=zeros(m,nhors);
	y=zeros(m,nhors);
	for k=1:nhors
		x(:,k) = objget(obj,2*k-1);
		y(:,k) = objget(obj,2*k);
	end
        hold on;
	plot(x,y)
%	hold off;
end
if( strcmp(action,'quit') )
% get the vector of handles
	h = get(gcf,'userdata');
% get figure 2
	hfig2 = h(10);
	if(hfig2 > 0)
	  close(hfig2);
	end
% get figure 3
	hfig3 = h(11);
	if(hfig3 > 0)
	  close(hfig3);
	end
% close the current figure
	close(gcf);
end