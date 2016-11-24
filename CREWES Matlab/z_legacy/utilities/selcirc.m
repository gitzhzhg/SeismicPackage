function selcirc(action)
% like selbox except it is for a circle...uses function
% twoptcirc 
%  T. N. BISHOP,  DECEMBER 1993,  CPTC CANADA
%
% SELBOX is somewhat like the built in RBBOX except that it 
% works in axes (data) coordinates rather than figure 
% coordinates (pixels). Thus the selection rectangle can 
% be used directly in data coordinates to test data for
% inclusion. SELBOX uses the userdata of the current axes
% for storage and assumes that it initially contains [pt1x pt1y] the
% x and y coordinates of the starting point of the selection rectangle. 
% So that SELBOX can be convieniently called several times in a row without
% outside intervention, SELBOX begins by deleting the graphics object whose
% handle is the fifth entry in the axes user data. An abort will occur if
% this user data entry is non-null with an invalid graphics handle.) After
% returning, the axes user data will contain the x,y coordinate of the 2
% points defining the rectangle and the graphics handle of the selection
% rectangle:  [p1x p1y p2x p2y handle]
%
% Several convienience fucntions make it easier to use SELBOX and you actually
% will rarely need to call SELBOX directly. SELBOXINIT should be called to
% begin the selection box process and SELBOXFINI to end it. You need do
% nothing else. SELBOXFINI, will return five values: [pt1x pt1y pt2x pt2y hbox]
% Where pt1 and pt2 are the points defining the rectangle and hbox is the
% graphics handle to its on-screen image. (Note that SELBOXFINI needs no
% arguments while SELBOXINIT has one optional argument described below)
% Example:
% [x,y,z]=peaks; % get something to plot
% pcolor(x,y,z); % plot it
% 
% selboxinit % begin the seltion box
% % the selection box is drawn on screen
% vals=selboxfini; % obtain the selection box coordinates
%
% Often you will want to do some action of your own upon a mouse button up
% event. This is easily done by passing a string argument to SELBOXINIT which
% contains any Matlab command (or series of commands).
% selboxinit('do_my_zoom')
%
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
        p1=get(gca,'currentpoint');
        dat=get(gca,'userdata');
        if(length(dat)>=4) delete(dat(4)); delete(dat(5)); end
        set(gca,'userdata',p1(1,1:2));
        set(gcf,'windowbuttonmotionfcn','selcirc(''motion'')');
	return;
end
if(strcmp(action,'fini'))
	set(gcf,'windowbuttonmotionfcn','');
	return;
end
if( strcmp(action,'motion') )
% get the starting point from axes userdata
	h=get(gca,'userdata');
	
	p1=h(1:2);
	
	% delete any pre-existing rectangle
	if(length(h)>=4) delete(h(4)); delete(h(5)); end
	
% get the current point from the axes
	p2=get(gca,'currentpoint');
	p2=p2(1,1:2);
% draw the circle
        [x,y,r] = twoptcircle([p1(1) p2(1)],[p1(2) p2(2)]);
	 h=line(x,y,'erasemode','xor','linestyle','-.','linewidth',4,...
	'color',[.5 .5 .5]);
         s = sprintf('r = %8.3f',r);
         h2 = text(p1(1),p1(2),s,'erasemode','xor','color',[.5 .5 .5]);
%h=line([p1(1),p21),p2(1),p1(1),p1(1)],[p1(2),p1(2),p2(2),p2(2),...
%	p1(2)],'erasemode','xor','linestyle','-.','linewidth',4);
% update the info in userdata
 	set(gca,'userdata',[p1 r h h2]);
	return;
end