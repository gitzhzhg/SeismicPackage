function clabel(cs, contours)
% CLABEL - Add contour labels to a contour plot.
%	CLABEL(CS) adds height labels to the current contour plot
%	using the contour structure CS output from the CONTOUR routine.
%	The label positions are selected randomly.  For example:
%
%	   cs = contour(rand(10)); clabel(cs)
%
%	CLABEL(CS,V) labels just those contour levels given in
%	vector V.  The default action is to label all known contours.
%
%	CLABEL(CS,'manual') places contour labels at the locations
%	clicked on with a mouse.  Pressing the return key terminates
%	labeling.  Use the space bar to enter contours and the arrow
%	keys to move the crosshair if no mouse is available.
%
%	See also CONTOUR, GINPUT.
%	Charles R. Denham, MathWorks, 1988, 1989.
%	Revised 4/12/90 by CRD: XYLIST now in screen coordinates.
%	Revised 4/16/90 by CRD: Removed an inappropriate test.
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

%	Copyright (c) 1984-92 by The MathWorks, Inc.
if nargin == 0
	error('Not enough input arguments.')
end
if min(size(cs)) > 2
	error('First input must be a valid contour description matrix.')
end
cax = gca;
next = lower(get(cax,'NextPlot'));
hold_state = ishold;
manual = 0;
choice = 0;
if nargin > 1
   if isstr(contours)
      manual = strcmp(contours, 'manual');
	if ~manual
		error('Invalid argument.');
	end
     else
	choice = 1;
	contours = sort(contours(:));
   end
end
hold on;
[mcs, ncs] = size(cs);
% Decompose contour data structure if manual mode.
if manual
   disp(' '), disp('    Please wait a moment...')
   x = []; y = []; clist = []; k = 0; n = 0;
   while (1)
      k = k + n + 1; if k > ncs, break, end
      c = cs(1,k); n = cs(2,k); nn = 2 .* n -1;
      xtemp = zeros(nn, 1); ytemp = zeros(nn, 1);
      xtemp(1:2:nn) = cs(1, k+1:k+n);
      xtemp(2:2:nn) = (xtemp(1:2:nn-2) + xtemp(3:2:nn)) ./ 2;
      ytemp(1:2:nn) = cs(2, k+1:k+n);
      ytemp(2:2:nn) = (ytemp(1:2:nn-2) + ytemp(3:2:nn)) ./ 2;
      x = [x; xtemp]; y = [y; ytemp];   % Keep these.
      clist = [clist; c .* ones(2*n-1, 1)];
   end
   ax = axis; 
   xmin = ax(1); xmax = ax(2); ymin = ax(3); ymax = ax(4);
   xrange = xmax - xmin; yrange = ymax - ymin;
   xylist = (x .* yrange + sqrt(-1) .* y .* xrange);
   disp(' ');
   disp('   Carefully select contours for labeling.')
   disp('   When done, press RETURN while the Graph window is the active window.')
end
k = 0; n = 0; flip = 0;
ENT = 3;   % Ascii for "ENTER" key.
CR = 13;   % Ascii for "RETURN" key.
while (1)
% Use GINPUT and select nearest point if manual.
   if manual
      [xx, yy, button] = ginput(1);
      if isempty(button), break, end
      if button == ENT | button == CR, break, end
      if xx < xmin | xx > xmax, break, end
      if yy < ymin | yy > ymax, break, end
      xy = xx .* yrange + sqrt(-1) .* yy .* xrange;
      dist = abs(xylist - xy);
      f = find(dist == min(dist));
      if length(f) > 0
         f = f(1); xx = x(f); yy = y(f); c = clist(f);
         okay = 1;
        else
         okay = 0;
      end
   end
% Select a labeling point randomly if not manual.
   if ~manual
      k = k + n + 1; if k > ncs, break, end
      c = cs(1, k); n = cs(2, k);
      if choice
         f = find(abs(c-contours)/max(eps+abs(contours)) < .00001);
         okay = length(f) > 0;
        else
         okay = 1;
      end
      if okay
         j = fix(rand .* (n - 1)) + 1;
         if flip, j = n - j; end
         flip = ~flip;
         x1 = cs(1, j+k); y1 = cs(2, j+k);
         x2 = cs(1, j+k+1); y2 = cs(2, j+k+1);
         xx = (x1 + x2) ./ 2; yy = (y1 + y2) ./ 2;   % Test was here; removed.
      end
   end
% Label the point.
   if okay
      s = [' ' sprintf('%0.5g', c)];
      plot(xx, yy, '+');
	text(xx, yy, s, 'verticalalignment', 'bottom', 'horizontalalignment', 'left');
   end
end
if ~hold_state, set(cax,'NextPlot',next); end