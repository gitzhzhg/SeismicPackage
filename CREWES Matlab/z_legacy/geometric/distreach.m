function [s,indicies]=distreach(x,y,xnot,ynot,r)
% [s,indicies]=distreach(x,y,xnot,ynot,r)
%
% This function searches the point set represented by x,y to find those points
% which lie within a radius r of the single point xnot,ynot. The distances of
% the found points are returned in s and their indicies in indicies. Thus, the
% number of found points is length(s) and they are identified as:
% x(indicies),y(indicies) is the set of points whose distances from xnot,ynot
% are given by s <= r.
% See also distcum distinc and distpoint
%
% x = vector containing the x coordinates of the data points
%
% y = vector containing the y coordinates of the data points
%
% xnot = scalar x coordinate of the single point
%
% ynot = scalar y coordinate of the single point
%
% r= radius of the search region (also called the reach)
%
% s = distance vector containing the distance between each point in
%     x and y and the point xnot,ynot
%
% by G.F. Margrave, March 1993
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
% do first sort
xmin = xnot-r;xmax=xnot+r;
ymin= ynot-r; ymax= ynot+r;
ind = find((x<=xmax)&(x>=xmin));
ind = find((y(ind)<=ymax)&(y(ind)>=ymin));
s=distpoint(x(ind),y(ind),xnot,ynot);
indicies = find( s<= r);
s=s(indicies);
indicies=ind(indicies); 
 