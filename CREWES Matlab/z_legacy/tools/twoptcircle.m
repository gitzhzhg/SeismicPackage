function [xc,yc,radius] = twoptcircle(xp,yp,n)
% [xc,yc,radius] = twoptcircle(xp,yp,n)
% [xc,yc,radius] = twoptcircle(xp,yp)
%
% TWOPTCIRCLE takes two points, and makes a circle with point 1 as the
% center, and point 2 defining the radius.
%
% xp	= x-coordinates for point1 and point2, i.e. x(1) and x(2)
% yp	= y-coordinates for point1 and point2, i.e. y(1) and y(2)
% n	= number of points on the circle
% **************************** Default = 30 *******************************
%
% xc	= x-coordinates for points on circle
% yc	= y-coordinates for points on circle
%
%  T. N. BISHOP,  OCTOBER 1993,  CPTC CANADA
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
if nargin<3
	n=30;
end
radius = sqrt( (xp(2)-xp(1)).^2 + (yp(2)-yp(1)).^2 );
for i = 1:n
  t(i) = (i-1)*2.*pi/n;
end
t = [t,t(1)];
  xc=xp(1) + radius*cos(t);
  yc=yp(1) + radius*sin(t);