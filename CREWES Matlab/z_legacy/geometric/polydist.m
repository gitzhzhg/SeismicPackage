function dmin = polydist(x,y,datum,icol)
% dmin = polydist(x,y,datum,icol)
% dmin = polydist(x,y,datum)
%
% POLYDIST is the minimum distance from a polynomial to a datum line
%
% x,y 	= coordinate vectors of polygon 
% datum = 4-vector, [x1 y1 x2 y2] definining 2 pts on datum
% icol 	= color for plotting, 'y','m','c','r','g', or 'b'
% ====================== Default = 'b' =======================
%
% T.N.Bishop  Oct.93
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
% get slope for datum
m = zeros(size(y));  %slope of datum
m = (datum(4)-datum(2))/(datum(3)-datum(1));
if(datum(3) == datum(1)) 
  m = 1/eps;
end
b = datum(2) - m*datum(1);
%find dist from each point in polygon to datum line
msq = m.^2;
denom = 1+msq;
xd=zeros(size(y));   %intercept of datum for each perpendicular
yd=zeros(size(y));   %y coord of intercept of datum 
d = zeros(size(y));  %dist.along perpendicular
%     perpendicular is perpendicular to the datum and it also
%     intersects the polygon point (x(i), y(i)).
xd =( x+m.*(y-b) )./denom;
yd =( x*m+msq*y+b )./denom;
d = sqrt((x-xd).^2 + (y-yd).^2);
dmin = min(d);
imin = find(d == dmin);
% plot segment (if icol is called by the function)
if(nargin > 3) 
  hold on
  xtmp=[x(imin) xd(imin)];
  ytmp=[y(imin) yd(imin)];
  plot(xtmp,ytmp,icol)
end