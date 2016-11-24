function [d,xhat,yhat]=pdist(x,y,xnot,ynot)
% d=pdist(x,y,xnot,ynot)
%
% pdist computes the perpendicular distance between the the straight line
% represented by the points (x(1),y(1)) & (x(2),y(2)) and the third point
% (xnot,ynot). (xhat,yhat) is the point of intersetion between the
% perpendicular and the straight line.
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
if(abs(x(2)-x(1))>eps)
	m=(y(2)-y(1))/(x(2)-x(1));
	b=y(1)-m*x(1);
	d=abs(m*xnot-ynot+b)/sqrt(m*m+1);
	xhat=(m*(ynot-y(1))+xnot+m*m*x(1))/(m*m+1);
	yhat=m*(xhat-x(1))+y(1);
else
	xhat=x(1);
	yhat=ynot;
	d=abs(xhat-xnot);
end