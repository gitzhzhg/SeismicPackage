function a=xy2arclen(x,y,xnot,ynot)
% a=xy2arclen(x,y,xnot,ynot)
% a=xy2arclen(x,y,xnot)
% a=xy2arclen(x,y)
%
% Given a piecewise linear curve whose nodes are specified by the vectors x & y
% XY2ARCLEN, in the form a=xy2arclen(x,y,xnot,ynot), computes the arclength 
% (or inline distance) from (x(1),y(1)) to each of the points given by the 
% vectors (xnot,ynot). If (xnot,ynot) does not lie on the curve then a NAN 
% is returned. In the form, a=xy2arclen(x,y), the arclength to each of the
% verticies of (x,y) is returned. Lastly, in the form a=xy2arclen(x,y,xnot)
% the arclength to each point on the curve with x coordinate xnot is
% returned.  In this form, xnot must be a single scalar value and the length
% of a will be greater than 1 if the curve is multi-valued. The 2 argument
% form is the most efficient since no validity checking must be done to
% determine if points lie on the curve or not.
%
% G.F. Margrave December 1993
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
if( nargin == 2)
	n=length(x);
	xx=x(2:n);
	yy=y(2:n);
	d=sqrt( (x(1:n-1)-xx).^2 + (y(1:n-1)-yy).^2 );
	ind=find(isnan(d));
	if(length(ind)>0)
		d(ind)=zeros(size(ind));
	end
	a=zeros(size(x));
	a(2:n)=cumsum(d);
	return;
end
if( nargin == 3 )
	if(length(xnot)>1)
		error('xnot must be a scalar when three arguments are supplied');
	end
	ynot=ycurve(x,y,xnot);
	if(length(ynot)>1)
		xnot=xnot*ones(size(ynot));
	end
end
n=length(xnot);
anodes=xy2arclen(x,y);
a=zeros(size(xnot));
for k=1:n
	m=oncurve(x,y,xnot(k),ynot(k));
	
	if(m==0)
		a(k)=NaN;
	else
	 % note the explicit use of m(1) here. If m has more than one value, then
	 % the curve is multivalued and that will not be handled correctly except int
	 % the case of logsec segments which are linked at the multi valued points
		ainc=sqrt( (x(m(1))-xnot(k)).^2 + (y(m(1))-ynot(k)).^2);
		a(k)=anodes(m(1))+ainc;
	end
	
end
	