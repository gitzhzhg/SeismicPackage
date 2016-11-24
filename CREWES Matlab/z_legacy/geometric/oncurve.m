function index=oncurve(x,y,xnot,ynot)
% index=oncurve(x,y,xnot,ynot)
% 
% ONCURVE returns zero if the point (xnot,ynot) is not determined to lie on
% the piecewise linear curve represented by the vectors x & y. If
% (xnot,ynot) does lie on the curve then the returned value is the index
% such that (xnot,ynot) lies between 
% (x(index),y(index)) & (x(index+1),y(index+1)). 
% (If (xnot,ynot) is exactly equal to a point in (x,y) then index gives
% that point.
%
% G.F. Margrave, December 1993
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
it=[];
%t=clock;
n=length(x);
index=0; % assume false
closenough=10*eps;
% test for equality
ind= find(x==xnot);
if(~isempty(ind))
	ind2=find(y(ind)==ynot);
	if(~isempty(ind2))
			index=ind(ind2);
			return;
	end
end
% find the points which bracket xnot,ynot
indx = surround(x,xnot);
indy = surround(y,ynot);
%check for a vertical segment
if isempty(x(indy+1))
	if isempty(x(indy))
		indx=indy;
	end
elseif ~isempty(x(indy+1))
	if x(indy)==x(indy+1)
		indx=indy;
	end
end
for k=1:length(indx)
	it=find(indx(k)==indy);
	if(~isempty(it))
		itest=indx(k);
		break;
	end
end
if(isempty(it))
	return;
end
	
%compute perpedicular distance
if( x(itest)~=x(itest+1) )
	m=(y(itest+1)-y(itest))./(x(itest+1)-x(itest));
	b=y(itest)-m.*x(itest);
	dtest=abs(m*xnot-ynot+b)./sqrt(m.*m+1);
else
 dtest=abs(xnot-x(itest));
end
d=min(dtest);
	
if(d<closenough)
		it=find(d==dtest);
		index=itest(it);
		%etime(clock,t)
		return;
end
	
	