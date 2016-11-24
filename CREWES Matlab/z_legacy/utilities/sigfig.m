function [xx,xp,e]=sigfig(x,n)
% [xx,xp,e]=sigfig(x,n)
%
% SIGFIG returns x rounded to n significant figures
% (works for vectors)
% 
% xx ... x rounded to n sig figures
% xp and e ... xx==xp*10^e where 0<abs(xp)<1000
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
sgn=zeros(size(x));
xx=zeros(size(x));
xp=xx;
e=xx;
ind=find(x~=0.);
sgn(ind)=x(ind)./abs(x(ind));
xl=log10(abs(x(ind)));
ee= fix(xl);
if(xl<0)
	tmp=10.^(xl-ee+n);
	xx(ind)=round(tmp).*10.^(ee-n);
	xl=log10(x(ind));
	e(ind)=fix(xl);
	xp(ind)=10.^(xl-ee);
else
	tmp=10.^(xl-ee+n-1);
	xx(ind)=round(tmp).*10.^(ee-n+1);
	xl=log10(x(ind));
	e(ind)=fix(xl);
	xp(ind)=10.^(xl-ee);
end
xx=xx.*sgn;
xp=xp.*sgn;