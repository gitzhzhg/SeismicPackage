function [xnot,ynot]=arclen2xy(x,y,a)
% [xnot,ynot]=arclen2xy(x,y,a)
%
% Given a piecewise linear curve whose nodes are described by the vectors
% x & y, ARCLEN2XY computes the (x,y) coordinates of the points whose
% arclength (or inline distance) from (x(1),y(1)) is given by the vector a.
% If the arclength is too large (or negative) then NaN is returned.
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
anodes=xy2arclen(x,y);
anodes(length(anodes))=anodes(length(anodes))+5*eps;
n=length(a);
xnot=nan*ones(size(a));
ynot=nan*ones(size(a));
for k=1:n
	ind=surround(anodes,a(k));
	if(~isempty(ind))
		ind=ind(1);% make sure we take the first one
		if( ind>0 )
			factor=(a(k)-anodes(ind))/(anodes(ind+1)-anodes(ind));
			xnot(k)=x(ind)+factor*(x(ind+1)-x(ind));
			ynot(k)=y(ind)+factor*(y(ind+1)-y(ind));
		end
	end
	
end