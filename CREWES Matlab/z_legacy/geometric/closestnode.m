function inode=closestnode(x,y,xnot,ynot)
% inode=closestnode(x,y,xnot,ynot)
% 
% Given a piecewise linear curve whose nodes are defined by the vectors x & y
% CLOSESTNODE returns the index of the node on the curve which is closest to
% the point (xnot,ynot)
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
n=length(x);
% test for equality
ind= find(x==xnot);
if(~isempty(ind))
	ind2=find(y(ind)==ynot);
	if(~isempty(ind2))
			inode=ind(ind2);
			return;
	end
end
% find the points which bracket xnot,ynot
indx = surround(x,xnot);
indy = surround(y,ynot);
for k=1:length(indx)
	if isempty(indy) 
		it=find(isempty(indx(k)));
	else
		it=find(indx(k)==indy);
	end
	if ~isempty(it)
		itest=indx(k);
		break;
	end
end
if(isempty(it))
	% if no points bracket (xnot,ynot) then it is way exterior to the curve
	% we have no choice but to compute the distances to the various nodes and find 
	% the minimum
	d=sqrt( (x-xnot).^2 + (y-ynot).^2 );
	[d,ind]=sort(d);
	inode=ind(1);
	return;
else
	itest=[itest itest+1];
	d=sqrt( (x(itest)-xnot).^2 + (y(itest)-ynot).^2 );
	ind=find(d==min(d));
	inode=itest(ind(1));% just in case we get two minima
	return;
end
	