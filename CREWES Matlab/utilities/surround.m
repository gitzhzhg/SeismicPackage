function ind=surround(x,xtest)
% SURROUND: analyze how a vector surrounds some test points
%
% ind=surround(x,xtest)
%
% SURROUND returns a vector of indicies indicating how the vector
% x surrounds xtest which must be a scalar. If 
% isempty(ind) then xtest lies outside the range of x. Otherwise,
% ind will be the index of a point in x just greater (or less) than 
% xtest. Thus the following will be true:
%	x(ind) <= xtest < x(ind+1)
%		or
%	x(ind) >= xtest > x(ind+1)
%
% So, for if xtest is an interior point for the vector x,
% ind and ind+1 select those points in x which surround (or bracket)
% xtest. Note that x need not be monotonic. If the xtest is surrounded
% more than once by x, then ind will be a vector.
% example: x=1:10;
% >>surround(x,-1)
%    returns []
% >>surround(x,3)
%   returns 3
% now let x=[1:10 9:-1:1]
% >>surround(x,3)
%    returns 3 17
% >>surround(x,pi)
%   returns 3 16
%
% G.F. Margrave
% Jan 1994, revised Jan 95
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

	
	n=length(x);
	x1=x(1:n-1);
	x2=x(2:n);
	
	ind=find( (x1<=xtest & x2>xtest ) | ...
			(x1>=xtest & x2 < xtest ) );