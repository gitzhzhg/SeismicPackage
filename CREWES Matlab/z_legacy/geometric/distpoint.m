function s=distpoint(x,y,xnot,ynot)
% s=distpoint(x,y,xnot,ynot)
%
% This function computes the distance between each pair of points represented
% by the vectors x and y and the single point xnot,ynot. That is S(j) is the 
% distance from x(j),y(j) to xnot,ynot.
% See also distcum distinc and distreach
%
% x = vector containing the x coordinates of the data points
%
% y = vector containing the y coordinates of the data points
%
% xnot = scalar x coordinate of the single point
%
% ynot = scalar y coordinate of the single point
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
[nx,mx]=size(x);
[ny,my]=size(y);
if (nx~=ny) | (mx~=my)
	error(' Vectors must be same length');
end
s=zeros(nx);
s(1:nx,1:mx) = (x(1:nx,1:mx)-xnot).^2  +  (y(1:nx,1:mx)-ynot).^2;
s=s.^.5;
 
 