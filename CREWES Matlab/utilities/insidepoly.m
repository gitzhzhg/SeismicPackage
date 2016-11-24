function k = insidepoly(z,w);
% INSIDEPOLY: identify points inside a polygonal region in 2D.
%   k = inside(z,w) is a vector of indices.
%   The points z(k) are strictly inside the region defined by w.
%   Here, z is a complex vector of points in the plane, and
%   w is a complex vector of points definining the vertices of a
%   polygonal region in the plane.  The region should be "starlike"
%   wth respect to the "center", i.e. any ray eminating from
%   mean(w) should intersect the boundary only once.
%   Convex regions satisfy this requirement.
%   For example, the vertices of the unit square are
%   w = [0 1 1+i i] and k = inside(z,w) is the same as
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

%   k = find((real(z)>0) & (real(z)<1) & (imag(z)>0) & (imag(z)<1))

%   Copyright (c) 1984-98 by The MathWorks, Inc.
%   $Revision: 1.1 $  $Date: 2002/05/28 17:13:21 $
%   Modified, June 2000, by G.F. Margrave (lines 40 & 54)

% Translate both z and w so that the region center is at the origin.
w0 = mean(w);
w = w - w0;
z = z - w0;

% Describe the polygon in polar coordinates with sorted angles.
% Duplicate two vertices so that all of -pi <= theta <= pi is included.
n = length(w);
if w(1) == w(n)
    w(n) = [];
    n = n-1; 
end
th = atan2(imag(w),real(w));
[th,p] = sort(th(:));
w = w(p);
th = [th(n)-2*pi; th; th(1)+2*pi];
w = [w(n); w; w(1)];

% Use polar coordinates for z's also.
% Compute s = [cos(t) sin(t)] 
t = atan2(imag(z),real(z));
s = z./(abs(z)+100*eps);
s = [real(s) imag(s)];

% Now check points inside each wedge shaped region determined
% by rays from the origin through two consecutive vertices.
% Determine the constants in the polar equation of the line segment from
% each vertex to the next, rho = 1/(c(1)*cos(theta) + c(2)*sin(theta)).
% Find where rays through the points intersect the line segments
% and determine which points are inside the truncated wedge.
k = [];
for j = 1:n+1;
   p = find((t >= th(j) & (t < th(j+1))));
   if length(p) > 0
      c = [real(w(j:j+1)) imag(w(j:j+1))] \ [1; 1];
      r = 1 ./ (s(p,:)*c+100*eps);
      q = find(abs(z(p)) < r);
      k = [k; p(q)];
   end
end
k = sort(k);