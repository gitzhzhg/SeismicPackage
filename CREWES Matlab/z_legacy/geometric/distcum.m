function s=distcum(x,y)
% s=distcum(x,y)
% s=distcum(x)
%
% If the first form is used, x and y represent the 2-D coordinates of a set of
% points. If the second form is used, x represents an incremental distance 
% vector computed from the point coordinates by distinc. The return value, s,
% is the cumulative distance for the set of points. That is, s(j) represents
% the distance measured along the point set from the jth point to the first
% point.
% See also distpoint and distreach
%
% x = vector containing the incremental distances as computed by distinc
%
% s = cumulative distance vector containing the distance between each point
%      and the first point. 
%     Note: length(s) == length(x). s(1) == 0, s(2) == ( distance from
%     point 2 to point 1), s(j) = ( distance from point j to point 1 )
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
% 
if nargin > 1
	d = distinc(x,y);
else
	d=x;
end
s=zeros(size(d));
for j=2:length(d)
	s(j) = s(j-1)+d(j);
end
 
 