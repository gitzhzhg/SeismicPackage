function flag = ccw(pt1,pt2,pt3)
% flag = ccw(pt1,pt2,pt3)
%
% CCW is an implementation of Sedgewick's algorithm (Algorithm's in C++,
% Robert Sedgewick, 1992, Addison-Wesley, p350) for determining the sense of
% rotation when traveling from pt1 to pt2 to pt3 (these are points in the
% x,y plane). If this travel results in rotation through a counter clockwise
% angle then +1 is returned which -1 is returned for a clockwise angle. A
% special case is when the three points are colinear. In this case, if pt1
% is between pts 2&3 then -1 is returned, if pt2 is between pts 1&3 then +1
% is returned, and if pt3 is between pts 1&2 then 0 is returned. 
% 
% Note that each point is a 2 element vector giving x first then y.
%
% G.F. Margrave January 1994
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
dx1=pt2(1)-pt1(1); dy1=pt2(2)-pt1(2);
dx2=pt3(1)-pt1(1); dy2=pt3(2)-pt1(2);
if( dx1*dy2 > dy1*dx2 ) flag=1; return; end
if( dx1*dy2 < dy1*dx2 ) flag=-1; return; end
if( (dx1*dx2<0) | (dy1*dy2<0) ) flag=-1; return; end
if( (dx1*dx1+dy1*dy1) < (dx2*dx2+dy2*dy2) ) flag=1; return; end
flag=0;