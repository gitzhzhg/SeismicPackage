function [jmin,jmax] = polyind(ipoly,x,y)
% [jmin,jmax] = polyind(ipoly,x,y)
%
% x,y	= input arrays of polygon coordinates
% ipoly = input desired index of polygon is 
% 
% POLYIND returns:
% jmin 	= index of the first point
% jmax 	= index of the last point
%   
% Tom Bishop, Oct.93
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
% compute distmax
distmax=.05;    %threshold of 5 percent of picture
scalex=1./(max(x)-min(x));
scaley=1./(max(y)-min(y));
j=0;
distnext=distmax;
%
%loop thru all polygons
%
for loop = 1:ipoly
  jmin = j+1;  %reset from last polygon
  dist = distmax + 1;   % dont want 2nd point 
  j=jmin+1;
  while (dist > distmax) | (dist >= distnext) 
    j=j+1;
    if(j > length(x))
%      fprintf('reached end of array, length = %d\n',length(x))
      jmax = length(x);
      return
    end
    distx = scalex*(x(j)-x(jmin));
    disty = scaley*(y(j)-y(jmin));
    dist  = sqrt(distx.^2 + disty.^2);
    jnext = min(j+1,length(x));  %dont run off end of array
    distxnext = scalex*(x(jnext)-x(jmin));
    distynext = scaley*(y(jnext)-y(jmin));
    distnext  = sqrt(distxnext.^2 + distynext.^2);
%    fprintf('dist,distnext,j =  %8.4f %8.4f %d\n',...
%             dist,distnext,j) 
  end
  jmax=j;
end