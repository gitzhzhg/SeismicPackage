function s=distinc(x,y)
% s=distinc(x,y)
%
% This function computes the incremental distance between each pair of points
%  represented by the vectors x and y. See also distcum distpoint and distreach
%
% x = vector containing the x coordinates of the data
%				 ******* default= no field set *********
% y = vector containing the y coordinates of the data
%				 ******* default= no field set *********
% s = distance vector containing the distance between each pair of points in
%     x and y. Note: length(s) == length(x). s(1) == 0, s(2) == ( distance from
%     point 2 to point 1), s(j) = ( distance from point j to point j-1 )
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
[a,b]=size(x);
x=x(:);
y=y(:);
n=length(x);
s=zeros(size(x));
s(2:n) = (x(2:n)-x(1:n-1)).^2  +  (y(2:n)-y(1:n-1)).^2;
s=s.^(.5);
s=reshape(s,a,b);
 
 