function y = filttri(x,n)
% y = filttri(x,n)
%
% THIS IS BASICALLY A WEIGHTED MOVING AVERAGE OPERATOR
% x is the input vector; the no.of pts. of the vector
%    need not be specified
% n is the number of points of the triangle smoother, 
% y is the smoothed x, convolved with the
%    triangle of n pts.  Because the added edges at
%    the beginning and end are truncated, y will have
%    the same number of points as x.
%  T.N.Bishop, CCR, 4/94
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
%  compute the triangle function of n points
n1 = fix(n/2)+1;
box = ones(1,n1)/n1;
f1 = conv(box,box);
		%pad input data with end values
npad = fix(n/2);
onepad = ones(1,npad);
xpad = [x(1)*onepad x x(length(x))*onepad];
%  compute the center of the boxcar, assume n is odd
nc = fix(n/2) + 1;
%  use Gary's fct from seis.toolbox, to get same no.of
%  output points as input
y = convz(xpad,f1,nc);
		% unpad output data with end values
y = y((npad+1):(length(y)-npad));