function y2=paratran(x,y,x1,y1,x2)
% y2=paratran(x,y,x1,y1,x2)
%
% PARATRAN does parallel transport of a point (x1,y1) to the x coordinate
% x2 by moving parallel to the piecewise linear curve {x,y}
% If x2 lies outside the bounds of (x,y) then the curve is extended as
% needed by simple constant extrapolation of the endpoints.
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
 y1p=ycurve(x,y,x1);
 y2p=ycurve(x,y,x2);
	if(length(y1p)>1 || length(y2p)>1 )
		disp('parallel transport fails if curve is multivalued');
        disp('curve has x coordinates:')
        x
        disp('curve has y coordinates:')
        y
        error('Curve is multivalued in y, you need to fix it before continuing')
end
y2=y2p+y1-y1p;