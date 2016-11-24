function ynot=ycurve(x,y,xnot)
% ynot=ycurve(x,y,xnot)
%
% compute the y coordinates of the curve given by x,y at the point xnot.
% If the curve is multivalued, then multiple y's will be returned. If the
% curve is a polygon, then it must have its first and last points explicitly
% equal. If xnot lies outside the bounds and the curve is not a polygon,
% then the curve is extrapolated by repeating the% first or last sample as
% needed. If it is outside the bounds and the curve is a polygon, then []
% is returned.
%
% by G.F. Margrave December 1993
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
	ind=find(x==xnot);
	if(isempty(ind))
		ind=surround(x,xnot);
		if(isempty(ind)) % test for out of range
			if( x(1)==x(length(x)) & y(1)==y(length(y)) )
				ynot=[]; % don't extrapolate a polygon
				return;
			else
				% extrapolate at constant y either the first or last point
				xends=[x(1) x(length(x))];
				yends=[y(1) y(length(y))];
				if( xnot < xends(1) & xnot< xends(2) )
					ind=find(xends==min(xends));
					ynot=yends(ind);
					return;
				else
					ind=find(xends==max(xends));
					ynot=yends(ind);
					return;
				end
			end
		end
				
		ynot=y(ind)-(x(ind)-xnot).*(y(ind)-y(ind+1))./(x(ind)-x(ind+1));
	else
		ynot=y(ind);
	end