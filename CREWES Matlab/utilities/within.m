function result=within(xpt,ypt,xpoly,ypoly)
% WITHIN: test a point to see if it is inside a polygon or not
%
% result = within(xpt,ypt,xpoly,ypoly)
%
% function returns 1 if (xpt,ypt) is inside the polygon described by
% xpoly and ypoly and 0 otherwise
%
%  xpt = x coordinates of the test points. May be a scalar, vector or matrix.
%  ypt = y coordinates of the test pts
%  xpoly = vector of the x coordinates of the polygon vertices
%          The last vertex is tested against the first and is discarded
%          if it is identical
% ypoly =  vector of the y coordinates of the polygon vertices
%
% result =  vector of boolean values the same size as xpt(:). Points inside
%           the polygon may be addressed as xpt(result) and ypt(result) while
%           those outside are xpt(~result) and ypt(~result)
%
%  Transcribed from FORTAN by G.F. Margrave, October 1993
%  FORTRAN written by T.N. Bishop, May 1989
%  Algorithm after B.J. Larkin, Computer & Geosciences, Vol 14, pp1-14, 1988
%  Original algorithm due to K.R. Anderson, Mathematical Geology, Vol 8, no 1,
%  1976, pp 105-106
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


	[m,n]=size(xpoly);
	if( m~= 1) xpoly = xpoly'; end % make sure we have row vectors
	[m,n]=size(ypoly);
	if( m~= 1) ypoly = ypoly'; end % make sure we have row vectors

% determine the number of vertices

	nvert = length(xpoly);
	if( nvert ~= length(ypoly) )
		error( 'xpoly and ypoly must be the same length' );
	end

% check for last pt == first pt
	if( (xpoly(1)==xpoly(nvert))&(ypoly(1)==ypoly(nvert)))
		xpoly = xpoly(1:nvert-1);
		ypoly = ypoly(1:nvert-1);
		nvert=nvert-1;
	end 

	if( nvert < 3 )
		error( 'polygon must have more than 2 vertices');
	end

	
% count number of polygon sides to the right of the point

	xprev = [xpoly(nvert) xpoly(1:nvert-1)];
	yprev = [ypoly(nvert) ypoly(1:nvert-1)];

	xpt=xpt(:);ypt=ypt(:);
	result=zeros(size(xpt)); % guilty until proven innocent
	
	for n=1:length(result)

	for k=1:nvert
		test1 = ypt(n) > yprev(k);
		test2 = ypt(n) <= ypoly(k);
		if( test1 == test2 )
			grad = (xpoly(k)-xprev(k))/(ypoly(k)-yprev(k));
			xint = xprev(k) - grad*yprev(k);
			xcut = grad*ypt(n)+xint;
			if( xpt(n) < xcut ) result(n) = ~result(n); end
		end
	end

	end