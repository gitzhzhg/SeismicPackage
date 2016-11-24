function zout = ungrid( arg1,arg2, arg3, arg4,arg5)
% zout = ungrid( objin, x, y)
% or
% zout = ungrid( xgrid,ygrid,zgrid,x,y )
%
% ungrid interpolates values at arbitrary (x,y) locations within a grid,
% which is essentially the inverse operation to gridding.
% This is done by interpolating from the grid to each of the distributed 
% points. The interpolation algorithm used fits a local
% polynomial surface to the grid at each interpolation site.
%
% objin = input grid object containing the grid to be evaluated as its
%			first grid.
% x = vector of x coordinates of the distributed points
% y = vector of y coordinates of the distributed points
% xgrid = grid or vector of x coordinates of the grid. if a vector then its length
%		must be the same as the number of columns in zgrid
% ygrid = grid or vector of y coordinates of the grid. if a vector then its length
%		must be the same as the number of rows in zgrid
% zgrid = grid of z values
%
% zout = vector of interpolated values at the locations: (x,y)
%
% G.F. Margrave
%      October, 1993
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
% set defaults
	if( nargin == 3 | nargin == 4)
		if( nargin < 4) npoly = 3; end
		objin = arg1;
		x = arg2;
		y = arg3;
	elseif( nargin > 4 )
		xg=arg1;
		yg=arg2;
		zg=arg3;
		x=arg4;
		y=arg5;
		if( nargin < 6) npoly = 3; end
		if( isvec(xg) ) delx = xg(2)-xg(1); xnot = xg(1);
		else delx = xg(1,2)-xg(1,1); xnot = xg(1,1); end
		if( isvec(yg) ) dely = yg(2)-yg(1); ynot = yg(1);
		else dely = yg(2,1)-yg(1,1); ynot = yg(1,1); end
		objin = gridobj('Ungrid',zg,'Grid',delx,dely,xnot,ynot);
		xg=[];yg=[];zg=[];
	end
	
	arg1=[];
	arg2=[];
	arg3=[];
	arg4=[];
	arg5=[];
			
	% next line correct only for 3rd order fit
	ptsneeded = 10; % need this many pts for the fit
	
% get the grid and the geometry
	gridin = objget(objin,1);
	xg = objget(objin,'xg');
	yg = objget(objin,'yg');
	delx = objget(objin,'delx');
	dely = objget(objin,'dely');
% scale the x and y coordinates
	mnx=min(xg(:));
	mny=min(yg(:));
	xg=xg-mnx;
	x=x-mnx;
	yg=yg-mny;
	y=y-mny;
% determine log factor
	fact = round(log10(max(xg(:))));
	fact = 10.^(fact-1);
	xg=xg/fact;
	yg=yg/fact;
	x=x/fact;
	y=y/fact;
	delx=delx/fact;
	dely=dely/fact;
	
% loop over the distributed points
	zout = zeros(size(x));
	for k = 1:length(x)
		
		 % find enough points
		 ptsfound = 0;
		 xdist = delx;
		 ydist = dely;
		 
		 while( ptsfound < ptsneeded)
		 	
		 	ind = between( x(k)-xdist, x(k)+xdist, xg);
		 	ind2 = between( y(k)-ydist, y(k)+ydist, yg(ind) );
			ind = ind(ind2);
		 	ind2 = find(~isnan(gridin(ind)));
			ind = ind(ind2);
		 	
		 	ptsfound =length(ind);
		 	xdist = xdist+delx;
		 	ydist =ydist+dely;
		 	
		 end
		 
		 % determine the polynomial
		 
		 c = polysurf( xg(ind),yg(ind),gridin(ind),npoly );
		 
		 [xx,yy,zz] = poly2d( c,3,x(k),y(k) );
		 zout(k) = zz;
		 		 
	end
		 	
		 
		 	
		 	