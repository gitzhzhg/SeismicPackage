function a=integrate(u,x)
% a=integrate(u,x)
% a=integrate(u)
%
% INTEGRATE numerically integrates the vector u with respect to x.
% That means that x must be a vector of the same length as u giving
% the xcoordinates of u. If x is not supplied, one is generated as 
% x=1:length(u) which implies unit spacing between the elements of
% u. For wavelengths large compared to the grid spacing, INTEGRATE
% does an approximate job of undoing MATLAB's GRADIENT function to
% within an additive constant. The method is essentially Simpson's
% trapezoidal rule. See Numerical Recipies (2nd Ed.) eqn 4.1.11
%
%	example: 
%		x=0:2*pi/1000:2*pi;
%		y=cos(x);
%		yp=gradient(y,x);
%		yapprox=integrate(yp,x)+y(1);
%		plot(x,y,x,yp,x,yapprox,'r.');
%		rmserr=sqrt(sum((y-yapprox).^2)/(length(x)))
%
% G.F. Margrave 1994
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
	nu=length(u);
	[m,n]=size(u);
	if(nargin<2)
		x=1:nu;
		if(n==1)
			x=x';
		end
	end
	dx=x(2:nu)-x(1:nu-1);
	a=zeros(size(u));
	b=u(1:nu-1) + u(2:nu);
	a(2:nu)=b.*dx/2;
	a=cumsum(a);