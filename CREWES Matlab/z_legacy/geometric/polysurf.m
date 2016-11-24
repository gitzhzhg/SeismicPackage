function c=polysurf(x,y,z,order,w)
% fit a 2-D polynomial surface to the data
%
% c=polysurf(x,y,z,order,w)
% c=polysurf(x,y,z,order)
%
% x = vector containing the x coordinates of the data
%			 
% y = vector containing the y coordinates of the data
%				  
% z = vector containing the z coordinates of the data
%				
% c= vector of 2-D polynomial coefficients
%
% w = vector of weights for the data points
%   ****** default = ones(size(z)) *******
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
%generate the 2-D Vandermonde matrix of order=order
numcols = sum( 1:order+1 );
V = zeros(length(z),numcols);
 
colcount = 1;
for n=order:-1:0
   for m=0:n
         V(:,colcount) = (x(:).^(n-m)).*(y(:).^m);
         colcount=colcount+1;
   end
end
% generate a matrix of weights and apply them to the Vandermonde matrix
if( nargin > 4 )
	[nr,nc]=size(V);
	[wr,wc]=size(w);
	if(wr==1) w=w';end
	z = z.*w;
	w=w*ones(1,nc);% each row gets a constant weight
	V=V.*w; % apply with array multiply
end
% now we have generated the matrix V necessary for the equation
% Vc=z where c is the unknown vector of 2-d polynomial coefficients
% The solution is:
% z=z.';
c = V\z;
 
 