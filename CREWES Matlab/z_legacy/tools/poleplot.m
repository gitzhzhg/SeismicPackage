function poleplot(x,y,z,zref)
% poleplot( x, y, z, zref)
%
% x = vector containing the x coordinates of the data
%				 ******* default= no field set *********
% y = vector containing the y coordinates of the data
%				 ******* default= no field set *********
% z = vector containing the z coordinates of the data
%				 ******* default= no field set *********
% zref = reference zlevel for drawing x,y paths
% ************* default = mean(z) *******************
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
if(nargin <4)
	zref = mean(z);
end
x=x(:);
y=y(:);
z=z(:);
n=length(z);
displayx=zeros(3,n);
displayy=zeros(3,n);
displayz=zeros(3,n);
xbar= mean(x);
ybar=mean(y);
for j=1:n
  displayx(1,j)=xbar;
  displayx(2,j)=x(j);
  displayx(3,j)=x(j);
  displayy(1,j)=ybar;
  displayy(2,j)=y(j);
  displayy(3,j)=y(j);
  displayz(1,j)=zref;
  displayz(2,j)=zref;
  displayz(3,j)=z(j);
end
plot3(displayx,displayy,displayz,'-b');
hold on;
index=find(z>zref);
plot3(x(index),y(index),z(index),'m*');
index=find(z<=zref);
plot3(x(index),y(index),z(index),'r*');
hold off;