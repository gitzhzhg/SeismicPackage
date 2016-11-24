function [vel,x,z]=dipmodel(dx,xmax,zmax,vhigh,vlow,dip,znot)
% dipmodel: build a simple 2D model of a dipping reflector
% 
% [vel,x,z]=dipmodel(dx,xmax,zmax,vhigh,vlow,dip,dnot)
%
% dx ... grid interval (distance between grid points in x and z)
% xmax ... maximum x coordinate (minimum is zero)
%  *********** default 2500 **********
% zmax ... maximum z coordinate (minimum is zero)
%  *********** default 1000 ************
% vhigh ... velocity below the dipping reflector
%  *********** default 4000 ************
% vlow ... velocity above the dipping reflector
%  *********** default 2000 ************
% dip ... dip in degrees of the reflector, positive is down to the right
%  *********** default = 20 degrees ************
% znot ... depth to dipping reflector at xmax/2
%  *********** default = zmax/4 **************
%
% vel ... velocity model matrix
% x ... x coordinate vector for vel
% z ... z coordinate vector for vel
%
% NOTE: the simplest way to plot vel is: plotimage(vel-mean(vel(:)),z,x)
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

if(nargin==0)
    [vel,x,z]=dipmodel(10);
    figure
    imagesc(x,z,vel);colorbar
    title('Dip model, colors indicate velocity')
    xlabel('distance (m)')
    zlabel('depth (m)')
    clear
    return;
end

if(nargin<5)
    vlow=2000;
end
if(nargin<4)
    vhigh=4000;
end
if(nargin<3)
    zmax=1000;
end
if(nargin<2)
    xmax=2500;
end
if(nargin<6)
    dip=10;
end
if(nargin<7)
    znot=zmax/3;
end

x=0:dx:xmax; % x coordinate vector
z=0:dx:zmax; % z coordinate vector

%initialize velocity matrix as a constant matrix full of vlow
vel=vlow*ones(length(z),length(x));

% define the dipping horizon as a three point polygon
dx2=dx/2;
xnot=xmax/2;
x1=x(1)-dx2;x2=x(end)+dx2;
z1=znot+(x1-xnot)*tand(dip);
z2=znot+(x2-xnot)*tand(dip);

xpoly=[x1 xnot  x2 x2 x1];zpoly=[z1 znot z2 zmax+dx2 zmax+dx2];

% install the basement in the velocity matrix
vel=afd_vmodel(dx,vel,vhigh,xpoly,zpoly);