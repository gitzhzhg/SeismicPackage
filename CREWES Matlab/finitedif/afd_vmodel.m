function vmodout=afd_vmodel(dx,vmodin,vel,xpoly,zpoly);
% AFD_VMODEL ... makes simple polygonal velocity models
%
% vmodout=afd_vmodel(dx,vmodin,vel,xpoly,zpoly);
%
% AFD_VMODEL will superimpose a polygon with a different velocity onto
% the background velocity model.  The background model may be homogenous,
% layered, or as complicated as desired.  The program will return the
% velocity model with the polygon superimposed.
% HINT:  Plot your initial velocity model using imagesc with proper x and z 
% coordinates and then use ginput to pick the points of the polygon with your
% mouse and return the coordinates of the points.   
%
% dx = the bin spacing for both horizontal and vertical (in consistent units)
% vmodin = the 'background" velocity matrix in consistent units
%         the upper left corner is (0,0).
% vel = the velocity within the polygon in consistent units (scalar)
% xpoly = a vector of the x coordinates of the polygon
%       = can be entered in consistent units or bin numbers
% zpoly = a vector of the z coordinates of the polygon
%       = can be entered in consisent units or in bin numbers
%         NOTE:  the program will trace out the polygon
%         in the order of the coordinates entered - the 
%         order of your coordinates can effect the shape
%         of the polygon.
%
% vmodout = the velocity matrix with the polygon superimposed
%
% by Carrie Youzwishen, February 1999
% completely rewritten by G.F. Margrave June 2000
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

if(size(xpoly)~=size(zpoly))
	error('xpoly must be the same size as zpoly')
end
if(length(vel)~=1)
	error('vel must be a scalar')
end

vmodout=vmodin;
[nz,nx]=size(vmodin);
x=(0:nx-1)*dx;
z=((0:nz-1)*dx);
%cpts=ones(nz,1)*x+i*(z')*ones(1,nx);
%cpoly=xpoly+i*zpoly;

%in=insidepoly(cpts(:),cpoly.');
z=z';
xx=ones(size(z))*x;
zz=z*ones(size(x));
%in=inpolygon(xx,zz,xpoly,zpoly);
in=inpolygon(xx,zz,xpoly,zpoly);
%vmodout(on)=vel;
vmodout(in)=vel;