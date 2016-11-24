function [x,y,z]=tvd_minc(zmd,azmi,incl)
% [x,y,z]=tvd_minc(zmd,azmi,incl)
%
% TVD_MINC uses the minimum curvature method to compute true 3-D coordinates
% of a wellbore path given information from a deviation survey. The method is
% documented in 'Directional Survey Calculation', by J.T. Craig and B.V. Randal
% found in 'Petroleum Engineer',March, 1976.
%
% zmd ... vector of measured depths
% azmi ... vector of azimuth angles in degrees
% incl ... vector of inclination angles in degrees
%
% G.F. Margrave
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
zmd=zmd(:);
azmi=azmi(:);
incl=incl(:);
%make sure we have a 0 zmd
pad=0;
if(zmd(1)~=0)
	zmd=[0;zmd];
	azmi=[0;azmi];
	incl=[0;incl];
	pad=1;
end
torad= pi/180.;
phi=torad*incl;
theta=torad*azmi;
npts=length(zmd);
k=2:npts;
kminus=1:npts-1;
cosd= cos(phi(k)-phi(kminus)) - sin(phi(kminus)).*sin(phi(k)).*...
		(1. - cos(theta(k)-theta(k-1)));
tand= sqrt( cosd.^(-2) -1.);
dl = atan(tand);
ind= abs(dl) > .00001;
fc=zeros(size(dl));
fc(~ind)=ones(sum(~ind),1);
fc(ind)= 2*tan(dl(ind)/2)./dl(ind);
sinphi= fc.*(sin(phi(k)).*sin(theta(k))+sin(phi(kminus)).*sin(theta(kminus)))/2;
cosphi= fc.*(sin(phi(k)).*cos(theta(k))+sin(phi(kminus)).*cos(theta(kminus)))/2;
x=zeros(size(zmd));
y=zeros(size(zmd));
z=zeros(size(zmd));
x(k)= cumsum( (zmd(k)-zmd(kminus)).*cosphi );
y(k)= cumsum( (zmd(k)-zmd(kminus)).*sinphi );
z(k)= cumsum( fc.*(zmd(k)-zmd(kminus)).*(cos(phi(k))+cos(phi(kminus)))/2.);
if(pad)
	x=x(k);
	y=y(k);
	z=z(k);
end
%flip x and y to conform with MINCOM
tmp=x;
x=y;
y=tmp;