% VZANTICLINE: model an anticline beneath a v(z) medium
%
% 
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

% Just run the script

dx=2; %cdp interval
xmax=2500;zmax=1000; %maximum line length and maximum depth

%define the v(z) medium
zlayers=[50 100 200 250 300 330 380 400];%depths to layer bottoms
vlayers=[2000 2100 2200 2400 2500 2800 3000 3100];%layer velocities

x=0:dx:xmax; % x coordinate vector
z=0:dx:zmax; % z coordinate vector

%initialize velocity matrix as a constant matrix full of v=4000
vel=4000*ones(length(z),length(x));

%loop over layers and define each
for k=1:length(zlayers)
	if(k==1) z0=0; else z0=zlayers(k-1); end
	z1=zlayers(k);
	xpoly=[-dx xmax+dx xmax+dx -dx];
	zpoly=[z0-dx z0-dx z1+dx z1+dx];
	vel=afd_vmodel(dx,vel,vlayers(k),xpoly,zpoly);
end

% define an anticline beneath the layered medium

x0=xmax/2;z0=600; % x and z of the crest of the anticline
a=.0005; % a parameter that determines the steepness of the flanks
za=a*(x-x0).^2+z0; % model the anticline as a parabola

% build a polygon that models the anticline
ind=surround(za,zmax+dx);
xpoly=[x(ind(1):ind(2)) x(ind(2)) x(ind(1)) ];zpoly=[za(ind(1):ind(2)) zmax+dx zmax+dx];

vanti=5000; %velocity of the anticline;

%install the anticline in the velocity model
vel=afd_vmodel(dx,vel,vanti,xpoly,zpoly);

% bottom layer
%xpoly=[0 xmax xmax 0];zpoly=[.9*zmax .9*zmax zmax+dx zmax+dx];

%vel=afd_vmodel(dx,vel,vhigh,xpoly,zpoly);

%plot the velocity model
global COLOR_MAP
COLOR_MAP='jet';
plotimage(vel-4000,z,x)
title('Velocity model ''vzanticline''')

%do a finite-difference model
dt=.004; %temporal sample rate
dtstep=.0005;
tmax=2*zmax/2000; %maximum time
[w,tw]=wavemin(dt,30,.2); %minimum phase wavelet
[seisfilt,seis,t]=afd_explode(dx,dtstep,dt,tmax, ...
 		vel,x,zeros(size(x)),[5 10 40 50],0,1);

%plot the seismogram
COLOR_MAP='seisclrs';
plotimage(seisfilt,t,x)
title('Exploding reflector response of ''vzanticline''')