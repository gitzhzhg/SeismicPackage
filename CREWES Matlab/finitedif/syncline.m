%%
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

global SCALE_OPT
SCALE_OPT=2;
% buried synlcine (focal point below the surface)
%see help for synclinemodel to understand the next few parameters
dx=5;xmax=2000;
zmax=1000;
vhigh=4000;
vlow=2000;
zsyncline=300;
zfocal=100;
radius=500; %radius+zfocal should exceed zsyncline
[vel,x,z]=synclinemodel(dx,xmax,zmax,vhigh,vlow,zsyncline,zfocal,radius);
figure
imagesc(x,z,vel);colorbar
title(['Syncline model zfocal= ' num2str(zfocal) 'm'])
% prepare for finite difference modelling. See help for afd_explode
dt=.004;
dtstep=.0005;
tmax=1.5;
%wavelet
[w,tw]=ricker(dt,30,.2);
%
[seis1,seis_unfiltered,t]=afd_explode(dx,dtstep,dt,tmax,vel,x,zeros(size(x)),w,tw,2);
% [seis,t]=afd_explode_alt(dx,dtstep,dt,tmax,vel,x,zeros(size(x)),w,tw,2);
plotimage(seis1,t,x)
title(['Syncline with focal depth at z= ' num2str(zfocal) 'm'])
%%
% focal point above surface
global SCALE_OPT
SCALE_OPT=2;
dx=5;xmax=2000;
zmax=1000;
vhigh=4000;
vlow=2000;
zsyncline=300;
zfocal=-100;
radius=500; %radius+zfocal should exceed zsyncline
[vel,x,z]=synclinemodel(dx,xmax,zmax,vhigh,vlow,zsyncline,zfocal,radius);
figure
imagesc(x,z,vel);colorbar
title(['Syncline model zfocal= ' num2str(zfocal) 'm'])
% prepare for finite difference modelling
dt=.004;
dtstep=.0005;
tmax=1.5;
%wavelet
[w,tw]=ricker(dt,30,.2);
%
[seis2,seis_unfiltered,t]=afd_explode(dx,dtstep,dt,tmax,vel,x,zeros(size(x)),w,tw,2);
% [seis,t]=afd_explode_alt(dx,dtstep,dt,tmax,vel,x,zeros(size(x)),w,tw,2);
plotimage(seis2,t,x)
title(['Syncline with focal depth at z= ' num2str(zfocal) 'm'])
%%
% buried sinusoidal syncline
global SCALE_OPT
SCALE_OPT=2;
dx=5;xmax=2000;
x=0:dx:xmax;
zmax=1000;
z=0:dx:zmax;
vhigh=4000;
vlow=2000;
wavelength=.5*xmax;
zsyncline=500;%mean depth of the sinusoid
zamp=200;%amplitude of the sinusoid.
ztop=zamp*cos((x-mean(x))*2*pi/(wavelength))+zsyncline;
xpoly=[x fliplr(x)];
zpoly=[zeros(size(x)) ztop];
vel=vhigh*ones(length(z),length(x));
vel=afd_vmodel(dx,vel,vlow,xpoly,zpoly);
figure
imagesc(x,z,vel);colorbar
title(['Co-Syncline model'])
% prepare for finite difference modelling
dt=.004;
dtstep=.0005;
tmax=1.5;
%wavelet
[w,tw]=ricker(dt,30,.2);
%
[seis3,seis_unfiltered,t]=afd_explode(dx,dtstep,dt,tmax,vel,x,zeros(size(x)),w,tw,2);
% [seis,t]=afd_explode_alt(dx,dtstep,dt,tmax,vel,x,zeros(size(x)),w,tw,2);
plotimage(seis3,t,x)
title(['Co-Syncline model'])
%%
% un-buried sinusoidal syncline
global SCALE_OPT
SCALE_OPT=2;
dx=5;xmax=2000;
x=0:dx:xmax;
zmax=1000;
z=0:dx:zmax;
vhigh=4000;
vlow=2000;
wavelength=.5*xmax;
zsyncline=150;%mean depth of the sinusoid
zamp=100;%amplitude of the sinusoid.
ztop=zamp*cos((x-mean(x))*2*pi/(wavelength))+zsyncline;
xpoly=[x fliplr(x)];
zpoly=[zeros(size(x)) ztop];
vel=vhigh*ones(length(z),length(x));
vel=afd_vmodel(dx,vel,vlow,xpoly,zpoly);
figure
imagesc(x,z,vel);colorbar
title(['Co-Syncline model'])
% prepare for finite difference modelling
dt=.004;
dtstep=.0005;
tmax=1.5;
%wavelet
[w,tw]=ricker(dt,30,.2);
%
[seis3,seis_unfiltered,t]=afd_explode(dx,dtstep,dt,tmax,vel,x,zeros(size(x)),w,tw,2);
% [seis,t]=afd_explode_alt(dx,dtstep,dt,tmax,vel,x,zeros(size(x)),w,tw,2);
plotimage(seis3,t,x)
title(['Co-Syncline model'])