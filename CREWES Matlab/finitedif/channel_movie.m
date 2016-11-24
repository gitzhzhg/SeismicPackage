% CHANNEL: model a channel beneath a few layers
%
% low velocity channel beneath a v(z) medium
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

width=100;

dx=5;xmax=2500;zmax=1000;%grid size, max line length, max depth
x=0:dx:xmax;z=0:dx:zmax; % x and z coordinate vector
vhigh=3000;vlow=2000;vrange=vhigh-vlow; % high and low velocities
vel=vlow*ones(length(z),length(x));%initialize velocity matrix
z1=100;z2=200;v1=vlow+vrange/5;%first layer
xpoly=[-dx xmax+dx xmax+dx -dx];zpoly=[z1 z1 z2 z2];
vel=afd_vmodel(dx,vel,v1,xpoly,zpoly);%install first layer
z3=271;v2=vlow+2*vrange/5;zpoly=[z2 z2 z3 z3];%second layer
vel=afd_vmodel(dx,vel,v2,xpoly,zpoly);%install second layer
z4=398;v3=vlow+pi*vrange/5;zpoly=[z3 z3 z4 z4];%third layer
vel=afd_vmodel(dx,vel,v3,xpoly,zpoly);%install third layer
zpoly=[z4 z4 zmax+dx zmax+dx];%last layer
vel=afd_vmodel(dx,vel,vhigh,xpoly,zpoly);%install last layer
thk=50;vch=vlow+vrange/6;%channel
xpoly=[xmax/2-width/2 xmax/2+width/2 xmax/2+width/2 xmax/2-width/2];
zpoly=[z4 z4 z4+thk z4+thk];
vel=afd_vmodel(dx,vel,vch,xpoly,zpoly);%install channel
plotimage(vel-.5*(vhigh+vlow),z,x);%plot the velocity model

%do a finite-difference exploading reflector model
dt=.004; %temporal sample rate
dtstep=.001; %modelling step size
tmax=2*zmax/vlow; %maximum time
snap1=zeros(size(vel));
snap2=snap1;
snap2(1,75)=1;
M=afd_movie(dx,dtstep,tmax,vel,snap1,snap2,2,40);

%a shot record left of the channel
%snap1=zeros(size(vel));
%snap2=snap1;
%snap2(1,75)=1;
%[shotf,shot,tshot]=afd_shotrec(dx,dtstep,-dt,tmax,...
%	vel,snap1,snap2,x,zeros(size(x)),[10 15 40 50],0,1);

%plot the seismogram
%plotimage(shotf,tshot,x)


%a shot record over the channel
%snap1=zeros(size(vel));
%snap2=snap1;
%snap2(1,125)=1;
%[shotf2,shot2,tshot]=afd_shotrec(dx,dtstep,-dt,tmax,...
%	vel,snap1,snap2,x,zeros(size(x)),[10 15 40 50],0,1);


%plot the seismogram
%plotimage(shotf2,tshot,x)