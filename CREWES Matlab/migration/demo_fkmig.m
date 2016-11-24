%%
% migrate a single diffraction
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

% try changing fmax and dip max and observe the consequences
clear
fmax=75;
dipmax=80;
dt=.004;
tmax=2.0;
dx=5;
nx=1000;
v=2000;
x=(0:nx-1)*dx;
nt=round(tmax/dt)+1;
t=(0:nt-1)*dt;
seis=zeros(nt,nx);
tnot=.5;
xnot=max(x)/2;
seis=event_hyp(seis,t,x,tnot,xnot,v,1);
%filter
seis=filtf(seis,t,[10 5],[fmax-10 10],0);
params=nan*ones(1,13);
params(1)=fmax;%frequency limit
params(3)=dipmax;%dip limit
[seismig,tmig,xmig]=fkmig(seis,t,x,v,params);
zz=tmig*v/2;
plotimage(seismig,zz,xmig)
title('Migration')
xlabel('m');ylabel('m')
[specmig,kz,kx]=fktran(seismig,zz,xmig);
plotimage(abs(specmig),kz,kx)
title('Spectrum after migration')
xlabel('k_x m^{-1}');ylabel('k_z m^{-1}');
[fk,f,k]=fktran(seis,t,x);
plotimage(abs(fk),f,k)
%draw evanescent boundaries
fbdy=v*abs(k)/2;
ind=find(fbdy<=max(f));
h=line(k(ind),fbdy(ind),'color','r');
legend(h,'evanescent boundary')
title('Spectrum before migration')
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
plotimage(seis,t,x)
title('Diffraction before migration')
xlabel('m');ylabel('t')
%%
% use fkmig to show the impulse response of migration
% try changing fmax and dip max and observe the consequences
clear
fmax=75;
dipmax=80;
dt=.004;
tmax=2.0;
dx=5;
nx=500;
v=2000;
x=(0:nx-1)*dx;
nt=round(tmax/dt)+1;
t=(0:nt-1)*dt;
seis=zeros(nt,nx);
tmp=impulse(t);
tmp=filtf(tmp,t,[10 5],[50 10]);
seis(:,round(nx/2))=tmp;
params=nan*ones(1,13);
params(1)=fmax;%maximum frequency to migrate
params(3)=dipmax;%maximum dip to migrate
[seismig,tmig,xmig]=fkmig(seis,t,x,v,params);
plotimage(seismig,tmig,xmig)
title(['Impulse response of migration for fmax= ' int2str(fmax) 'Hz, dipmax= ' int2str(dipmax) ' degrees'])
zz=t*v/2;
[fk,kz,kx]=fktran(seismig,zz,xmig);
plotimage(abs(fk),kz,kx)
title(['Migrated spectrum for fmax= ' int2str(fmax) 'Hz, dipmax= ' int2str(dipmax) ' degrees'])
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
[fk,f,kx]=fktran(seis,t,x);
plotimage(abs(fk),f,kx)
title('Input spectrum ')
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
%draw evanescent boundaries
fbdy=v*abs(kx)/2;
ind=find(fbdy<=max(f));
h=line(kx(ind),fbdy(ind),'color','r');
legend(h,'evanescent boundary')
plotimage(seis,t,x);
title('Impulse to be migrated')

%%
% demo migration of a single live trace
% try changing fmax and dip max and observe the consequences
clear
fmax=75;
dipmax=80;
dt=.004;
tmax=2.0;
nt=round(tmax/dt)+1;
dx=5;
nx=500;
nz=500;
v=2000;
x=(0:nx-1)*dx;
seis=zeros(nt,nx);
[r,t]=reflec(tmax,dt);

s=filtf(r,t,[10 5],[50 10]);
% w=ricker(dt,40);
% s=convz(r,w);
seis(:,round(nx/2))=s(1:nt);
params=nan*ones(1,13);
params(1)=fmax;
params(3)=dipmax;%specify dip limit
[seismig,tmig,xmig]=fkmig(seis,t,x,v,params);
plotimage(seismig,tmig,xmig)
title(['Migration of one trace for fmax= ' int2str(fmax) 'Hz, dipmax= ' int2str(dipmax) ' degrees'])
zz=t*v/2;
[fk,kz,kx]=fktran(seismig,zz,xmig);
plotimage(abs(fk),kz,kx)
title(['Migrated spectrum of one trace for fmax= ' int2str(fmax) 'Hz, dipmax= ' int2str(dipmax) ' degrees'])
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
[fk,f,kx]=fktran(seis,t,x);
plotimage(abs(fk),f,kx)
title('Input spectrum ')
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
%draw evanescent boundaries
fbdy=v*abs(kx)/2;
ind=find(fbdy<=max(f));
h=line(kx(ind),fbdy(ind),'color','r');
legend(h,'evanescent boundary')
plotimage(seis,t,x);
title('Trace to be migrated')
%%
%make a standard synthetic to migrate
% try changing fmax and dip max and observe the consequences
clear
fmax=75;
dipmax=80;
dt=.004;
dx=5;
tmax=2;
xmax=2000;
v=2000;
[w,tw]=ricker(dt,30,.2);
[seis,t,x]=makestdsynh(dt,dx,tmax,xmax,v,w,tw);
%fkmig
params=nan*ones(1,13);
params(1)=fmax;
params(3)=dipmax;
[seism,tm,xm]=fkmig(seis,t,x,v,params);
plotimage(seism,tm,xm)
title(['Migration standard synthetic for fmax= ' int2str(fmax) 'Hz, dipmax= ' int2str(dipmax) ' degrees'])
% spectra
fk=fktran(seis,t,x);
[fkm,f,k]=fktran(seism,tm,xm);
plotimage(abs(fkm),f,k)
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
title(['Migrated spectrum of standard synthetic for fmax= ' int2str(fmax) 'Hz, dipmax= ' int2str(dipmax) ' degrees'])
plotimage(abs(fk),f,k)
title('Standard synthetic unmigrated spectrum')
%draw evanescent boundaries
fbdy=v*abs(k)/2;
ind=find(fbdy<=max(f));
h=line(k(ind),fbdy(ind),'color','r');
legend(h,'evanescent boundary')
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
plotimage(seis,t,x)
title('Standard synthetic before migration')
%% Diffraction section
%make a section full of diffractions
% try changing fmax and dip max and observe the consequences
clear
fmax=75;
dipmax=80;
tmax=1;
dt=.004;
t=(0:dt:tmax)';
dx=5;x=(0:200)*dx;
fmin2=[10 5];fmax2=[fmax 10];
nt=length(t);nx=length(x);
seis=zeros(nt,nx);
%determine diffraction locations
itdiff=1:50:nt;
ixdiff=1:20:nx;    
tmp=zeros(size(t));
tmp(itdiff)=1;
%bandlimit
%tmp=filtf(tmp,t,fmin,fmax);
%install
for k=ixdiff
    seis(:,k)=tmp;
end
%linear v(z) function
vc=2000;
z=0:dx:2000;
vz=vc+0.0*z;%use a zero accelerator to get constant velocity
tv=2*vint2t(vz,z);
v=interp1(tv,vz,t);%velocity in time
%set up for modelling
params=nan*(1:13);
params(1)=fmax;
params(2)=0;
params(13)=20;
[seis_diff,tmod,xmod]=vz_fkmod(seis,v,t,x,params);
%apply filter
seis_diff=filtf(seis_diff,tmod,fmin2,fmax2);
%fkmig
params=nan*ones(1,13);
params(1)=fmax;
params(3)=dipmax;
[seism,tm,xm]=fkmig(seis_diff,tmod,xmod,vc,params);
plotimage(seism,tm,xm)
title(['Migration diffraction section for fmax= ' int2str(fmax) 'Hz, dipmax= ' int2str(dipmax) ' degrees'])
% spectra
[fk,f,k]=fktran(seis_diff,tmod,xmod);
[fkm,fm,km]=fktran(seism,tm,xm);
plotimage(abs(fkm),fm,km)
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
title(['Migrated spectrum of diffraction section for fmax= ' int2str(fmax) 'Hz, dipmax= ' int2str(dipmax) ' degrees'])
plotimage(abs(fk),f,k)
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
title('Unmigrated spectrum of diffraction section')
%draw evanescent boundaries
fbdy=vc*abs(k)/2;
ind=find(fbdy<=max(f));
h=line(k(ind),fbdy(ind),'color','r');
legend(h,'evanescent boundary')
plotimage(seis_diff,tmod,xmod);
title(['Diffraction section for constant velocity, v=' int2str(vc(1)) ' m/s'])
%% buried syncline. This will be a circular depression imposed on a horizontal reflector
%
%                                   0
%
% ---------------------------               -----------------------------
%                             *           *
%                               **     **
%                                 *****
%
% Notice how ievery controls the width of the unmigrated spectrum. With
% ievery=1, the unmigrated spectrum does not seem to extend to the
% evanescent boundaryies. But with ievery=5, it does. Can you explain this?
%
clear
fmax=75;%maximum frequency to migrate
dipmax=80;%maximum fip to migrate
v=2000; %velocity of simulation
dt=.004;%time sample rate
dx=5;%x (cmp) sample size
tmax=1;%maximum time
xmax=2000;%maximum x (linelength)
zmax=1000;
x=0:dx:xmax;
t=0:dt:tmax;
z=t*v/2;
dz=z(2);
ievery=1;%make a point diffraction every this many traces
filtparms=[5,10,fmax-10,fmax]; %filter specification Ormsby style


zr=zmax/3;%depth to reflector

%x0,z0 are coordinates of the focus (the 0 in the diagram)
x0=xmax/2;
%try zr/2 and -zr/2 for the following.
z0 = zr/2;%Should be no larger than zr. A positive number for z0 means a 
%buried syncline and the seismic image will have a reverse-time branch. A
%negative number means the syncline is not buried and there is no reverse
%time branch

r=1.5*(zr-z0);%radius of the syncline. This should be greater than zr-z0 or there will be no syncline at all
x1=x0-sqrt(r^2-(zr-z0)^2);%start of syncline
x2=x0+sqrt(r^2-(zr-z0)^2);%end of syncline

%diffraction coordinates
xd=x(1:ievery:end);
zd=zeros(size(xd));
ind=xd<=x1;
zd(ind)=zr;
ind=xd>=x2;
zd(ind)=zr;
ind=find(zd==0);
for k=1:length(ind)
    xdtmp=xd(ind(k));
    zd(ind(k))=z0+sqrt(r^2-(x0-xdtmp)^2);
end
td=2*zd/v;

%ok install diffractions
seis=zeros(length(t),length(x));
for k=1:length(xd)
    seis=event_hyp(seis,t,x,td(k),xd(k),v,1);
end

%filter
seis=filtf(seis,t,[filtparms(2) filtparms(2)-filtparms(1)],[filtparms(3) filtparms(4)-filtparms(3)]);



%fkmig
params=nan*ones(1,13);
params(1)=fmax;
params(3)=dipmax;
[seism,tm,xm]=fkmig(seis,t,x,v,params);
plotimage(seism,tm,xm)
title('Migrated syncline')
% spectra
[fk,f,k]=fktran(seis,t,x);
[fkm,fm,km]=fktran(seism,tm,xm);
plotimage(abs(fkm),fm,km)
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
title('Migrated spectrum of buried syncline')
plotimage(abs(fk),f,k)
title('Unmigrated spectrum of buried syncline')
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
%draw evanescent boundaries
fbdy=v*abs(k)/2;
ind=find(fbdy<=max(f));
h=line(k(ind),fbdy(ind),'color','r');
legend(h,'evanescent boundary')

plotimage(seis,t,x);
title(['Buried syncline, focal depth =' int2str(z0)])
h=line(xd,td,'color','r');
set(h,'linestyle',':','linewidth',2)
h2=line(x0,z0*2/v,'color','r');
set(h2,'linestyle','none','marker','o')
legend([h h2],'Actual syncline','focal point')
figure
plot(xd,zd,x0,z0,'ro');flipy
title('diffraction locations')
grid
axis equal

%% model a buried cosine or "cosyncline"
%
% Notice how ievery controls the width of the unmigrated spectrum. With
% ievery=1, the unmigrated spectrum does not seem to extend to the
% evanescent boundaryies. But with ievery=5, it does. Can you explain this?
%
clear
polarity=1;%use a 1 to get a major syncline in the middle and anticlines on the flanks. A -1 gets the opposite
znot=500;%mean depth of the cosine, try values like 500 and 150
fmax=75;
dipmax=80;
ievery=1;%put a diffraction every this many traces
xperiod=1000;
xmax=2000;
tmax=1;
xnot=500;%x coordinate of zero argument
amp=100;%amplitude of cosine
% zmax=1000;
v=2000;
dx=5;
dt=.004;
x=0:dx:xmax;
t=0:dt:tmax;

filtparms=[5,10,fmax-10,fmax]; %filter specification Ormsby style

%diffraction coordinates
xd=x(1:ievery:end);
zd=znot-polarity*amp*cos((xd-xnot)*2*pi/xperiod);
td=2*zd/v;

%ok install diffractions
seis=zeros(length(t),length(x));
for k=1:length(xd)
    seis=event_hyp(seis,t,x,td(k),xd(k),v,1);
end

%filter
seis=filtf(seis,t,[filtparms(2) filtparms(2)-filtparms(1)],[filtparms(3) filtparms(4)-filtparms(3)]);

%fkmig
params=nan*ones(1,13);
params(1)=fmax;
params(3)=dipmax;
[seism,tm,xm]=fkmig(seis,t,x,v,params);
plotimage(seism,tm,xm)
title('Migrated co-syncline')
% spectra
[fk,f,k]=fktran(seis,t,x);
[fkm,fm,km]=fktran(seism,tm,xm);
plotimage(abs(fkm),fm,km)
title('Cosyncline: Migrated spectrum')
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
plotimage(abs(fk),f,k)
title('Cosyncline: Unmigrated spectrum')
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
%draw evanescent boundaries
fbdy=v*abs(k)/2;
ind=find(fbdy<=max(f));
h=line(k(ind),fbdy(ind),'color','r');
legend(h,'evanescent boundary')
plotimage(seis,t,x);
title('Buried co-syncline')
h=line(xd,td,'color','r');
set(h,'linestyle',':','linewidth',2)
legend(h,'Actual cosyncline')
figure
plot(xd,zd);flipy
title('diffraction locations')
grid
axis equal
%% dipping reflector
%
% Notice how ievery controls the width of the unmigrated spectrum. With
% ievery=1, the unmigrated spectrum does not seem to extend to the
% evanescent boundaryies. But with ievery=5, it does. Can you explain this?
%
clear
fmax=75;
dipmax=80;
ievery=1;
xmax=2000;
zmax=1000;
xnot=1500;%x coordinate where the reflector outcrops
z1=100;%first depth with nonzero amplitude
z2=500;%last depth with nonzero amplitude
dip=-30;%dip in degrees, positive is down to the right. Try different dips. You might want to change xnot as well
v=2000;
dx=5;
dt=.004;
tmax=1;
x=0:dx:xmax;
t=0:dt:tmax;

filtparms=[5,10,fmax,fmax+20]; %filter specification Ormsby style

%diffraction coordinates
if(dip>0)
    ixnot=round(xnot/dx)+1;
    xd=x(ixnot:ievery:end);
    zd=(xd-xnot)*tand(dip);
    td=2*zd/v;
else
    ixend=round(xnot/dx)+1;
    xd=x(1:ievery:ixend);
    zd=(xd-xnot)*tand(dip);
    td=2*zd/v;
end

%ok install diffractions
seis=zeros(length(t),length(x));
for k=1:length(xd)
    if(between(z1,z2,zd(k)))
        seis=event_hyp(seis,t,x,td(k),xd(k),v,1);
    end
end

%filter
seis=filtf(seis,t,[filtparms(2) filtparms(2)-filtparms(1)],[filtparms(3) filtparms(4)-filtparms(3)]);

plotimage(seis,t,x);
title(['Dipping reflector, dip= ' int2str(dip) ' degrees'])
h=line(xd,td,'color','r');
set(h,'linestyle',':','linewidth',2)
figure
plot(xd,zd);flipy
title('diffraction locations')
grid
axis equal

%fkmig
params=nan*ones(1,13);
params(1)=fmax;
params(3)=dipmax;
[seism,tm,xm]=fkmig(seis,t,x,v,params);
plotimage(seism,tm,xm)
title('Migrated dipping reflector')
h=line(xd,td,'color','r');
set(h,'linestyle',':','linewidth',2)
% spectra
[fk,f,k]=fktran(seis,t,x);
[fkm,fm,km]=fktran(seism,tm,xm);
plotimage(abs(fkm),fm,km)
title('Migrated spectrum')
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
plotimage(abs(fk),f,k)
title('Unmigrated spectrum')
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
%draw evanescent boundaries
fbdy=v*abs(k)/2;
ind=find(fbdy<=max(f));
h=line(k(ind),fbdy(ind),'color','r');
legend(h,'evanescent boundary')
%% horizontal reflector with a local amplitude anomaly
clear
fmax=75;
dipmax=80;
ievery=1;%put a diffraction every this many traces
xmax=2000;
zmax=1000;
dx=5;
zr=zmax/3;%reflector depth;
xwid=10*dx;%width of amplitude anomaly
amp=1.1;%amplitude of the anomaly. Everything else has amplitude 1.0
dz=3*dx;%anomaly will be at depth zr+dz
xend=.1*xmax;%width of no  reflector at edges of section
xnot=xmax/2;%anomaly centered here
v=2000;
dx=5;
dt=.004;
tmax=2*zmax/v;
x=0:dx:xmax;
t=0:dt:tmax;

filtparms=[5,10,fmax,fmax+20]; %filter specification Ormsby style

%diffraction coordinates. Start at xnot and go both directions to ensure
%that we have sampled the anomaly symmetrically
%
inot=round(xnot/dx)+1;
i1=round(xend/dx)+1;
i2=round((xnot-xwid*.5)/dx)+1;
i3=round((xnot+xwid*.5)/dx)+1;
i4=round((xmax-xend)/dx)+1;

xd_left=x(inot:-ievery:i1);
xd_right=x(inot:ievery:i4);
td_left=(2*zr/v)*ones(size(xd_left));
ind=between(xnot,xnot-.5*xwid,xd_left,2);
td_left(ind)=(2*(zr+dz)/v)*ones(size(ind));
td_right=(2*zr/v)*ones(size(xd_right));
ind=between(xnot,xnot+.5*xwid,xd_right,2);
td_right(ind)=(2*(zr+dz)/v)*ones(size(ind));

%ok install diffractions
seis=zeros(length(t),length(x));
for k=1:length(xd_left)
    if(between(xnot,xnot-.5*xwid,xd_left(k),2));
        seis=event_hyp(seis,t,x,td_left(k),xd_left(k),v,amp);
    else
        seis=event_hyp(seis,t,x,td_left(k),xd_left(k),v,1);
    end
end
for k=2:length(xd_right)
    if(between(xnot,xnot+.5*xwid,xd_right(k),2));
        seis=event_hyp(seis,t,x,td_right(k),xd_right(k),v,amp);
    else
        seis=event_hyp(seis,t,x,td_right(k),xd_right(k),v,1);
    end
end

%filter
seis=filtf(seis,t,[filtparms(2) filtparms(2)-filtparms(1)],[filtparms(3) filtparms(4)-filtparms(3)]);


%fkmig
params=nan*ones(1,13);
params(1)=fmax;
params(3)=dipmax;
[seism,tm,xm]=fkmig(seis,t,x,v,params);
plotimage(seism,tm,xm)
title('Migrated horizontal reflector with anomaly')
td=2*(zr+dz)/v;
h=line([xnot-.5*xwid,xnot+.5*xwid],[td td],'color','r');
set(h,'linestyle','-','linewidth',2)
legend(h,'anomaly')
% spectra
[fk,f,k]=fktran(seis,t,x);
[fkm,fm,km]=fktran(seism,tm,xm);
plotimage(abs(fkm),fm,km)
title('Migrated spectrum')
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
plotimage(abs(fk),f,k)
title('Unmigrated spectrum')
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
%draw evanescent boundaries
fbdy=v*abs(k)/2;
ind=find(fbdy<=max(f));
h=line(k(ind),fbdy(ind),'color','r');
legend(h,'evanescent boundary')

plotimage(seis,t,x);
title(['Horizontal reflector, anomaly width ' int2str(xwid) ', amplitude=' num2str(amp) ', depth shift=' num2str(dz)])

h=line([xnot-.5*xwid,xnot+.5*xwid],[td td],'color','r');
set(h,'linestyle','-','linewidth',2)
legend(h,'anomaly')
%%
% model a popup reef structure. flanks are always 45 degrees
%
%                           ------------------
%                          .                  .
%                         .                    .
%                        .                      .
%------------------------                        -------------------------
%
%
clear
fmax=75;
dipmax=60;
v=2000;dx=5;dt=.004;%basic model parameters
xmax=3000;
x=0:dx:xmax;%x axis

zb=1000;%depth of base of structure;
zt=800;%depth of top of structure
xw=xmax/3;%width of structure;
xcntr=max(x)/2;%center of structure
amp=1;
tmax=1.5*2*zb/v;

t=0:dt:tmax;%t axis

ievery=1;
seis1=zeros(length(t),length(x));%allocate seismic matrix
seis1=event_diph2(seis1,t,x,v,0,xcntr-.5*xw,zb,ievery,0,amp);
disp('Working ...')
seis1=event_diph2(seis1,t,x,v,xcntr-.5*xw,xcntr-.5*xw+(zb-zt)*tand(45),zb,ievery,-45,amp);
disp('Working ...')
seis1=event_diph2(seis1,t,x,v,xcntr-.5*xw+(zb-zt)*tand(45),xcntr+.5*xw-(zb-zt)*tand(45),zt,ievery,0,amp);
disp('Working ...')
seis1=event_diph2(seis1,t,x,v,xcntr+.5*xw-(zb-zt)*tand(45),xcntr+.5*xw,zt,ievery,45,amp);
disp('Still at it ...')
seis1=event_diph2(seis1,t,x,v,xcntr+.5*xw,max(x),zb,ievery,0,amp);
disp('Wrapping up ...')
[w,tw]=ricker(dt,40,.2);%make ricker wavelet
seis1=sectconv(seis1,t,w,tw);%apply wavelet

%fk migrations
params=nan*ones(1,13);
params(1)=fmax;
params(3)=dipmax;
[seis1mig,tmig,xmig]=fkmig(seis1,t,x,v,params);
plotimage(seis1mig,tmig,xmig)
title(['FK migration of reef with every ' int2str(ievery) 'th diffraction'])

% spectra
[fk,f,k]=fktran(seis1,t,x);
[fkm,fm,km]=fktran(seis1mig,tmig,xmig);
plotimage(abs(fkm),fm,km)
title('Migrated spectrum')
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
plotimage(abs(fk),f,k)
title('Unmigrated spectrum')
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
%draw evanescent boundaries
fbdy=v*abs(k)/2;
ind=find(fbdy<=max(f));
h=line(k(ind),fbdy(ind),'color','r');
legend(h,'evanescent boundary')

plotimage(seis1,t,x);
title(['Popup structure showing every ' int2str(ievery) 'th diffraction hyperbola'])