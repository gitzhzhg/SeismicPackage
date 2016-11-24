%% demo fk extrapolation
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

%impulse response
global SCALE_OPT;
SCALE_OPT=2;
dz=100;%depth step
imaginaryfraction=.0001;%including a smally imaginary component in the velocity causes slight viscous damping which is a good thing
vel=2000*(1+imaginaryfraction*1i);%include a small imaginary component for damping
%make a section with an impulse
dt=.002;
dx=5;
nt=512;
nx=512;
t=(0:nt-1)*dt;
x=(0:nx-1)*dx;
seis=zeros(nt,nx);
seis(round(nt/2),round(nx/2))=1;
tnot=(round(nt/2)-1)*dt;
xnot=(round(nx/2)-1)*dx;
fmins=[10 5];
fmaxs=[1/(4*dt) 1/(10*dt)];
seis=filtf(seis,t,fmins,fmaxs);

%forward fk transform
[seisfk,f,k]=fktran(seis,t,x);
seisfk=fftshift(seisfk,2);%ips wants a wrapped wavenumber spectrum

%extrapolate up and down
seisfkup=ips(seisfk,f,dx,vel,-dz);
seisfkup(end,:)=0;
seisup=ifktran(fftshift(seisfkup,2),f,k);
seisfkdown=ips(seisfk,f,dx,vel,dz);
seisfkdown(end,:)=0;
seisdown=ifktran(fftshift(seisfkdown,2),f,k);

%plots
%phase spectra
plotimage(fftshift(angle(seisfk),2),f,k)
title('FK spectrum (phs) of impulse');
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
plotimage(fftshift(angle(seisfkup),2),f,k)
title('FK spectrum (phs) of upward extrapolation');
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
%amplitude spectra
plotimage(fftshift(abs(seisfk),2),f,k)
title('FK spectrum (amp) of impulse');
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
plotimage(fftshift(abs(seisfkup),2),f,k)
title('FK spectrum (amp) of upward extrapolation');
xlabel('k_x m^{-1}');ylabel('f s^{-1}');
%x-t domain impulse response
plotimage(seisdown,t,x);
title(['Extrapolated down ' num2str(dz) ' meters'])
h1=line(xnot,tnot,'linestyle','none','marker','o','color','r');
legend(h1,'impulse location')
plotimage(seisup,t,x);
title(['Extrapolated up ' num2str(dz) ' meters'])
h2=line(xnot,tnot,'linestyle','none','marker','o','color','r');
legend(h2,'impulse location')
%%
% a single diffraction hyperbola
vel=2000;
imaginaryfraction=.0001;%including a smally imaginary component in the velocity causes slight viscous damping which is a good thing
dt=.002;
dx=5;
nt=512;
nx=512;
t=(0:nt-1)*dt;
x=(0:nx-1)*dx;
dip=20;
xnot=nx/2*dx;%x location of the diffraction
tnot=2*(200+(xnot-100*dx)*tand(dip))/vel;%this is being chosen to compare with the center of the dipping reflecctor in the next cell
% tnot=.25*max(t);%apex time of the diffraction
seis=zeros(nt,nx);
seis=event_hyp(seis,t,x,tnot,xnot,vel,1,3);
fmins=[10 5];
fmaxs=[1/(8*dt) 1/(16*dt)];
seis=filtf(seis,t,fmins,fmaxs);
%amat=event_diph(amat,t,x,v,x0,x1,z0,theta,amp,flag,noversamp)

%step down to depth of diffractor
%forward fk transform
[seisfk,f,k]=fktran(seis,t,x);
seisfk=fftshift(seisfk,2);%ips wants a wrapped wavenumber spectrum

%extrapolate down
dz=tnot*vel/2;
seisfkdown=ips(seisfk,f,dx,(1+imaginaryfraction*1i)*vel/2,dz);
%make f Nyquist real
seisfkdown(end,:)=real(seisfkdown(end,:));
seisdown=ifktran(fftshift(seisfkdown,2),f,k);

%plot

plotimage(seisdown(1:length(t),:),t,x);
title('Extrapolated to depth of diffraction')
plotimage(seis,t,x)
title('Input')
%%
% a dipping reflector
vel=2000;
dt=.002;
dx=5;
nt=512;
nx=512;
t=(0:nt-1)*dt;
x=(0:nx-1)*dx;
seisd=zeros(nt,nx);
x1=100*dx;%starting x of event
x2=400*dx;%ending x of event
z1=200;%starting depth
if(exist('dip','var')==0)
    error('run the previous cell first')
end
seisd=event_diph(seisd,t,x,vel,x1,x2,z1,dip,1);
fmins=[10 5];
fmaxs=[1/(8*dt) 1/(16*dt)];
seisd=filtf(seisd,t,fmins,fmaxs);
%amat=event_diph(amat,t,x,v,x0,x1,z0,theta,amp,flag,noversamp)

%step down to first diffraction
%forward fk transform
[seisfk,f,k]=fktran(seisd,t,x);
seisfk=fftshift(seisfk,2);%ips wants a wrapped wavenumber spectrum

%extrapolate down
dz=200;
seisfkdown1=ips(seisfk,f,dx,(1+.01*1i)*vel/2,dz);
%make f Nyquist real
seisfkdown1(end,:)=real(seisfkdown1(end,:));
seisdown1=ifktran(fftshift(seisfkdown1,2),f,k);

%plot
plotimage(seisdown1(1:length(t),:),t,x);
title(['Dipping reflector extrapolated down ' num2str(dz) ' meters'])
plotimage(seisd,t,x)
title(['Input, dipping reflector, dip= ' int2str(dip) 'degrees'])

%% compare wavelets from single diffraction and dipping reflector
% run previous two cells first

%
%this will fail unless the previous cell has already been run
%
znot=tnot*vel/2;
delx=znot*tand(dip);%migration shift
ix=round((xnot+delx)/dx)+1;%trace to grab from the diffraction and the dipping event
s1=seis(:,ix);%trace from diffraction
s2=seisd(:,ix);%trace from dipping reflector
phaserotation=constphase(s1,s2);
figure
plot(t,seis(:,ix),t,seisd(:,ix))
legend('diffraction','dipping reflector')
title({['Phase rotation = ' int2str(phaserotation) ' degrees'];'Can you explain the phase rotation?'})

%compare after migration
params=nan*ones(1,13);
params(1)=sum(fmaxs);
params(3)=80;
[seisdm,tm,xm]=fkmig(seisd,t,x,vel,params);
[seism,tm,xm]=fkmig(seis,t,x,vel,params);
ix=round((xnot)/dx)+1;%trace to grab from the diffraction and the migrated dipping event
s1=seism(:,ix);%trace from diffraction
s2=seisdm(:,ix);%trace from dipping reflector
phaserotation=constphase(s1,s2);
figure
plot(t,seism(:,ix),t,seisdm(:,ix))
legend('diffraction','migrated dipping reflector')
title({['Phase rotation = ' int2str(phaserotation) ' degrees'];'Can you explain the phase rotation?'})

%%
%make a section with 9 diffractions and then do a recursive extrapolation
%and depth migration through it
%
vel=2000*(1+.0001*1i);
dz=25;
zmax=1000;
z=(0:dz:zmax);
znot=[100, 200, 300 400 500 600 700 800 900];%make sure these occur at integer depth steps
tnot=2*znot/real(vel);
dt=.002;
dx=5;
nt=1024;
nx=512;
t=(0:nt-1)*dt;
x=(0:nx-1)*dx;
xnot=nx/2*dx;
seis=zeros(nt,nx);
for k=1:length(tnot)
%     seis=event_hyp(seis,t,x,tnot(k),xnot,real(vel),(-1)^k);
    seis=event_hyp(seis,t,x,tnot(k),xnot,real(vel),1);
end

% fmins=[10 5];
% fmaxs=[1/(8*dt) 1/(16*dt)];
% seis=filtf(seis,t,fmins,fmaxs);
fdom=30;
[w,tw]=ricker(dt,fdom,.2);
seis=convz(seis,w);

%initialize depth section
seisz=zeros(length(z),length(x));

%extrapolate down
seisdown=cell(1,length(znot)+1);
titles=seisdown;
seisdown{1}=seis;
titles{1}='Input diffraction section';
seisz(1,:)=seis(1,:);%imaging condition
nsteps=length(z)-1;
%this loop does a depth Migration!!!!
for kk=1:nsteps %stepping down in depth
    dz=z(kk+1)-z(kk);
    %forward fk transform
    [seisfk,f,k]=fktran(seisdown{kk},t,x);
    seisfk=fftshift(seisfk,2);%ips wants a wrapped wavenumber spectrum
    seisfkdown=ips(seisfk,f,dx,vel/2,dz);%apply the wavefield extrapolator to get to the next depth
    %make f Nyquist real
    seisfkdown(end,:)=0;
    %inverse fk transform
    seisdown{kk+1}=ifktran(fftshift(seisfkdown,2),f,k);
    titles{kk+1}=['Extrapolated down ' num2str(z(kk+1)) ' meters'];
    %imaging condition, evaluate the extrapolated zos at t=0
    seisz(kk+1,:)=seisdown{kk+1}(1,:);
end

%plot
plotgathers(seisdown,x,t,'distance (m)','time (s)',titles);
plotimage(seisz,z,x)
title('Depth section')
xlabel('distance (m)')
ylabel('depth (m)')
%%
%make a v(z) exploding reflector section
%and depth migration through it
%
% znot=[100 300 500 700 900];%make sure these occur at integer depth steps
znot=[100, 200, 300 400 500 600 700 800 900];%make sure these occur at integer depth steps

dx=2;
nx=512;

x=(0:nx-1)*dx;
zmax=1500;
z=(0:dx:zmax)';
xnot=nx/2*dx;
vz=1800+.6*z;%linear v(z)
vel=vz*ones(size(x));
ixnot=round(xnot/dx)+1;
tmp=zeros(size(vz));
iznot=round(znot/dx)+1;
for k=1:length(znot)
%     tmp(iznot(k)-5:iznot(k)+5)=.3*(-1)^k;
    tmp(iznot(k)-5:iznot(k)+5)=.3;
end
vel(:,ixnot-5:ixnot+5)=vel(:,ixnot-5:ixnot+5).*(1+tmp*ones(1,11));

dt=.004; %temporal sample rate
dtstep=.0005;
tmax=1.5*zmax/vz(1); %maximum time
[seisf,seis,t]=afd_explode(dx,dtstep,dt,tmax, ...
 		vel,x,zeros(size(x)),[5 10 50 60],0,2);
%attach a zero pad to a power of two in t
ntnew=2^nextpow2(length(t));
seisf=[seisf;zeros(ntnew-length(t),nx)];
t=dt*(0:size(seisf,1)-1)';

%initialize depth section
%note that the depth coordinate for finite difference modelling and for
%depth migration are not related. We can choose any downward step size that
%we wish. The modelling was done with dz=dx=2 for less grid dispersion
%while here we use a downward step of 25.
dz=25;
z=(0:dz:zmax)';
vz=1800+.6*z;
seisz=zeros(length(z),length(x));

%extrapolate down
seisdown=cell(1,length(znot)+1);
titles=seisdown;
seisdown{1}=seisf;
titles{1}='Input seismic section';
seisz(1,:)=seis(1,:);%imaging condition
nsteps=length(z)-1;
%this loop does a depth Migration!!!!
for kk=1:nsteps %stepping down in depth
    dz=z(kk+1)-z(kk);
    %forward fk transform
    [seisfk,f,k]=fktran(seisdown{kk},t,x);
    seisfk=fftshift(seisfk,2);%ips wants a wrapped wavenumber spectrum
    seisfkdown=ips(seisfk,f,dx,vz(kk)/2,dz);%apply the wavefield extrapolator to get to the next depth
    %make f Nyquist real
    seisfkdown(end,:)=0;
    %inverse fk transform
    seisdown{kk+1}=ifktran(fftshift(seisfkdown,2),f,k);
    titles{kk+1}=['Extrapolated down ' num2str(z(kk+1)) ' meters'];
    %imaging condition, evaluate the extrapolated zos at t=0
    seisz(kk+1,:)=seisdown{kk+1}(1,:);
    disp(['finished step ' int2str(kk)])
end

%plot
plotgathers(seisdown,x,t,'distance (m)','time (s)',titles);
plotimage(seisz,z,x)
title('Depth section')
xlabel('distance (m)')
ylabel('depth (m)')
%%
%recursive 
nsteps=100;
vel=linspace(2000,4000,nsteps);
dv=vel.*(rand(1,nsteps)-.5)*.1;%10%fluctuation
vel=vel+dv;
dz=5;
z=(0:nsteps-1)*dz;
t2=vint2t(real(vel),z);
vrms=vint2vrms(real(vel),t2);
vave=vint2vave(real(vel),t2);
vmean=vrms.^2./vave;


imaginaryfraction=0.0001;%including a smally imaginary component in the velocity causes slight viscous damping which is a good thing

%make a section with an impulse
dt=.002;
dx=5;
nt=512;
nx=512;
t=(0:nt-1)*dt;
x=(0:nx-1)*dx;
seis=zeros(nt,nx);
seis(round(nt/2),round(nx/2))=1;
tnot=(round(nt/2)-1)*dt;
xnot=(round(nx/2)-1)*dx;
fmins=[10 5];
fmaxs=[1/(4*dt) 1/(10*dt)];
seis=filtf(seis,t,fmins,fmaxs);

%forward fk transform
[seisfk,f,k]=fktran(seis,t,x);
seisfk=fftshift(seisfk,2);%ips wants a wrapped wavenumber spectrum
seisfkup=seisfk;
seisfkdown=seisfk;

for kstep=1:nsteps
    seisfkup=ips(seisfkup,f,dx,vel(kstep)*(1+imaginaryfraction*1i),-dz);
    seisfkdown=ips(seisfkdown,f,dx,vel(kstep),dz);
end

seisfkup(end,:)=0;
seisfkdown(end,:)=0;
seisup=ifktran(fftshift(seisfkup,2),f,k);
seisdown=ifktran(fftshift(seisfkdown,2),f,k);


% plotimage(seisup,t,x);
% title(['Extrapolated up ' int2str(nsteps) ' steps of ' num2str(dz) ' meters each'])
% h2=line(xnot,tnot,'linestyle','none','marker','o','color','r');
% legend(h2,'impulse location')
% grid

%compare to rms and mean extrapolation
for kk=1:2
    userms=kk-1;
    if(userms==1)
        vf=vrms(end);%focussing velocity
    else
        vf=vmean(end);%focussing velocity
    end
    seisfkdownrms=ips(seisfk,f,dx,vf*(1+imaginaryfraction*sqrt(nsteps)*1i),nsteps*dz);
    seisfkdownrms(end,:)=0;
    seisdownrms=ifktran(fftshift(seisfkdownrms,2),f,k);
    %apply the differential static
    tshift=nsteps*dz*(1/vave(end)-1/vf);
    seisdownrmsave=stat(seisdownrms,t,-tshift);
    %plot
    if(userms==1)
        plotimage(seisdownrms,t,x);
        title(['Extrapolated down ' int2str(nsteps*dz) 'm in 1 step with vrms'])
        h1=line(xnot,tnot,'linestyle','none','marker','o','color','r');
        legend(h1,'impulse location')
        grid
        plotimage(seisdownrmsave,t,x);
        title(['Extrapolated down ' int2str(nsteps*dz) 'm in 1 step with vrms and vave'])
        h2=line(xnot,tnot,'linestyle','none','marker','o','color','r');
        legend(h2,'impulse location')
        grid
    else
        plotimage(seisdownrms,t,x);
        title(['Extrapolated down ' int2str(nsteps*dz) 'm in 1 step with vmean'])
        h1=line(xnot,tnot,'linestyle','none','marker','o','color','r');
        legend(h1,'impulse location')
        grid
        plotimage(seisdownrmsave,t,x);
        title(['Extrapolated down ' int2str(nsteps*dz) 'm in 1 step with vmean and vave'])
        h2=line(xnot,tnot,'linestyle','none','marker','o','color','r');
        legend(h2,'impulse location')
        grid
    end


    figure
    if(userms==1)
        ind=near(t,.3,.42);
        plot(t(ind),seisdown(ind,round(nx/2)-1),t(ind),seisdownrms(ind,round(nx/2)-1),t(ind),seisdownrmsave(ind,round(nx/2)-1))
        title('Comparison of center trace from different extrapolations')
        cc1=maxcorr(seisdown(:,round(nx/2)-1),seisdownrms(:,round(nx/2)-1),20);
        cc2=maxcorr(seisdown(:,round(nx/2)-1),seisdownrmsave(:,round(nx/2)-1),20);
        legend('Recursive',['RMS maxcorr(1)=' num2str(cc1(1)) ' maxcorr(2)=' num2str(cc1(2))],['RMSAVE maxcorr(1)=' num2str(cc2(1)) ' maxcorr(2)=' num2str(cc2(2))])
    else
        ind=near(t,.3,.42);
        plot(t(ind),seisdown(ind,round(nx/2)-1),t(ind),seisdownrms(ind,round(nx/2)-1),t(ind),seisdownrmsave(ind,round(nx/2)-1))
        title('Comparison of center trace from different extrapolations')
        cc1=maxcorr(seisdown(:,round(nx/2)-1),seisdownrms(:,round(nx/2)-1),20);
        cc2=maxcorr(seisdown(:,round(nx/2)-1),seisdownrmsave(:,round(nx/2)-1),20);
        legend('Recursive',['Mean maxcorr(1)=' num2str(cc1(1)) ' maxcorr(2)=' num2str(cc1(2))],['MeanAVE maxcorr(1)=' num2str(cc2(1)) ' maxcorr(2)=' num2str(cc2(2))])
    end
end

%plot
plotimage(seisdown,t,x);
title(['Extrapolated down ' int2str(nsteps) ' steps of ' num2str(dz) ' meters each'])
h1=line(xnot,tnot,'linestyle','none','marker','o','color','r');
legend(h1,'impulse location')
grid
figure
plot(vel,z,vrms,z,vave,z,vmean,z);flipy
legend('interval','RMS','Ave','Mean')
title('Velocity profile')