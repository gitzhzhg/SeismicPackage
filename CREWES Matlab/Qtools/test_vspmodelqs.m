%% use Pedro's model (extra interface)
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

prepfig_globals;%sets some plotting parameters
global SCALE_OPT
SCALE_OPT=1;
%this section runs vspmodelqs1 which assumes the source is in layer 1
%make a very simple model
dz=5;
z=[0 (500:dz:2000)]';
% z=(0:dz:2000)';
vp=zeros(size(z));
rho=vp;
Q=vp;
%install interfaces at 750m and at 1350m
z1=750;%first interface
z2=1350;%second interface
v1=1000;%velocity above first interface
v2=2000;%velocity between interfaces
v3=1000;%velocity below second interface
Q1=20;%Q above first interface
Q2=30;%Q between interfaces
Q3=40;%Q below second interface
r1=2000;%density above first interface
r2=5000;%density between interfaces
r3=2000;%density below second interface
i1=near(z,z1);
i2=near(z,z2);
vp(1:i1)=v1;
vp(i1:i2)=v2;
vp(i2:end)=v3;
Q(1:i1)=Q1;
Q(i1:i2)=Q2;
Q(i2:end)=Q3;
rho(1:i1)=r1;
rho(i1:i2)=r2;
rho(i2:end)=r3;

% figure
% hh=plot(vp,z,rho,z,100*Q,z);flipy
% set(hh(2),'marker','.')
% title('Simple model')
% ylabel('Depth (m)');
% xlabel('Velocity or Density (MKS units)')
% legend('Velocity','Density','100*Q');
% axis([0 6000 0 2000])
% prepfig

dt=.001;
tmax=3;
tw=(0:dt:.1)';
fdom=10;
w=(sin(2*pi*fdom*tw)-.5*sin(2*pi*2*fdom*tw)).*mwhalf(length(tw),20);
% [w,tw]=ricker(dt,30,.2);
% [w,tw]=wavemin(dt,fdom,tmax);
% figure
% plot(tw,w)
% title('Wavelet')
% xlabel('time (s)')
% prepfig
zr=0:5:1500;% receiver depths

%compute a time depth curve from the well velocities
tz=vint2t(vp,z);
%interpolate times at each receiver depth
trec=interp1(z,tz,zr);
%compute drift times
td=tdrift(Q,z,vp,fdom);
%interpolate at the receiver depth
tdr=interp1(z,td,zr);

%run the model
rflag=0;
fpress=1;
fmult=1;
f0=12500;%logging frequency
zs=200;%source depth
[vspq,tq,upq,downq]=vspmodelqs(vp,rho,Q,z,w,tw,tmax,zr,zs,f0,rflag,fpress,fmult);

plotimage(vspq',zr,tq);
title(['Total field, rflag=' num2str(rflag) ', fpress=' int2str(fpress) ', Source depth=' num2str(zs)] );
ylabel('receiver depth');
xlabel('time');

plotimage(upq',zr,tq);
title(['Upgoing field, rflag=' num2str(rflag) ', fpress=' int2str(fpress)] );
ylabel('receiver depth');
xlabel('time');

plotimage(downq',zr,tq);
% h1=line(zr,trec,'color','r');
% h2=line(zr,trec+tdr,'color','g');
title(['Downgoing field, rflag=' num2str(rflag) ', fpress=' int2str(fpress)]);
ylabel('receiver depth');
xlabel('time');
% legend([h1 h2],'time at well velocity','time at seismic velocity');

ind=near(zr,500);
figure
subplot(3,1,1)
plot(tq,vspq(:,ind(1)))
title(['Total field, receiver at z=500, rflag=' num2str(rflag) ', fpress=' int2str(fpress) ]);
subplot(3,1,2)
plot(tq,downq(:,ind(1)))
title(['Downgoing field, rflag=' num2str(rflag) ', fpress=' int2str(fpress) ]);
subplot(3,1,3)
plot(tq,upq(:,ind(1)))
title(['Upgoing field, rflag=' num2str(rflag) ', fpress=' int2str(fpress) ]);
%% use well 1409 to create a more complex model
wellname='1409.las';%las file to read
s=which('vspmodelq');
if(isempty(s))
    error('Well file for well 1409 not found, you need to load and install the CREWES toolbox')
end
ind = strfind(s,'vspmodelq');
filename=[s(1:ind-1) '1409.las'];
disp(['Well 1409 loaded from ' filename])
dzblk=1;%blocking size
dzout=1;%sample size
vp0=1600;
vs0=900;
rho0=1800;

[vp,vs,rho,z]=blocklogs(filename,dzblk,dzout,vp0,vs0,rho0);

%invent a Q
Qmin=20;Qmax=200;
vp1=1500;vp2=4500;
rho1=1800;rho2=3000;
[Q,Qrand]=fakeq(vp,rho,Qmin,Qmax,2,vp1,vp2,rho1,rho2,1,1);

figure
plot(vp,z,rho,z,10*Q,z);flipy
title('Well model')
ylabel('Depth (m)');
xlabel('Velocity or Density (MKS units) and Q')
legend('Velocity','Density','10*Q');
prepfig
%% surface source (vspmodelq)
%define wavelet and receivers
dt=.002;
fdom=30;
% [w,tw]=ricker(dt,30,.2);
[w,tw]=wavemin(dt,fdom,.2);

figure
plot(tw,w)
title('Wavelet')
xlabel('time (s)')
prepfig

tmax=dt*(1023);%make it a factor of 2 long
zr=0:2:1400;
rflag=1;
fpress=0;
fmult=1;
% noQ=inf*Q;
[vspq,t,upq,downq]=vspmodelq(vp,rho,Q,z,w,tw,tmax,zr,12500,rflag,fpress,fmult);
% [vspq,tq,upq,downq]=vspmodelq(vp,rho,noQ,z,w,tw,tmax,zr,0,12500,rflag,fpress,fmult);

%compute a time depth curve from the well velocities
tz=vint2t(vp,z);
%interpolate times at each receiver depth
trec=interp1(z,tz,zr);
%compute drift times
td=tdrift(Q,z,vp,fdom);
%interpolate at the receiver depth
tdr=interp1(z,td,zr);

plotimage(vspq',zr,t);
title('Total field');
ylabel('receiver depth');
xlabel('time');
plotimage(upq',zr,t);
title('Upgoing field');
ylabel('receiver depth');
xlabel('time');
plotimage(downq',zr,t);
% h1=line(zr,trec,'color','r');
% h2=line(zr,trec+tdr,'color','g');
title('Downgoing field');
ylabel('receiver depth');
xlabel('time');
% legend([h1 h2],'time at well velocity','time at seismic velocity');
% figure
% trace=downq(:,end);
% amp=max(abs(trace))/3;
% ind=near(t,trec(end-1));
% p1=zeros(size(t));
% p1(ind)=amp;
% ind=near(t,trec(end-1)+tdr(end-1));
% p2=zeros(size(t));
% p2(ind)=amp;
% plot(t,trace,t,p1,'r',t,p2,'k')
% title('Downgoing field at deepest receiver') 
% xlabel('Time (s)')
% legend('Deepest receiver','Well velocity time','Seismic velocity time')
%% source in overburden at z=100 (vspmodelqs)
%define wavelet and receivers
dt=.002;
fdom=30;
% [w,tw]=ricker(dt,30,.2);
[w,tw]=wavemin(dt,fdom,.2);

figure
plot(tw,w)
title('Wavelet')
xlabel('time (s)')
prepfig

tmax=dt*(1023);%make it a factor of 2 long
%run the model
rflag=0;
fpress=0;
fmult=1;
fghost=1;
zs=100;%source depth
[vspq100,t,upq100,downq100]=vspmodelqs(vp,rho,Q,z,w,tw,tmax,zr,zs,12500,rflag,fpress,fmult);

plotimage(vspq100',zr,t);
title(['Total field, source depth=' num2str(zs) 'm']);
ylabel('receiver depth');
xlabel('time');
plotimage(upq100',zr,t);
title(['Upgoing field, source depth=' num2str(zs) 'm']);
ylabel('receiver depth');
xlabel('time');
plotimage(downq100',zr,t);
% h1=line(zr,trec,'color','r');
% h2=line(zr,trec+tdr,'color','g');
title(['Downgoing field, source depth=' num2str(zs) 'm']);
ylabel('receiver depth');
xlabel('time');
%% source in overburden at z=400 (vspmodelqs)
%define wavelet and receivers
dt=.002;
fdom=30;
% [w,tw]=ricker(dt,30,.2);
[w,tw]=wavemin(dt,fdom,.2);

figure
plot(tw,w)
title('Wavelet')
xlabel('time (s)')
prepfig

tmax=dt*(1023);%make it a factor of 2 long
%run the model
rflag=0;
fpress=0;
fmult=1;
fghost=1;
zs=400;%source depth
[vspq400,t,upq400,downq400]=vspmodelqs(vp,rho,Q,z,w,tw,tmax,zr,zs,12500,rflag,fpress,fmult);

plotimage(vspq400',zr,t);
title(['Total field, source depth=' num2str(zs) 'm']);
ylabel('receiver depth');
xlabel('time');
plotimage(upq400',zr,t);
title(['Upgoing field, source depth=' num2str(zs) 'm']);
ylabel('receiver depth');
xlabel('time');
plotimage(downq400',zr,t);
% h1=line(zr,trec,'color','r');
% h2=line(zr,trec+tdr,'color','g');
title(['Downgoing field, source depth=' num2str(zs) 'm']);
ylabel('receiver depth');
xlabel('time');