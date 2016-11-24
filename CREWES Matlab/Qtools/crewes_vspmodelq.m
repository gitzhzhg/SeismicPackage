%% Cell 1: build a very simple model
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
%make a very simple model
dz=10;%depth sample size
zmax=2000;%maximum depth

%interface depths
z1=zmax/10;%first interface
z2=75*zmax/200;%second interface (formula gives 750 if zmax=2000)
z3=135*zmax/200;%third interface interface (formula gives 1350 if zmax=2000)

%layer properties
v1=1500;%velocity above first interface
v2=1000;%velocity above second interface
v3=2000;%velocity above third interface
v4=1000;%velocity below third interface
Q1=30;%Q above 0th interface
Q2=15;%Q above first interface
Q3=30;%Q between interfaces
Q4=40;%Q below second interface
r1=3500;%density above 0th interface
r2=2000;%density above first interface
r3=5000;%density between interfaces
r4=2000;%density below second interface

%specify wavelet and time sampling
waveflag=1;%make 0 for Ricker, 1 for min phase
fdom=30;%wavelet dominant frequency
dt=0.002;%time sample interval
tlen=.2;%wavelet length
tmax=3;%maximum record length

%DO NOT MAKE EDITS PAST HERE for Cell 1

z=(0:dz:zmax)';%define depth vector
vp=zeros(size(z));
rho=vp;
Q=vp;

i1=near(z,z1);
i2=near(z,z2);
i3=near(z,z3);
vp(1:i1)=v1;
vp(i1:i2)=v2;
vp(i2:i3)=v3;
vp(i3:end)=v4;
Q(1:i1)=Q1;
Q(i1:i2)=Q2;
Q(i2:i3)=Q3;
Q(i3:end)=Q4;
rho(1:i1)=r1;
rho(i1:i2)=r2;
rho(i2:i3)=r3;
rho(i3:end)=r4;

figure
xmax=max([max(vp) max(rho) max(10*Q)]);
xmin=min([min(vp) min(rho) min(10*Q)]);
xmax=ceil(xmax/100)*100+500;
xmin=floor(xmin/100)*100-500;
if(xmin<0);xmin=0;end
plot(vp,z,rho,z,10*Q,z);flipy
xlim([xmin xmax])
title('Simple model')
ylabel('Depth (m)');
xlabel('Velocity or Density or 10*Q (MKS units)')
legend('Velocity','Density','10*Q');
prepfig
grid
h1=gcf;
posnfig(gcf,.4,.5);

if(waveflag==0)
    [w,tw]=ricker(dt,fdom,tlen);
elseif(waveflag==1)
    [w,tw]=wavemin(dt,fdom,tlen);
else
    error('invalid waveflag')
end
figure
tint=tw(1):dt/5:tw(end);
w2=interpbl(tw,w,tint);
plot(tint,w2)
title('Wavelet')
xlabel('time (s)')
prepfig
posnfig(gcf,.6,.5)
figure(h1)

%% Cell 2: run the simple model (run Cell 1 first)

%specify receivers
zrec1=0;%first receiver depth
zrecmax=1500;%maximum receiver depth
dzrec=dz;%interval between receivers

%vspmodelq parameters
% rflag ... 0 means calculate reflectivity with complex velocities
%           1 means calculate reflectivity with input velocities
% ********* default = 0 **********
% fpress ... 0 means a displacement seismogram
%            1 means a pressure seismogram
% ********* default = 0 ***********
% fmult ... 0 means primaries only, no internal multiples but still a surface multiple
%       ... 1 means all multiples
%       ... -1 means no internal multiples and no surface multiples
%       ... 2 means internal multiples but no surface multiples
%       ... 3 means no multiples and no transmission loss
% ********* default = 1 ***********
rflag=0;
fpress=1;
fmult=1;
% f0 ... frequency at which vp has been measured
f0=12500;

%DO NOT MAKE EDITS PAST HERE in Cell 2

zr=zrec1:dzrec:zrecmax;% receiver depths

%compute a time depth curve from the well velocities
tz=vint2t(vp,z);
%interpolate times at each receiver depth
trec=interp1(z,tz,zr);
%compute drift times
td=tdrift(Q,z,fdom,vp,f0);
%interpolate at the receiver depth
tdr=interp1(z,td,zr);

%run the model

[vspq,tq,upq,downq]=vspmodelq(vp,rho,Q,z,w,tw,tmax,zr,f0,rflag,fpress,fmult);

plotimage(vspq',zr,tq);
title(['Total field, rflag=' num2str(rflag)] );
ylabel('receiver depth');
xlabel('time');
posnfig(gcf,.3,.6)
h1=gcf;

plotimage(upq',zr,tq);
title(['Upgoing field, rflag=' num2str(rflag)] );
ylabel('receiver depth');
xlabel('time');
posnfig(gcf,.7,.6);
h2=gcf;

plotimage(downq',zr,tq);
hline1=line(trec,zr,'color','r');
hline2=line(trec+tdr,zr,'color','g');
title(['Downgoing field, rflag=' num2str(rflag)]);
ylabel('receiver depth');
xlabel('time');
legend([hline1 hline2],'time at well velocity','time at seismic velocity');
posnfig(gcf,.3,.4);
h3=gcf;

figure
zspecial=1000;%receiver depth to examine
irec=near(zr,zspecial);%points to receiver at zspecial
trace=vspq(:,irec);%grab total field at zspecial
amp=max(abs(trace))/3;
ind=near(tq,trec(irec-1));
p1=nan*zeros(size(tq));
p1(ind)=amp;
ind=near(tq,trec(irec-1)+tdr(irec-1));
p2=nan*zeros(size(tq));
p2(ind)=amp;
subplot(3,1,1)
plot(tq,trace,tq,p1,'r.',tq,p2,'k.')
yl=get(gca,'ylim');
xlabel('Time (s)')
title('Total field')
legend(['Receiver at ' num2str(zspecial) 'm'],'Well velocity time','Seismic velocity time')

subplot(3,1,2)
trace=downq(:,irec);%grab downgoing field at zspecial
plot(tq,trace)
ylim(yl)
xlabel('Time (s)')
title('Downgoing field')

subplot(3,1,3)
trace=upq(:,irec);%grab upgoing field at zspecial
plot(tq,trace)
ylim(yl)
xlabel('Time (s)')
title('Upgoing field')
prepfig
bigfont(gcf,.8,1);
legendfontsize(.8);
posnfig(gcf,.7,.4);
figure(h3);figure(h2);figure(h1);


%% Cell 3: Read well 1409 and block it in prep for a more complex model
%load a well log and block it

% define the overbuden and blocking parameters
vp0=1600;%p-wave velocity at the surface
vs0=900;%s-wave velocity at the surface (not used in this simulation)
rho0=1800;%density at the surface
dzblk=1;%blocking size
dzout=1;%sample size
%Note: dzout should be <= dzblk

%define the fake Q parameters
Q1=20; %low reference Q
Q2=200; %high reference Q
vp1=1500; %velocity corresponding to Q1
vp2=4500; %velocity corresponding to Q2
rho1=1800; %density corresponding to Q1
rho2=3000; %density corresponding to Q2

%specify wavelet and time sampling
waveflag=1;%make 0 for Ricker, 1 for min phase
fdom=30;%wavelet dominant frequency
dt=0.002;%time sample interval
tlen=0.2;%length of wavelet
tmax=3;%maximum record length

%DO NOT MAKE EDITS PAST HERE in Cell 3

%NOTE: This script is hardwired to read in LAS file 1409.las from the same
%folder in the CREWES toolbox that contains vspmodelq.m . To read in a
%different well, put the las file in your current Matlab directory, comment
%out the lines below marked with ***, and set the variables filename and
%wellname to be identical and equal to a string containing your LAS file
%name. These last two steps are indicated by the lines marked with ###
wellname='1409.las';%las file to read ***
s=which('vspmodelq');% ***
if(isempty(s)) %***
    error('Well file for well 1409 not found, you need to load and install the CREWES toolbox') %***
end %***
ind = strfind(s,'vspmodelq'); %***
filename=[s(1:ind-1) '1409.las']; %***
disp(['Well 1409 loaded from ' filename]) %***

% wellname='mywell.las'; %###
% filename=wellname; %###

%read the LAS file, block the logs, and attach the overburden
[vp,vs,rho,z]=blocklogs(filename,dzblk,dzout,vp0,vs0,rho0);

%make the fake Q
[Q,Qrand]=fakeq(vp,rho,Q1,Q2,2,vp1,vp2,rho1,rho2,1,1);

figure
plot(vp,z,rho,z,10*Q,z);flipy
title(['Well ' wellname, ', dzblk=' num2str(dzblk) ', dzout=' num2str(dzout)])
ylabel('Depth (m)');
xlabel('Velocity or Density (MKS units) and Q')
legend('Velocity','Density','10*Q');
prepfig
posnfig
h1=gcf;

if(waveflag==0)
    [w,tw]=ricker(dt,fdom,tlen);
elseif(waveflag==1)
    [w,tw]=wavemin(dt,fdom,tlen);
else
    error('invalid waveflag')
end
figure
tint=tw(1):dt/5:tw(end);
w2=interpbl(tw,w,tint);
plot(tint,w2)
title('Wavelet')
xlabel('time (s)')
prepfig
posnfig(gcf,.6,.5)
figure(h1)
%% Cell 4: create the VSP on the blocked logs model from Cell 3 (run Cell 3 first)
%specify receivers
zrec1=0;%first receiver depth
zrecmax=1500;%maximum receiver depth
dzrec=dz;%interval between receivers
zr=zrec1:dzrec:zrecmax;%specify the receivers

%vspmodelq parameters
% rflag ... 0 means calculate reflectivity with complex velocities
%           1 means calculate reflectivity with input velocities
% ********* default = 0 **********
% fpress ... 0 means a displacement seismogram
%            1 means a pressure seismogram
% ********* default = 0 ***********
% fmult ... 0 means primaries only, no internal multiples but still a surface multiple
%       ... 1 means all multiples
%       ... -1 means no internal multiples and no surface multiples
%       ... 2 means internal multiples but no surface multiples
%       ... 3 means no multiples and no transmission loss
% ********* default = 1 ***********
rflag=0;
fpress=0;
fmult=1;
% f0 ... frequency at which vp has been measured
f0=12500;

%DO NOT MAKE EDITS PAST HERE in Cell 4

[vspq,tq,upq,downq]=vspmodelq(vp,rho,Q,z,w,tw,tmax,zr,f0,rflag,fpress,fmult);

%compute a time depth curve from the well velocities
tz=vint2t(vp,z);
%interpolate times at each receiver depth
trec=interp1(z,tz,zr);
%compute drift times
td=tdrift(Q,z,fdom,vp,f0);
%interpolate at the receiver depth
tdr=interp1(z,td,zr);

plotimage(vspq',zr,tq);
title(['Total field, dzblk=' num2str(dzblk) ', dzout=' num2str(dzout)]);
ylabel('receiver depth');
xlabel('time');
title(['Total field, dzblk=' num2str(dzblk) ', dzout=' num2str(dzout)]);
posnfig(gcf,.3,.6)
h1=gcf;

plotimage(upq',zr,tq);
title(['Upgoing field, dzblk=' num2str(dzblk) ', dzout=' num2str(dzout)]);
ylabel('receiver depth');
xlabel('time');
posnfig(gcf,.7,.6);
h2=gcf;

plotimage(downq',zr,tq);
hline1=line(trec,zr,'color','r');
hline2=line(trec+tdr,zr,'color','g');
title(['Downgoing field, dzblk=' num2str(dzblk) ', dzout=' num2str(dzout)]);
ylabel('receiver depth');
xlabel('time');
legend([hline1 hline2],'time at well velocity','time at seismic velocity');
posnfig(gcf,.3,.4);
h3=gcf;

figure
zspecial=1000;%receiver depth to examine
irec=near(zr,zspecial);%points to receiver at zspecial
trace=vspq(:,irec);%grab total field at zspecial
amp=max(abs(trace))/3;
ind=near(tq,trec(irec-1));
p1=nan*zeros(size(tq));
p1(ind)=amp;
ind=near(tq,trec(irec-1)+tdr(irec-1));
p2=nan*zeros(size(tq));
p2(ind)=amp;
subplot(3,1,1)
plot(tq,trace,tq,p1,'r.',tq,p2,'k.')
yl=get(gca,'ylim');
xlabel('Time (s)')
title('Total field')
legend(['Receiver at ' num2str(zspecial) 'm'],'Well velocity time','Seismic velocity time')

subplot(3,1,2)
trace=downq(:,irec);%grab downgoing field at zspecial
plot(tq,trace)
ylim(yl)
xlabel('Time (s)')
title('Downgoing field')

subplot(3,1,3)
trace=upq(:,irec);%grab upgoing field at zspecial
plot(tq,trace)
ylim(yl)
xlabel('Time (s)')
title('Upgoing field')
prepfig
bigfont(gcf,.8,1);
legendfontsize(.8);
posnfig(gcf,.7,.4);
figure(h3);figure(h2);figure(h1);

%% Cell 5: Calculate Q (run cell 4 first)
trec1=0;%time of FB at zrec1
trecmax=.5;%time of FB at zrecmax

z0=400;%first receiver depth to compare to
% z0=400:100:1000; %uncomment this line to get a constant shift calculation
zq=800:100:1400;%vector of second receiver depths
% twin ... temporal window legth for spectral estimation
% f1 ... lowest trustworthy signal frequency
% f2 ... highest trustworthy signal frequency
% method ... choose 'specrat' (spectral ratio), 'specmatch' (spectral
%       matching), 'domfreq' (dominant frequency matching), or 'all'.
%       See qestimator.m for more information.
% spectype ... choose 'fourier','burg',or 'multi'. See qestimator.m for more
%       information.
% pflags ... vector the same length as zq. All entries must be either zero
%       or 1. A value of 1 causes a plot to be produced showing the result
%       of the Q estimation. A value of 0 makes no plot.
% ********* default = zeros(size(zq)) ***************
% wintype ... 'boxcar' 
%             'cosine' (raised cosine)
%             'gaus' Gaussian truncated at two standard deviations
%             'mwin'  see mwindow.m (20% taper)
%             'mwhalf' see mwhalf.m (20% taper). Intended for VSPs
% NOTE: wintype only matters if spectype='fourier'
% ************ default = 'mwhalf' **************
twin=.2;%size of the temporal window that begins at the FB and us used for Q estimation
f1=10;%lowest frequency used in the spectral ratio calculation
f2=60;%highest frequency used in the spectral ratio calculation
method='specrat';%the method used
spectype='fourier';%the type of spectrum
wintype='mwhalf';%the type of window
pflags=zeros(size(zq));
pflags(end)=1;%determines where analysis plots are made

%DO NOT MAKE EDITS PAST HERE in Cell 5

%pick first breaks
tp=VSP_pick(vspq,tq,zr,[trec1 trecmax],[zrec1 zrecmax]);

t=tq;
[Qint,z0,zq]=VSP_Q(downq,t,zr,tp,zq,z0,twin,f1,f2,method,spectype,pflags,wintype);%downq is the mathematically perfect downgoingwave
h1=gcf;
%calculate expected Q's
Qint_log=zeros(size(Qint));
for k=1:length(Qint_log)
    Qint_log(k)=qz2qint(Q,vp,z,z0(k),zq(k),fdom,12500);
end
figure
h0=plot(z,Q,'color',[.3 .3 .3]);
hint1=plotqint(Qint_log,z0,zq,'b','o',[.5 .1]);
hint2=plotqint(Qint,z0,zq,'r','*',[.5 .1]);
legend([h0 hint1(1) hint2(1)],'instantaneous Q=Q_{ins}','Average Q from Qins','Measured Q from VSP\_Q');
title({['VSP Q estimates, twin=' num2str(twin) 's, (f1,f2)=(' num2str(f1) ',' num2str(f2) ')Hz, ' method ', ' spectype];'Dashed lines show averaging interval'})
xlabel('Depth (m)');ylabel('Q value')
prepfig
bigfont(gcf,.8,1)
legendfontsize(.8)
figure(h1)

%% Cell 6: Calculate a buried source VSP
%specify source depth
zsource=100;
%specify receivers
zrec1=0;%first receiver depth
zrecmax=1500;%maximum receiver depth
dzrec=dz;%interval between receivers
zr=zrec1:dzrec:zrecmax;%specify the receivers

%vspmodelqs parameters
% rflag ... 0 means calculate reflectivity with complex velocities
%           1 means calculate reflectivity with input velocities
% ********* default = 0 **********
% fpress ... 0 means a displacement seismogram
%            1 means a pressure seismogram
% ********* default = 0 ***********
% fmult ... 0 means primaries only, no internal multiples but still a surface multiple
%       ... 1 means all multiples
%       ... -1 means no internal multiples and no surface multiples
%       ... 2 means internal multiples but no surface multiples
%       ... 3 means no multiples and no transmission loss
% ********* default = 1 ***********
rflag=0;
fpress=0;
fmult=1;
% f0 ... frequency at which vp has been measured
f0=12500;

%DO NOT MAKE EDITS PAST HERE in Cell 5

[vspq,t,upq,downq]=vspmodelqs(vp,rho,Q,z,w,tw,tmax,zr,zsource,12500,rflag,fpress,fmult);

plotimage(vspq',zr,t);
title(['Total field, source depth=' num2str(zsource) 'm']);
ylabel('receiver depth');
xlabel('time');
posnfig(gcf,.3,.6)
h1=gcf;

plotimage(upq',zr,t);
title(['Upgoing field, source depth=' num2str(zsource) 'm']);
ylabel('receiver depth');
xlabel('time');
posnfig(gcf,.7,.6);
h2=gcf;


plotimage(downq',zr,t);
title(['Downgoing field, source depth=' num2str(zsource) 'm']);
ylabel('receiver depth');
xlabel('time');
posnfig(gcf,.3,.4);
h3=gcf;

figure
zspecial=1000;%receiver depth to examine
irec=near(zr,zspecial);%points to receiver at zspecial
trace=vspq(:,irec);%grab total field at zspecial
amp=max(abs(trace))/3;

subplot(3,1,1)
plot(tq,trace)
yl=get(gca,'ylim');
xlabel('Time (s)')
title('Total field')
legend(['Receiver at ' num2str(zspecial) 'm'])

subplot(3,1,2)
trace=downq(:,irec);%grab downgoing field at zspecial
plot(tq,trace)
ylim(yl)
xlabel('Time (s)')
title('Downgoing field')

subplot(3,1,3)
trace=upq(:,irec);%grab upgoing field at zspecial
plot(tq,trace)
ylim(yl)
xlabel('Time (s)')
title('Upgoing field')
prepfig
bigfont(gcf,.8,1);
legendfontsize(.8);
posnfig(gcf,.7,.4);

figure(h3);figure(h2);figure(h1);