%% test fomel
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

%chirp test
dt=.001;%sample rate
tmax=2;%signal length
fmin=10;%sweep start freq
fmax=100;%sweep end freq
tsmo=tmax/10;%fomel's smoother length
[s,t]=sweep(fmin,fmax,dt,tmax);
s=s';
%lambda=norm(s);
lambda=.001;%fomel's lambda
t1=clock;
[freqloc,freqins]=fomelfreq(s,t,tsmo,lambda);
t2=clock;
twin=.1;tinc=.01;%gabor window half width and increment
freqlocg=gaborfreq(s,t,twin,tinc,4);
t3=clock;
figure
subplot(3,1,1)
plot(t,s)
xlabel('Seconds');
title('Signal')
subplot(3,1,2)
plot(t,freqins)
xlabel('Seconds');
title('Instantaneous frequency')
subplot(3,1,3)
plot(t,freqloc,t,freqlocg,'r')
dtf=etime(t2,t1);
dtg=etime(t3,t2);
legend('fomel','gabor')
xlabel('Seconds');
title(['Local frequency, Compute times: (fomel)=' num2str(dtf) ' (gabor)=' num2str(dtg)])
%% test of convolutional model
dt=.001;
tmax=2;
fdom=30;%dominant frequency of Ricker wavelet
tsmo=tmax/2;%fomel's smoother
[r,t]=reflec(tmax,dt);
[w,tw]=ricker(dt,fdom,.2);
s=convz(r,w);
lambda=.001;%fomel's lambda
t1=clock;
[freqloc,freqins]=fomelfreq(s,t,tsmo,lambda);
t2=clock;
twin=.2;tinc=.01;%Gabor window half width and increment
freqlocg=gaborfreq(s,t,twin,tinc,2);
t3=clock;
figure
subplot(3,1,1)
plot(t,s)
xlabel('Seconds');
title('Signal')
subplot(3,1,2)
plot(t,freqins)
xlabel('Seconds');
title('Instantaneous frequency')
subplot(3,1,3)
plot(t,freqloc,t,freqlocg,'r')
dtf=etime(t2,t1);
dtg=etime(t3,t2);
xlabel('Seconds');
title(['Local frequency, Compute times: (fomel)=' num2str(dtf) ' (gabor)=' num2str(dtg)])
legend('fomel','gabor')
%% superposition of two sine waves
f1=40;%frequency of sine 1
f2=60;%frequency of sine 2
A1=1;%amplitude of sine 1
A2=1;%amplitude of sine 2
dt=.001;
tmax=2;
t=0:dt:tmax;
s=A1*sin(2*pi*f1*t)+A2*sin(2*pi*f2*t);
tsmo=tmax/2;%fomel's smoother
lambda=.001;
t1=clock;
[freqloc,freqins]=fomelfreq(s,t,tsmo,lambda);
t2=clock;
twin=.2;tinc=.01;
freqlocg=gaborfreq(s,t,twin,tinc,2);
t3=clock;
figure
subplot(3,1,1)
plot(t,s)
xlabel('Seconds');
title('Signal')
subplot(3,1,2)
plot(t,freqins)
xlabel('Seconds');
title('Instantaneous frequency')
subplot(3,1,3)
plot(t,freqloc,t,freqlocg,'r')
dtf=etime(t2,t1);
dtg=etime(t3,t2);
xlabel('Seconds');
title(['Local frequency, Compute times: (fomel)=' num2str(dtf) ' (gabor)=' num2str(dtg)])
legend('fomel','gabor')
%% nonstationary test
Q=50;%Q value
dt=.001;
tmax=2;
fdom=30;%dominant frequency of source (i.e. near beginning of trace)
tsmo=tmax/10;%fomel's smoother
[r,t]=reflec(tmax,dt);
[w,tw]=wavemin(dt,fdom,.2);
qmat=qmatrix(Q,t,w,tw);
s=qmat*r;
lambda=.001;
t1=clock;
[freqloc,freqins]=fomelfreq(s,t,tsmo,lambda);
t2=clock;
twin=.1;tinc=.01;
freqlocg=gaborfreq(s,t,twin,tinc,2);
t3=clock;
figure
subplot(3,1,1)
plot(t,s)
xlabel('Seconds');
title('Signal')
subplot(3,1,2)
plot(t,freqins)
xlabel('Seconds');
title('Instantaneous frequency')
subplot(3,1,3)
plot(t,freqloc,t,freqlocg,'r')
dtf=etime(t2,t1);
dtg=etime(t3,t2);
xlabel('Seconds');
title(['Local frequency, Compute times: (fomel)=' num2str(dtf) ' (gabor)=' num2str(dtg)])
legend('fomel','gabor')
ifig=gcf;
%spectral plot
figure
[tvs,trow,fcol]=fgabor(s,t,twin,tinc);
nplots=5;
ifreqs=near(fcol,1,150);
iplot=1:floor(length(trow)/(nplots-1)):length(trow);
for k=1:nplots
subplot(nplots,1,k)
plot(fcol(ifreqs),abs(tvs(iplot(k),ifreqs)))
title(['Spectrum at t=' num2str(trow(iplot(k)))])
set(gca,'xminorgrid','on');
% grid
end
pos=get(ifig,'position');
set(ifig,'position',[pos(1)+pos(3)/2 pos(2) pos(3) pos(4)])