% demo dynamic time warping, smooth dynamic time warping and time-variant crosscorrelation 
% create s1 and s2 related by time shifts
N=2001;
dt=0.001;
tmax=(N-1)*dt;
[r,t]=reflec(tmax,dt);
fdom=30;
tlength=0.2;
wm=wavemin(dt,fdom,tlength);
s1=convm(r,wm);
tshift=0.03*sin(3*t);% create time shifts
s2=drift_corr(s1,t,tshift);
nshift=tshift/dt;% the known lag sequence

figure
subplot(2,1,1)
plot(t,s1)
hold on
plot(t,s2,'r')
xlabel('two-way traveltime(s)')
ylabel('amplitude')
xlim([min(t) max(t)])
ylim([-1.1*max(abs(s1)) 1.1*max(abs(s1))])
legend('s_1:original','s_2:time shifted')
L=50;
x=maxcorr(s1,s2,L);
x1=round(100*x(1))/100;
x2=round(100*x(2)*dt*1000)/100;
title(['max corr=' num2str(x1) ' at lag=' num2str(x2) ' ms'])

subplot(2,1,2)
plot(t,tshift)
xlabel('two-way traveltime(s)')
ylabel('time shifts(s)')
xlim([min(t) max(t)])
ylim([-1.2*max(abs(tshift)) 1.2*max(abs(tshift))])
%% dynamic time warping to estimate time shifts
L=50;
b=2;
[e,d,nshift_DTW] = DTW(s1,s2,L,b);
tshift_DTW=nshift_DTW*dt;

% plot alignment error array
figure
imagesc(1:length(s1),-L:L,e)
hold on
plot(1:length(t),nshift,'w','linewidth',0.7)% plot the known lag sequence
colorbar
flipy
xlabel('n: sample number');
ylabel('m: lag');
title('alignment error array e');

% plot distance array
figure
imagesc(1:length(s1),-L:L,d)
hold on
plot(1:length(t),nshift_DTW,'w','linewidth',0.7)% plot the estimated lag sequence
colorbar
flipy
xlabel('n: sample number');
ylabel('m: lag');
title('distance array d');

% compare known and estimated time shifts
figure
subplot(2,1,1)
plot(t,tshift,'b')
hold on
plot(t,tshift_DTW,'r')
xlim([0 max(t)])
ylim([-1.2*max(abs(tshift)) 1.2*max(abs(tshift))])
xlabel('two-way traveltime(s)')
ylabel('time shifts(s)')
legend('known','estimated by DTW')

% examine time shift estimation
s1_corr=drift_corr(s1,t,tshift_DTW);
subplot(2,1,2)
plot(t,s1_corr,'b')
hold on
plot(t,s2,':r','linewidth',0.8)
xlabel('two-way traveltime(s)')
ylabel('amplitude')
xlim([min(t) max(t)])
ylim([-1.1*max(abs(s1)) 1.1*max(abs(s1))])
legend('time shifted s_1 by DTW','s_2')
L=50;
x=maxcorr(s1_corr,s2,L);
x1=round(100*x(1))/100;
x2=round(100*x(2)*dt*1000)/100;
title(['max corr=' num2str(x1) ' at lag=' num2str(x2) ' ms'])
%% smooth dynamic time warping to estimate time shifts
h=100;% the coarse sample interval
j=1:h:N;
L=50;
[~,d,~,nshift_sDTW]=DTWs(s1,s2,L,j);
tshift_sDTW=nshift_sDTW*dt;

% plot distance array
figure
imagesc(1:length(d),-L:L,d)
hold on
plot(1:length(t),nshift_sDTW,'w','linewidth',0.7)% plot the estimated lag sequence
colorbar
flipy
xlabel('n: sample number');
ylabel('m: lag');
title('distance array d');

% compare known and estimated time shifts
figure
subplot(2,1,1)
plot(t,tshift,'b')
hold on
plot(t,tshift_sDTW,'r')
xlim([0 max(t)])
ylim([-1.2*max(abs(tshift)) 1.2*max(abs(tshift))])
xlabel('two-way traveltime(s)')
ylabel('time shifts(s)')
legend('known','estimated by SDTW')

% examine time shift estimation
s1_corrs=drift_corr(s1,t,tshift_sDTW');
subplot(2,1,2)
plot(t,s1_corrs,'b')
hold on
plot(t,s2,':r','linewidth',0.8)
xlabel('two-way traveltime(s)')
ylabel('amplitude')
xlim([min(t) max(t)])
ylim([-1.1*max(abs(s1)) 1.1*max(abs(s1))])
legend('time shifted s_1 by SDTW','s_2')
L=50;
x=maxcorr(s1_corrs,s2,L);
x1=round(100*x(1))/100;
x2=round(100*x(2)*dt*1000)/100;
title(['max corr=' num2str(x1) ' at lag=' num2str(x2) ' ms'])
%% time-variant crosscorrelation to estimate time shifts
twin=.05;
tinc=.01;
maxlag=100;
[ccshift,ccmax,s1_shift]=tvccorr(s1,s2,t,twin,tinc,maxlag);

% compare known and estimated time shifts
figure
subplot(3,1,1)
plot(t,tshift)
hold on
plot(t,ccshift,':r','linewidth',0.8)
xlim([0 max(t)])
ylim([-0.1 0.1])
ylabel('time shifts(s)')
legend('known','estimated by TVCC')

% plot time-variant crosscorrelation coefficient
subplot(3,1,2)
plot(t,ccmax)
ylim([-1 1.2])
set(gca,'ytick',[-1 0 1])
xlim([0 max(t)])
title('time-variant crosscorrelation coefficient')

% examine time shift estimation
subplot(3,1,3)
plot(t,s1_shift);
hold on
plot(t,s2,':r','linewidth',0.8);
xlabel('two-way traveltime(s)')
ylabel('amplitude')
xlim([min(t) max(t)])
ylim([-.02 .02])
legend('time shifted s_1 by TVCC','s_2')
L=50;
x=maxcorr(s1_shift,s2,L);
x1=round(100*x(1))/100;
x2=round(100*x(2)*dt*1000)/100;
title(['max corr=' num2str(x1) ' at lag=' num2str(x2) ' ms'])
