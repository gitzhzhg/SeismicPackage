% make a synthetic reflectivity and a minimum phase wavelet
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

dt=.002;
tmax=2;%reflectivity length
tpad=.5;
tmaxw=.2;%wavelet length
fdom=30;
[r,t]=reflec(tmax,dt,.2,3,pi);%synthetic reflectivity
if(tpad>0)
    nt=round((tmax+tpad)/dt)+1;
    t=(0:nt-1)*dt;
    r=pad_trace(r,t);
end
[w,tw]=wavemin(dt,fdom,.2,4);%min phase wavelet
s=convm(r,w);%convolve them using convm
%now plot the results using subplot to make 3 rows in the figure
figure
subplot(3,1,1)
plot(t,pad_trace(w,r))%note the use of pad to extend the wavelet
title('wavelet')
subplot(3,1,2)
plot(t,r)
title('reflectivity')
subplot(3,1,3)
plot(t,s)
title('trace=convm(r,w)')
h1=gcf;
prepfig
posnfig
% now lets do the convolution with a convolution matrix
%build a convolution matrix from w that is the right size to convolve with
%r
cmtx=convmtx(w,length(r));%the convolution matrix
plotimage(cmtx)%view it with plotimage
title('Convolution matrix with all rows')
bigfont;whitefig
h2=gcf;
cmtxm=cmtx(1:nt,:);%grabbing the first nt rows because we want to simulate convm
plotimage(cmtxm)
title('Convolution matrix after row truncation')
bigfont;whitefig
s2=cmtxm*r;%this should be identical to s. Is it?
h3=gcf;
figure
plot(t,s,t,s2,'r.')%this plot suggests s and s2 are pretty close
legend('Convm','Convolution matrix')
a=sum(abs(s-s2))/length(s);%this is a more precise test of equivalence
b=eps;%eps tells me thee precision of my computer
title(['Convm compared to convmtx, ave error=' num2str(a) ' and eps=' num2str(b)])
prepfig
posnfig
figure(h3);figure(h2);figure(h1);
% the fact that sum(abs(s-s2)) is similar to eps tells me that s and s2
% are equivalent to machine precision.
%%
%another way to make a convolution matrix is with the qmatrix command
%Q is the measure of attenuation in rocks. A Q of infinity means no
%attenuation while a Q of 50 is lots of attenuation.
Q=50; %use this line for constant Q
%Q=linspace(30,100,length(t)); %use this line for variable Q, in this case 
%the AVERAGE Q varies from 30 at t=0 to 100 at t=max(t).
if(length(Q)==1)
    qtitle=['Nonstationary, Q=' int2str(Q)];
else
    qmin=min(Q);qmax=max(Q);
    qtitle=['Nonstationary, Qmin=' int2str(qmin) ', Qmax=' int2str(qmax)];
end
qmat=qmatrix(inf,t,w,tw);%inf means "infinity" here.
plotimage(qmat)
title('Q matrix, Q=\infty')
bigfont;whitefig
h2=gcf;
qmat2=qmatrix(Q,t,w,tw);%another matrix for Q=50.
plotimage(qmat2)
xlabel('time (sec)');ylabel('time (sec)')
title(['Q matrix, ' qtitle])
bigfig;bigfont(gcf,1.5,2);whitefig
h1=gcf;
%compare the above two Q matrices. Note the difference between stationary
%and nonstationary
sinf=qmat*r;%stationary trace
sQ=qmat2*r;%nonstationary trace
figure
plot(t,sinf,t,sQ,'r')%compare the traces in the time domain
title('Compare stationary and nonstationary traces')
legend('Stationary',qtitle)
prepfig;posnfig
h3=gcf;
xlabel('Time (s)')
figure
hh=dbspec(t,[sinf sQ]);%compare the traces in the frequency domain;
title('frequency domain comparison')
set(hh(2),'color','r')
legend('Stationary',qtitle)
prepfig;posnfig
figure(h3);figure(h2);figure(h1)
%%
%examine local spectra in two windows
t1=.2;
t2=1.2;
twin=.6;
inwin1=near(t,t1,t1+twin);%finds the samples in window 1
inwin2=near(t,t2,t2+twin);%finds the samples in window 2
[S1inf,f]=fftrl(sinf(inwin1),t(inwin2));%Spectrum in win1
S2inf=fftrl(sinf(inwin2),t(inwin2));%spectrum in win2
S1Q=fftrl(sQ(inwin1),t(inwin2));%Spectrum in win1
S2Q=fftrl(sQ(inwin2),t(inwin2));%spectrum in win2
figure
plot(f,abs(S1inf),f,abs(S2inf),'r')
xlabel('Frequency (Hz)')
ylabel('Amplitude')
title('Stationary case')
legend(['Window 1 at ' num2str(t1+.5*twin) ' s'],...
    ['Window 2 at ' num2str(t2+.5*twin) ' s'])
prepfig;posnfig
h1=gcf;
figure
plot(f,abs(S1Q),f,abs(S2Q),'r')
xlabel('Frequency (Hz)')
ylabel('Amplitude')
title(['Nonstationary case, Q=' int2str(Q)])
legend(['Window 1 at ' num2str(t1+.5*twin) ' s'],...
    ['Window 2 at ' num2str(t2+.5*twin) ' s'])
prepfig;posnfig
figure(h1)
%% view the qmatrix in the frequency domain
[Qmat2,f]=fftrl(qmat2,t);%nonstationary
[Qmat,f]=fftrl(qmat,t);%stationary
ind=near(t,0,tmax);
figure
subplot(1,2,1)
imagesc(t,f,abs(Qmat(:,ind)));colorbar
ylim([0 100])
xlabel('time (sec)')
ylabel('frequency (Hz)')
title('Stationary')
subplot(1,2,2)
imagesc(t,f,abs(Qmat2(:,ind)))
ylim([0 100])
xlabel('time (sec)')
title('Nonstationary')
ylabel('frequency (Hz)')
prepfig;posnfig
h1=gcf;
%same stuff in decibels
figure
subplot(1,2,1)
imagesc(t,f,20*log10(abs(Qmat(:,ind))));colorbar
% ylim([0 100])
xlabel('time (sec)')
ylabel('frequency (Hz)')
title('Stationary (decibels)')
subplot(1,2,2)
imagesc(t,f,20*log10(abs(Qmat2(:,ind))))
% ylim([0 100])
xlabel('time (sec)')
title('Nonstationary (decibels)')
ylabel('frequency (Hz)')
prepfig;posnfig
figure(h1)