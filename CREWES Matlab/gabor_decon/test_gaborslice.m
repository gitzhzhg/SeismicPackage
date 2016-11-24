%% test the POU construction
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

dx=10;
xmin=0;xmax=10010;
%s3epcify width and increment
xwid=200;xinc=50;
x0=xmin;
x=linspace(xmin,xmax,round((xmax-xmin)/dx)+1);
gdb=inf;
[g,nf,xnotvec,nwin]=gaussian_upou(x,x0,xwid,xinc,0,0,gdb,0);
xinc=xnotvec(2)-xnotvec(1);
testsum=zeros(size(g));
pou=zeros(nwin,length(g));
for k=1:nwin
    g=gaussian_upou(x,xnotvec(k),xwid,xinc,nf,xnotvec,gdb,0);
    testsum=testsum+g;
    pou(k,:)=g;
end

figure
subplot(3,1,1)
plot(x,nf)
title(['Normalization factor, ' int2str(nwin) ' total windows'])
subplot(3,1,2)
ninc=round(nwin/20);
plot(x,pou(1:ninc:nwin,:))
title(['Every ' int2str(ninc) '^{th} window'])
subplot(3,1,3)
plot(x,testsum)
title('Sum of the windows (should be unity)')
ylim([0 2])
%% Asymmetric POU
%this is not used by fborga so you may want to skip this. It just
%demonstrates that the POU windows need not be symmetric.
%
dx=.002;
xmin=0;xmax=1;
xwid=[.05 .2];xinc=.02;
x0=xmin;
x=linspace(xmin,xmax,round((xmax-xmin)/dx)+1);
[g,nf,xinc,nwin]=agaussian_upou(x,x0,xwid,xinc);

testsum=zeros(size(nf));
pou=zeros(nwin,length(x));
for k=1:nwin
    g=agaussian_upou(x,x0+(k-1)*xinc,xwid,xinc);
    testsum=testsum+g;
    pou(k,:)=g;
end

figure
subplot(3,1,1)
plot(x,nf)
title('Normalization factor')
subplot(3,1,2)
ninc=round(nwin/20);
plot(x,pou(1:ninc:nwin,:))
title(['Every ' int2str(ninc) '^{th} window'])
subplot(3,1,3)
plot(x,testsum)
title('Sum of the windows (should be unity)')
ylim([0 2])
%% build a nonstationary synthetic
dt=.002;
tmax=1.8;
r=reflec(tmax,dt);
rp=padpow2(r);
t=((0:length(rp)-1)*dt)';
Q=50;fdom=40;
[w,tw]=wavemin(dt,fdom,5/fdom);
qmat=qmatrix(Q,t,w,tw);
s=qmat*rp;
figure
plot(t,rp,t,5*s+.1)
title(['Reflectivity and nonstationary synthetic, Q=' int2str(Q)])
xlabel('Time (sec)')
%% compute Gabor slices
twin=.2;
tinc=.05;
padflag=1;
normflag=1;
p=1;
gdb=inf;
[slices,trow,tcol]=gaborslice(s,t,twin,tinc,p,gdb,normflag,padflag);
figure
imagesc(tcol,trow,slices);colormap('seisclrs');colorbar
title(['Gabor slices, twin=' num2str(twin) ' tinc=' num2str(tinc)]);
xlabel('Frequency (Hz)')
ylabel('Time (sec)')
%test reconstruction
sb=sum(slices,2);

figure
plot(t,s,t,sb,'r.')
err=s-sb;
rmserr=sqrt(sum(err.^2)/length(err));
srms=sqrt(sum(s.^2)/length(s));
title(['Gabor slices reconstruction rmserr= ' num2str(100*rmserr/srms) '%'])

figure
subplot(2,1,1)
plot(t,s-sb)
title('Gabor slices reconstrunction error (time domain)')
subplot(2,1,2)
dbspec(t,s-sb)
title('Gabor slices reconstruction error (frequency domain)')
%% Hilbert envelope of Slices
figure
imagesc(foutb,toutb,ins_amp(slices));colorbar
title(['Hilbert Envelope of Gabor slices, twin=' num2str(twin) ' tinc=' num2str(tinc)]);
xlabel('Frequency (Hz)')
ylabel('Time (sec)')
figure
imagesc(foutb,toutb,ins_freq(slices,toutb,10));colorbar
title(['Instantaneous frequency of Gabor slices, twin=' num2str(twin) ' tinc=' num2str(tinc)]);
xlabel('Frequency (Hz)')
ylabel('Time (sec)')
figure
imagesc(foutb,toutb,ins_phase(slices));colorbar
title(['Instantaneous phase of Gabor slices, twin=' num2str(twin) ' tinc=' num2str(tinc)]);
xlabel('Frequency (Hz)')
ylabel('Time (sec)')
%% Gabor spectrum
twin=.1;tinc=.05;
p=1;normflag=1;taperpct=200;padflag=1;
[tvsg,toutg,foutg,tnorm]=fgabor(s,t,twin,tinc,padflag,normflag,taperpct,p);
figure
imagesc(foutg,toutg,abs(tvsg));colorbar
title(['Gabor spectrum, twin=' num2str(twin) ' tinc=' num2str(tinc)]);
xlabel('Frequency (Hz)')
ylabel('Time (sec)')
%% Test inverse Gabor
sprime=igabor(tvsg,toutg,foutg,twin,tinc,p);
figure
plot(t,s,t,sprime(1:length(s)),'r.')
err=s-sprime(1:length(s));
rmserr=sqrt(sum(err.^2)/length(err));
srms=sqrt(sum(s.^2)/length(s));
title(['Gabor reconstruction rmserr= ' num2str(100*rmserr/srms) '%'])

figure
subplot(2,1,1)
plot(t,s-sprime)
title('Gabor reconstruction error (time domain)')
subplot(2,1,2)
%dbspec(t,[s s-sprime])
dbspec(t,s-sprime)
title('Gabor reconstruction error (frequency domain)')