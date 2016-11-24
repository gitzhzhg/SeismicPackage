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
tmax=2;
fdom=30;
[r,t]=reflec(tmax,dt);%synthetic reflectivity
[w,tw]=wavemin(dt,fdom,.2);%min phase wavelet
%now make stationary and nonstationary traces
s=convm(r,w);%stationary trace
Q=50;
qmat=qmatrix(Q,t,w,tw);%Q matrix for Q=50.
sQ=qmat*r;%nonstationary trace
figure
plot(t,s,t,sQ,'r')%compare the traces in the time domain
legend('Stationary',['Nonstationary, Q=' int2str(Q)])
prepfig
xlabel('Time (s)')
%now make an inverse Q matrix assuming an impulse wavlet
iqmat=invq(Q,t);%use the default tolerance
%apply to the nonstatinary seismogram
sQi=iqmat*sQ;
figure
hh=plot(t,s,t,sQ,'k',t,sQi,'r.');
set(hh(2),'color',[.5 .5 .5],'linewidth',1)
legend('Stationary seismogram','Nonstationary seismogram',...
    'Inverse Q matrix applied to nonstationary seismogram')
xlabel('Time (s)')
prepfig
%%
%compute inverse Q matrix applied to Qmat
w0=iqmat*qmat;
plotimage(w0,t,t)