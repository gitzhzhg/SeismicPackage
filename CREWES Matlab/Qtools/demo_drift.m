%load a well log and block it
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

filename='1409.las';%las file to read
dzblk=5;%blocking size
dzout=1;%sample size
vp0=1600;
vs0=900;
rho0=1800;

[vp,vs,rho,z]=blocklogs(filename,dzblk,dzout,vp0,vs0,rho0);

%invent a Q
Qmin=20;Qmax=500;
vp0=1500;vp1=4500;
rho0=1800;rho1=3000;
[Q,Qrand]=fakeq(vp,rho,Qmin,Qmax,2,vp0,vp1,rho0,rho1,1,1);

figure
plot(vp,z,rho,z,10*Q,z);flipy
title('Well model')
ylabel('Depth (m)');
xlabel('Velocity or Density (MKS units) and Q')
legend('Velocity','Density','10*Q');
prepfig


%calculate and disply the drift time
f1=40;
f0=12500;
tdr=2*tdrift(Q,z,f1,vp,f0);%multiply by 2 for two-way time
t0=2*vint2t(vp,z);
figure
subplot(1,2,1)
plot(t0,z,t0+tdr,z);flipy
legend('Traveltime at 12500 Hz','Traveltime at 40 Hz')
ylabel('Depth (m)')

grid
subplot(1,2,2)
plot(tdr,z);flipy
legend('Drift time')
ylabel('Depth (m)')
xlabel('time (s)')
grid
prepfig