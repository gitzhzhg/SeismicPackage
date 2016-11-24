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

wellname='1409.las';%las file to read
s=which('vspmodelq');
if(isempty(s))
    error('Well file for well 1409 not found, you need to load and install the CREWES toolbox')
end
ind = strfind(s,'vspmodelq');
filename=[s(1:ind-1) '1409.las'];
disp(['Well 1409 loaded from ' filename])
dzblk=5;%blocking size
dzout=1;%sample size
vp0=1600;
vs0=900;
rho0=1800;

[vins,vs,rho,z]=blocklogs(filename,dzblk,dzout,vp0,vs0,rho0);

%make universal function for comparison
vu=2500+.6*z;
%compute 2-way vertical traveltime
t=2*vint2t(vins,z);
tu=2*vint2t(vu,z);
%plot well velocity in depth and time
figure
subplot(1,2,1)
plot(vins,z,vu,z,'r');flipy
xlabel('m/s');
ylabel('depth (m)');
title('Vins in depth')
subplot(1,2,2)
plot(vins,t,vu,tu,'r');flipy
xlabel('m/s');
ylabel('time (s)');
title('Vins in time')
legend('Well','Universal');
prepfig
figure
subplot(1,2,1)
plot(z,t,z,tu,'r');flipy
title('time-depth curve \tau(z)')
ylabel('two-way time (s)');xlabel('depth (m)')
grid 
subplot(1,2,2)
plot(t,z,tu,z,'r');flipy
title('Depth-time curve z(\tau)')
xlabel('two-way time (s)');ylabel('depth (m)')
legend('Well','Universal');
grid 
prepfig
%% average velocity
vave=vint2vave(vins,t);
vaveu=vint2vave(vu,tu);
figure
plot(vins,t,'b',vave,t,'b:',vu,tu,'r',vaveu,tu,'r:');flipy
title('Comparing Vins and Vave, from well and universal function')
xlabel('m/s');
ylabel('time (s)');
prepfig
legend('Vins (W)','Vave (W)','Vins (U)','Vave (U)')
%% rms velocity (densely sampled)
vrms=vint2vrms(vins,t);
vrmsu=vint2vrms(vu,tu);
figure
plot(vins,t,'b',vave,t,'b:',vrms,t,'b-.',vu,tu,'r',vaveu,tu,'r:',vrmsu,tu,'r-.');flipy
title('Comparing Vins, Vave, Vrms, from well and universal function')
xlabel('m/s');
ylabel('time (s)');
prepfig
legend('Vins (W)','Vave (W)','Vrms (W)','Vins (U)','Vave (U)','Vrms (U)')
%% Vrms dense and sparse
% we can calculate Vrms for every point in the well log given the well log.
% However, seismic velocity analysis cannot make such dense estimates. The
% very best that can be done is an rms every .1 sesonds or so. Here we
% simulate dense versus sparse estimates
%
del2=.02;%time interval (s) between vrms calculations
t2=0:del2:max(t);%time for sparse vrms estimates
vrms2=vint2vrms(vins,t,t2);%sparse vrms
figure
plot(vins,t,vave,t,vrms,t,vrms2,t2,'k.');flipy
hold
%stairs(vrms2,t2,'linestyle','-.','color','g','marker','.');
% h=drawvint(t2,vrms2,'r','-.');
% set(h,'marker','.');
title('Vrms estimated densely and sparsely')
xlabel('velocity (m/s)');
ylabel('time (s)');
prepfig
legend('Vins','Vave','Vrms (dense)','Vrms (sparse)')
%% rms to interval
vint1=vrms2vint(vrms,t);%convert dense vrms to interval
vint2=vrms2vint(vrms2,t2);%convert sparse vrms to interval
figure
% plot(vins,t,vint1,t,'r.');flipy
plot(vins,t);flipy
hold
h1=drawvint(t,vint1,'r');
%stairs(vrms2,t2,'linestyle','-.','color','r','marker','*');
h2=drawvint(t2,vint2,'k');
set(h2,'linewidth',1);
set(h2,'marker','.');
xlabel('velocity (m/s)');
ylabel('time (s)');
title('Interval velocity from dense and sparse Vrms')
prepfig
legend('Vins','Vint from dense vrms','Vint from sparse vrms')
%% rms to interval with noise
ampfactor=1;%try values of 1, 10, and 100 for this
noisepct1=ampfactor*0.0001;%noise level for dense vrms
noisepct=ampfactor*.01;%noise level for sparse vrms
vrmsn=vrms.*(1+.01*noisepct1*randn(size(vrms)));%add noise
vrms2n=vrms2.*(1+.01*noisepct*randn(size(vrms2)));%add noise
vint1n=vrms2vint(vrmsn,t);
vint2n=vrms2vint(vrms2n,t2);
figure
subplot(1,2,1)
h1=drawvint(t,vint1,[.5 0 0]);flipy
set(h1,'linewidth',1);
hold
h1n=drawvint(t,vint1n,'r');
legend([h1 h1n],'Vint from dense Vrms no noise',['Vint from dense Vrms + ' num2str(noisepct1) '% noise'])
title('Dense Vrms to interval')
%stairs(vrms2,t2,'linestyle','-.','color','r','marker','*');
subplot(1,2,2)
h2=drawvint(t2,vint2,[.5 .5 .5]);
set(h2,'linewidth',2);
h2n=drawvint(t2,vint2n,'k');flipy
title('Sparse Vrms to interval')
set(h2,'linewidth',1);
xlabel('m/s');
ylabel('time (s)');
prepfig
legend([h2 h2n],'Vint from sparse Vrms no noise',['From sparse Vrms + ' num2str(noisepct) '% noise'])
figure
subplot(1,2,1)
plot(vrms,t,vrmsn,t,'r.');flipy
title('Dense Vrms')
legend('Vrms dense',['Vrms dense + ' num2str(noisepct1) '% noise'])
xlabel('m/s');
ylabel('time (s)');
subplot(1,2,2)
plot(vrms2,t2,vrms2n,t2,'r.');flipy
title('Sparse Vrms')
xlabel('m/s');
ylabel('time (s)');
prepfig
legend('Vrms sparse',['Vrms sparse + ' num2str(noisepct) '% noise'])
%% vrms2 inversion with different noise levels
noisepct2=[1 .5 .1 .01];
kols=[.8 .8 .8;.6 .6 .6;.3 .3 .3;.1 .1 .1];
vrms2n=cell(size(noisepct2));
h=zeros(size(noisepct2));
leg=cell(1,length(noisepct2)+1);
for k=1:length(noisepct2)
    vrms2n{k}=vrms2.*(1+.01*noisepct2(k)*randn(size(vrms2)));

    vint2n=vrms2vint(vrms2n{k},t2);
    
    if(k==1)
        figure
        plot(vins,t);flipy
        hold
        leg{1}='Vins';
    end
    h(k)=drawvint(t2,vint2n,kols(k,:));
    set(h(k),'linewidth',1);
    leg{k+1}=['From Vrms +' num2str(noisepct2(k)) '% noise'];
%     set(h,'marker','.');
end
%stairs(vrms2,t2,'linestyle','-.','color','r','marker','*');
% h=drawvint(t2,vint2n,'k');
% set(h,'linewidth',1);
% set(h,'marker','.');
xlabel('m/s');
ylabel('time (s)');
prepfig
legend(leg)
title('Noise affects interval velocities dramatically')
% figure
% plot(vrms,t,vrmsn,t,'b.',vrms2,t2,'r',vrms2n,t2,'k');flipy
% xlabel('m/s');
% ylabel('time (s)');
% prepfig
% legend('Vrms',['Vrms + ' num2str(noisepct1) '% noise'],...
%     'Vrms2',['Vrms2 + ' num2str(noisepct2) '% noise'])