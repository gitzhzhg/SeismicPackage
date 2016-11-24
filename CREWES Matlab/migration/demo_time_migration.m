%%
%make a section with 9 diffractions and then do a time mgration through it
%
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

%

dx=5;
nx=512;
dt=.002;
nt=1024;
t=(0:nt-1)*dt;
zmax=2000;
dz=25;
z=(0:dz:zmax);
%make rms velocity
v=1800+.6*z;
tv=2*vint2t(v,z);
dtau=8*dt;
% taumax=max(t);
taumax=1;
tau=(0:dtau:taumax)';
taucheck=linspace(0,max(tau)-dtau,20);
vrms=vint2vrms(v,tv,tau);
vave=vint2vave(v,tv,tau);
vrmsx=vrms*ones(1,nx);

znot=[100, 200, 300 400 500 600 700 800 900];%make sure these occur at integer depth steps
zv=vave.*tau/2;

x=(0:nx-1)*dx;
xnot=nx/2*dx;
seis=zeros(nt,nx);
%install the diffractions
for k=1:length(znot)
    ind=near(zv,znot(k));
    tnot=2*znot(k)/vave(ind(1));
    seis=event_hyp(seis,t,x,tnot,xnot,vrms(ind(1)),1);
end

fdom=30;
[w,tw]=ricker(dt,fdom,.2);
seis=convz(seis,w);


frange=[2 80];
[seismigt,exzos1]=pspi_stack_tmig_rms(seis,t,x,vrmsx,x,tau,frange,-taucheck);
titles1=cell(size(exzos1));
for k=1:length(taucheck)
    titles1{k}=['Focussed around tau= ' num2str(taucheck(k)) 's'];
end

%plot
plotimage(seis,t,x);
title('input')

tex=dt*(0:size(exzos1{1},1)-1);
xex=dx*(0:size(exzos1{1},2)-1);
plotgathers(exzos1,xex,tex,'distance (m)','time (s)',titles1);

plotimage(seismigt,t,x)
title(['Time migration section, dtau=' int2str(dtau/dt) '*dt'])
xlabel('distance (m)')
ylabel('migrated time (\tau) (s)')

%rerun the time migration to get the extrapolated sections output
%differently
[seismigt,exzos2]=pspi_stack_tmig(seis,t,x,vrmsx,x,tau,frange,taucheck);
titles2=cell(size(exzos2));
for k=1:length(taucheck)
    titles2{k}=['Focussed at and above tau= ' num2str(taucheck(k)) 's'];
end

%plot
plotgathers(exzos2,xex,tex,'distance (m)','time (s)',titles2);
%% depth migration of the previous
zcheck=0:100:2000;
frange=[2 80];
vel=v(:)*ones(size(x));
[seismigd,exzosd]=pspi_stack(seis,t,x,vel,x,z,frange,zcheck);
titlesd=cell(size(exzosd));
for k=1:length(titlesd)
    titlesd{k}=['Extrapolated to ' num2str(zcheck(k)) ' m'];
end
tex=dt*(0:size(exzosd{1},1)-1);
xex=dx*(0:size(exzosd{1},2)-1);
plotgathers(exzosd,xex,tex,'distance (m)','time (s)',titlesd);
plotimage(seismigd,t,x)
title(['Depth migration section, dz=' int2str(z(2)) ' m'])
xlabel('distance (m)')
ylabel('depth (m)')
%% The thrust model
%do a finite-difference model of thrust
modelname='thrust model';
dx=5;
% vlow=2000;vhigh=3500;
% xmax=5100;zmax=2500;
vlow=2500;vhigh=3145;
xmax=5100;zmax=2500;
[velt,xt,zt]=thrustmodel(dx,xmax,zmax,vhigh,vlow);
figure;imagesc(xt,zt,velt);colorbar
title(modelname)
dtt=.004; %temporal sample rate
dtstep=.001;
tmax=2*zmax/vlow; %maximum time
[seisfiltt,seis,tt]=afd_explode(dx,dtstep,dtt,tmax, ...
 		velt,xt,zeros(size(xt)),[5 10 40 50],0,2);
    
%% now a depth migration (run the previous cell first)
zcheck=0:100:2000;
frange=[2 80];
[zosmigd,exzos]=pspi_stack(seisfiltt,tt,xt,velt,xt,zt,frange,zcheck);

plotimage(zosmigd,xt,zt);
title('Depth migrated with exact model')

xs=cell(size(exzos));
ts=cell(size(exzos));
titles=cell(size(exzos));
for k=1:length(exzos)
    xs{k}=(xt(2)-xt(1))*(0:size(exzos{k},2)-1);
    ts{k}=(tt(2)-tt(1))*(0:size(exzos{k},1)-1);
    titles{k}=['Extrapolated to depth ' int2str(zcheck(k)) 'm'];
end

%load the extrapolations into plotgathers
plotgathers(exzos,xs,ts,'distance (m)','time (s)',titles);

%% now a time migration
dtau=8*dtt;
[vrmsmod,tau]=vzmod2vrmsmod(velt,zt,dtau,tmax);

frange=[2 80];
taucheck=0:.1:2;
[zosmigt,exzos]=pspi_stack_tmig_rms(seisfiltt,tt,xt,vrmsmod,xt,tau,frange,taucheck);
titles=cell(size(exzos));
for k=1:length(titles)
    titles{k}=['Focussed around tau= ' num2str(taucheck(k)) 's'];
end

%plot
tex=(tt(2)-tt(1))*(0:size(exzos{1},1)-1);
xex=(xt(2)-xt(1))*(0:size(exzos{1},2)-1);
plotgathers(exzos,xex,tex,'distance (m)','time (s)',titles);
plotimage(zosmigt,tt,xt)
title(['Time migration section, dtau=' int2str(dtau/dtt) '*dt'])
xlabel('distance (m)')
ylabel('migrated time (\tau) (s)')
%% a time migration with a single vrms function
%pick the first vms
vrms1=vrmsmod(:,1);

frange=[2 80];
taucheck=[];
[zosmigt1,exzos]=pspi_stack_tmig_rms(seisfiltt,tt,xt,vrms1,xt,tau,frange,taucheck);
% titles=cell(size(exzos));
% for k=1:length(titles)
%     titles{k}=['Focussed around tau= ' num2str(taucheck(k)) 's'];
% end

%plot
% tex=(tt(2)-tt(1))*(0:size(exzos{1},1)-1);
% xex=(xt(2)-xt(1))*(0:size(exzos{1},2)-1);
% plotgathers(exzos,xex,tex,'distance (m)','time (s)',titles);
plotimage(zosmigt1,tt,xt)
title(['Time migration section with single vrms from left edge, dtau=' int2str(dtau/dtt) '*dt'])
xlabel('distance (m)')
ylabel('migrated time (\tau) (s) ')
%% a time migration with a single vrms function
%pick the average vms
vrms2=mean(vrmsmod,2);

frange=[2 80];
taucheck=[];
[zosmigt2,exzos]=pspi_stack_tmig_rms(seisfiltt,tt,xt,vrms2,xt,tau,frange,taucheck);
% titles=cell(size(exzos));
% for k=1:length(titles)
%     titles{k}=['Focussed around tau= ' num2str(taucheck(k)) 's'];
% end

%plot
% tex=(tt(2)-tt(1))*(0:size(exzos{1},1)-1);
% xex=(xt(2)-xt(1))*(0:size(exzos{1},2)-1);
% plotgathers(exzos,xex,tex,'distance (m)','time (s)',titles);
plotimage(zosmigt2,tt,xt)
title(['Time migration section with average vrms, dtau=' int2str(dtau/dtt) '*dt'])
xlabel('distance (m)')
ylabel('migrated time (\tau) (s) ')
%% a time migration with a single vrms function
%pick the vms where the thrust outcrops
ix=near(xt,2250);
vrms3=vrmsmod(:,ix(1));

frange=[2 80];
taucheck=[];
[zosmigt3,exzos]=pspi_stack_tmig_rms(seisfiltt,tt,xt,vrms3,xt,tau,frange,taucheck);
% titles=cell(size(exzos));
% for k=1:length(titles)
%     titles{k}=['Focussed around tau= ' num2str(taucheck(k)) 's'];
% end

%plot
% tex=(tt(2)-tt(1))*(0:size(exzos{1},1)-1);
% xex=(xt(2)-xt(1))*(0:size(exzos{1},2)-1);
% plotgathers(exzos,xex,tex,'distance (m)','time (s)',titles);
plotimage(zosmigt3,tt,xt)
title(['Time migration section with vrms at thrust, dtau=' int2str(dtau/dtt) '*dt'])
xlabel('distance (m)')
ylabel('migrated time (\tau) (s) ')
%% compare the different time migrations
%Make sure you have run the previous cells first
timemigs={zosmigt;zosmigt1;zosmigt2;zosmigt3};
titles={'Migrated with exact vrms';'Migrated with left edge vrms';...
    'Migrated with average vrms';'Migrated with vrms at thrust'};
plotgathers(timemigs,xt,tt,'distance (m)','time (s)',titles);
%% time-migration of a flat section with the thrust velocity model
%make a flat section by creating a single trace and replicating it
%we use the same geometry as the thrust model
r=reflec(tmax,dtt);
fdom=30;
[w,tw]=ricker(dtt,fdom);
s=convz(r,w);
seisflat=s*ones(size(xt));
frange=[2 80];
migtimeflat=pspi_stack_tmig_rms(seisflat,tt,xt,vrmsmod,xt,tau,frange);
migdepthflat=pspi_stack(seisflat,tt,xt,velt,xt,zt,frange);
plotimage(migdepthflat,zt,xt)
title('Depth migration of flat section with thrust model')
xlabel('distance (m)')
ylabel('depth (m)')
plotimage(migtimeflat,tt,xt)
title(['Time migration of flat section with thrust vrms model, dtau=' int2str(dtau/dtt) '*dt'])
xlabel('distance (m)')
ylabel('migrated time (\tau) (s) ')
plotimage(seisflat,tt,xt)
title('Flat seismic section')
xlabel('distance (m)')
ylabel('time (s)')