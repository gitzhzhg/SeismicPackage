%this cell uses the wedge model. It is useful to understand depth migration
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

%do a finite-difference model of wedge
modelname='wedge model';
dx=5;
vlow=2000;vhigh=4000;
xmax=2500;zmax=1200;
[velw,x,z]=wedgemodel(dx,xmax,zmax,vhigh,vlow);
dt=.004; %temporal sample rate
dtstep=.001;
tmax=2*zmax/vlow; %maximum time
[seisfiltw,seis,t]=afd_explode(dx,dtstep,dt,tmax, ...
 		velw,x,zeros(size(x)),[5 10 40 50],0,2);
    
raymig(seisfiltw,velw,t,x,z,modelname)


%% THis cell uses the channel model. It is boring except for the channel diffraction
%do a finite-difference model of channel
modelname='channel model';
dx=5;
vlow=2000;vhigh=4000;
xmax=2500;zmax=1200;
[velc,x,z]=channelmodel(dx,xmax,zmax,vhigh,vlow);
dt=.004; %temporal sample rate
dtstep=.001;
tmax=2*zmax/vlow; %maximum time
[seisfiltc,seis,t]=afd_explode(dx,dtstep,dt,tmax, ...
 		velc,x,zeros(size(x)),[5 10 40 50],0,2);
    
raymig(seisfiltc,velc,t,x,z,modelname)

%% The thrust model has a shadow zone beneath the thrust
%do a finite-difference model of thrust
modelname='thrust model';
dx=5;
vlow=2000;vhigh=3500;
xmax=5100;zmax=2500;
[velt,x,z]=thrustmodel(dx,xmax,zmax,vhigh,vlow);
dt=.004; %temporal sample rate
dtstep=.001;
tmax=2*zmax/vlow; %maximum time
[seisfiltt,seis,t]=afd_explode(dx,dtstep,dt,tmax, ...
 		velt,x,zeros(size(x)),[5 10 40 50],0,2);
    
raymig(seisfiltt,velt,t,x,z,modelname)


%% depth migration of thrust using PSPI
% this is nice to compare to the raytracing in the previous cell

usesmooth=0;%0 to migrate with the exact model, 1 to use the smoothed model

if(usesmooth==1)
    zcheck=0:100:2000;
    % [zosmig,exzos]=pspi_stack(seisfiltt,t,x,velt,x,z,[5 50],zcheck);
    [zosmigsmo,exzossmo]=pspi_stack(seisfiltt,t,x,veltsmo,x,z,[5 50],zcheck);

    plotimage(zosmigsmo,x,z);
    title('Migrated with smoothed model')
    
    xs=cell(size(exzossmo));
    ts=cell(size(exzossmo));
    titles=cell(size(exzossmo));
    for k=1:length(exzossmo)
        xs{k}=(x(2)-x(1))*(0:size(exzossmo{k},2)-1);
        ts{k}=(t(2)-t(1))*(0:size(exzossmo{k},1)-1);
        titles{k}=['Extrapolated (smooth model) to depth ' int2str(zcheck(k)) 'm'];
    end
    
    %load the extrapolations into plotgathers
    plotgathers(exzossmo,xs,ts,'distance (m)','time (s)',titles);
else
    zcheck=0:100:2000;
    [zosmig,exzos]=pspi_stack(seisfiltt,t,x,velt,x,z,[5 50],zcheck);
    
    plotimage(zosmig,x,z);
    title('Migrated with exact model')
    
    xs=cell(size(exzos));
    ts=cell(size(exzos));
    titles=cell(size(exzos));
    for k=1:length(exzos)
        xs{k}=(x(2)-x(1))*(0:size(exzos{k},2)-1);
        ts{k}=(t(2)-t(1))*(0:size(exzos{k},1)-1);
        titles{k}=['Extrapolated to depth ' int2str(zcheck(k)) 'm'];
    end

    %load the extrapolations into plotgathers
    plotgathers(exzos,xs,ts,'distance (m)','time (s)',titles);
end

%% Very simple dipping reflector
%dipping reflector
%do a finite-difference model of dipping reflector
modelname='dip model';
dx=5;
vlow=2000;vhigh=4000;
xmax=2500;zmax=1200;
dip=10;
[veld,x,z]=dipmodel(dx,xmax,zmax,vhigh,vlow,dip);

%Make a ZOS
%exploding reflector
dt=.004; %temporal sample rate
dtstep=.001;
tmax=2*zmax/vlow; %maximum time
[seisfiltd,seis,t]=afd_explode(dx,dtstep,dt,tmax, ...
 		veld,x,zeros(size(x)),[5 10 40 50],0,2);

raymig(seisfiltd,veld,t,x,z,modelname)