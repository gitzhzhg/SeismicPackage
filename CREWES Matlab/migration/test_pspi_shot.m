%script to test PSPI_SHOT
%this script contains cells and you should execute the cells sequentially
%% Prepare for migration
% load the Marmousi dataset, plot sample shots, and smooth the velocity
% model
% The dataset below contains 41 shots modelled through the Marmousi model
% using afd_shotrec_alt in the CREWES toolbox. These 41 shots do not span
% the entire model, rather the shotpoints extend from 4050 to 8050 m (in
% terms of the inline cooredinate). The dataset may be downloaded from
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

% https://dl.dropbox.com/u/8337574/marmousi_newshots41.mat
load marmousi_newshots41
%this file contains lots of stuff including:
%shots_total ... length 41 cell array of 41 shots from the Marmousi model
%shots_reduced ...length 41 cell array of the same 41 shots with reduced far offset
%Note: both shots_total and shots_reduced have shots of identical size. The
%       latter just has lots of zero'd traces.
%xshots ... length 41 vector containing the shot coordinate for each shot
%t ... t coordinate for shots
%xrec ... x coordinate for receivers. Identical for all shots
%vel ... Marmousi velocity model
%x ... x coordinate for vel
%z ... z coordinate for vel
%w,tw ... wavelet used in modelling
%fdom ... dominant frequency of the wavelet

nshots=length(xshots);

%smooth the velocity model
%estimate smoother length
vmean=mean(vel(:));
lamda=vmean/fdom;
hw=5*round(lamda/10);%gaussian half width
velsmo=gaussian_smoother(vel,x,z,hw);
stab=.0001;%used in decon imaging condition
frange=[0 70];%frequency range to migrate
figure
subplot(2,1,1)
imagesc(x,z,vel)
title('Marmousi model')
xlabel('meters');ylabel('meters')
subplot(2,1,2)
imagesc(x,z,velsmo)
title('Marmousi model smoothed')
xlabel('meters');ylabel('meters')

%% this cell migrates shots_total
%shots_to_migrate=1:40;%specifies what to migrate, this says migrate all shots
%shots_to_migrate=[1 20 40];%specifies what to migrate, this says shots 1,20, and 41
shots_to_migrate=20;%this says we migrate only shot #20 out of 41 total
%shots_to_migrate=1:2:40;%specifies what to migrate, this says every other shot from 1 to 41
shots_mig_dec=cell(1,nshots);%cell array for migrated shots
shots_mig_cc=cell(1,nshots);%cell array for migrated shots
illumination=cell(1,nshots);%cell array for illuminations
%in the above assignments, if not all shots are migrated, then some cells
%in the migrated shots arrays are empty. This is intentional and is not a
%problem

%migrate the selected shots
for k=shots_to_migrate
    disp(['Migrating shot ' int2str(k)])
    [shots_mig_dec{k},shots_mig_cc{k},illumination{k}]=pspi_shot(shots_total{k},t,xrec,velsmo,x,z,xshots(k),frange,stab);
    disp(['Finished Migrating shot ' int2str(k)])
end

%stack the migrated shots
mute=[0 0;1000 1000;1500 1500];
killrad=200;
taper=100;
gainopt=0;
stackdec=migstack(shots_mig_dec,x,z,xshots,mute,killrad,taper,gainopt);
gainopt=1;
stackcc=migstack(shots_mig_cc,x,z,xshots,mute,killrad,taper,gainopt);
gainopt=0;
stackcc_ug=migstack(shots_mig_cc,x,z,xshots,mute,killrad,taper,gainopt);

plotimage(stackcc,z,x);title('Stack (shots_total) with CC imaging condition, gained');
plotimage(stackcc_ug,z,x);title('Stack (shots_total) with CC imaging condition, not gained');
plotimage(stackdec,z,x);title('Stack (shots_total) with DEC imaging condition');
plotimage(shots_total{shots_to_migrate(1)},t,xrec);title('First shot to be migrated');
plotimage(shots_mig_dec{shots_to_migrate(1)},z,x);title('First shot migration with dec imaging condition');
plotimage(shots_mig_cc{shots_to_migrate(1)},z,x);title('First shot migration with cc imaging condition');
plotimage(illumination{shots_to_migrate(1)},z,x);title('First shot illumination');
%% this cell migrates shots_reduced
%shots_to_migrate=1:41;%specifies what to migrate, this says migrate all shots
shots_to_migrate=[1 20 41];%specifies what to migrate, this says shots 1,20, and 41
%shots_to_migrate=1:2:41;%specifies what to migrate, this says every other shot from 1 to 41
shots_mig_dec=cell(1,nshots);%cell array for migrated shots
shots_mig_cc=cell(1,nshots);%cell array for migrated shots
illumination=cell(1,nshots);%cell array for illuminations
%in the above assignments, if not all shots are migrated, then some cells
%in the migrated shots arrays are empty. This is intentional and is not a
%problem

%migrate the selected shots
for k=shots_to_migrate
    disp(['Migrating shot ' int2str(k)])
    [shots_mig_dec{k},shots_mig_cc{k},illumination{k}]=pspi_shot(shots_reduced{k},t,xrec,velsmo,x,z,xshots(k),frange,stab);
    disp(['Finished Migrating shot ' int2str(k)])
end

%stack the migrated shots
mute=[0 0;1000 1000;1500 1500];
killrad=200;
taper=100;
gainopt=0;
stackdec=migstack(shots_mig_dec,x,z,xshots,mute,killrad,taper,gainopt);
gainopt=1;
stackcc=migstack(shots_mig_cc,x,z,xshots,mute,killrad,taper,gainopt);

plotimage(stackcc,z,x);title('Stack (shots_reduced) with CC imaging condition');
plotimage(stackdec,z,x);title('Stack (shots_reduced) with DEC imaging condition');
plotimage(shots_reduced{shots_to_migrate(1)},t,xrec);title('first shot to be migrated');
plotimage(shots_mig_dec{shots_to_migrate(1)},t,x);title('first shot migration with dec imaging condition');
plotimage(shots_mig_cc{shots_to_migrate(1)},t,x);title('first shot migration with cc imaging condition');
plotimage(illumination{shots_to_migrate(1)},t,x);title('first shot illumination');