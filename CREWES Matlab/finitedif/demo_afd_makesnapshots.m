% we will make a cell array of snapshots showing wave propagation in the marmousi model
% The snapshots will be displayed in two alternative tools called
% plotgathers and plotsnaps. These are similar tools except that the latter
% displays the seismic gathers on top of the velocity model. Both tools
% have a gui interface that allows easy comparison of the snapshots and 
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

% easy creation of a movie.

%specify the major parameters
dx=10;%spatial grid size (m), dx must be either 5, 10 or 25
dt=.001;%temporal step size (s)
%Note: dx and dt must be chosen to satisfy the stability condition for the
%4th order (in space) second order (in time) scalar wave equation. This
%means that max(vel(:))*dt/dx<=sqrt(3/8) or the code will not run. 
tmax=2;%record length (s)
fdom=30;%desired dominant frequency (Hz)
dtmovie=.1;%the time sample rate of the movie (s) 
%The number of frames in the movie is nframes=floor(tmax/dtmovie)+1
laplacian=2;%this means the fourth-order laplacian
boundary=2;%The top surface is reflecting and the other 3 sides are absorbing

% make the velocity model
[vel,x,z]=marmousimodel(dx);%dx must be either 5, 10 or 25

%specify source locations by inserting suitable 1's in snap2
snap1=zeros(size(vel)); %this is usually zero

% ***beginning of source specification***

%set sourceflag with the following meanings
% 1 ... single source point in the middle of the model at the surface (z=0)
% 2 ... four separate source points placed around the model
% 3 ... a source point in the reservoir
% 4 ... a planewave that dips down to the left

sourceflag=1;

if(sourceflag==1)
  xshot=mean(x);%x coordinate of the shot (single shot)
  zshot=0;
elseif(sourceflag==2)
    xshot=[xshot-3000, xshot xshot xshot+3000];%4 shot pattern
    zshot=[1500 100 2900 1500];%z coordinate of the shot  
elseif(sourceflag==3)
    xshot=6700;zshot=2470;%shot in the reservoir
elseif(sourceflag==4)
    x1=0;x2=8000;%start and end x coordinates for the plane-wave source
    z1=500;z2=0; %start and end z coordinates for the plane-wave source. This source dips down to the left.
    xshot=x1:dx:x2;%a continusous source from x1 to x2
    zshot=interp1([x1 x2],[z1 z2],xshot);%interpolate source depths
end

snap2=snap1;
for k=1:length(xshot)
    ixshot=near(x,xshot(k));
    izshot=near(z,zshot(k));
    snap2(izshot,ixshot)=1;
end  

% ***end of source specification***

%make a causal wavelet
[w,tw]=wavemin(dt,fdom,tmax);

%specify the snashop times
tsnaps=0:dtmovie:tmax;

%make the movie
snaps=afd_makesnapshots(dx,dt,vel,snap1,snap2,tsnaps,laplacian,boundary,w);

%make the snapshot titles
titles=cell(size(tsnaps));
for k=1:length(tsnaps)
    titles{k}=[' Snapshot at time ' num2str(tsnaps(k)) ' s'];
end

%show the results with plotgathers
plotgathers(snaps,x,z,'distance (m)','depth (m)',titles);
posnfig(gcf,.4,.5)
%show the results with plotsnaps
plotsnaps(snaps,vel,x,z,'distance (m)','depth (m)',titles,'Marmousi model');
posnfig(gcf,.6,.5)
%save the result for later


if(sourceflag==1)
    save marmsnaps_single_source snaps x z titles vel dx dt fdom w tw
elseif(sourceflag==2)
    save marmsnaps_4_sources snaps x z titles vel dx dt fdom w tw
elseif(sourceflag==3)
    save marmsnaps_shot_in_reservoir snaps x z titles vel dx dt fdom w tw
elseif(sourceflag==4)
    save marmsnaps_planewave snaps x z titles vel dx dt fdom w tw
end
    
%% Replot a saved result with a different colormap
load marmsnaps_single_source
plotsnaps(snaps,vel,x,z,'distance (m)','depth (m)',titles,'Marmousi model','jet');