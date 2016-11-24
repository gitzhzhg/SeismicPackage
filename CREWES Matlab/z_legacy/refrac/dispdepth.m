function dispdepth
% Display of the calculated depth model and the corresponding 
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

% standard deviation and fold for each receiver  
f=gcf;
depth=refdata('get','depth');
recelev=refdata('get','recelev');
% Find the position of the layer1-layer2 interface according 
% to the surface elevation
depthelev=recelev(2,:)-depth(2,:);
% Figure split in three part, first the surface elevation and depth average values,
% second the corresponding standard deviation, and third the fold
figure('menubar','none');
coordaxes = axes('position',[.1 .7 .8 .25]);
stdaxes = axes('position', [.1 .4 .8 .25]);
foldaxes = axes('position',[.1 .1 .8 .25]);
% Depth average values with surface elevation
axes(coordaxes);
hold on;
plot(depth(1,:),recelev(2,:),'color','y','linestyle','-')
plot(depth(1,:),depthelev,'color','g','linestyle','-.')
xmin=min(depth(1,:));
xmax=max(depth(1,:));
ymin=min(depthelev(1,:))-20;
ymax=max(recelev(2,:))+20;
axis([xmin xmax ymin ymax]);
ylabel('Elevation (m)')
title('Depth model (surface= -yellow; layer 1-2 interface= -.green)')
% Standard deviation
axes(stdaxes);
hold on;
plot(depth(1,:),depth(4,:),'color','g','linestyle','*')
ylabel('Standard deviation (m)')
% Fold
axes(foldaxes);
hold on;
plot(depth(1,:),depth(3,:),'color','g','linestyle','+')
ylabel('Fold')
xlabel('Coordinate (m)')
set(gcf,'units','pixels','position',[0 0 864 720]);
figure(f); set(gcf,'menubar','none');