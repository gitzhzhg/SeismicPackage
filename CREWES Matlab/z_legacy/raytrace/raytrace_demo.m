%
% Demo of simple raytracing in matlab
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

disp(['Raytracing demo'])
options=str2mat(...
'1 ... P-P and P-S OBC recording',...
'2 ... P-S conversion point for OBC',...
'3 ... P-P and P-S for continuous gradient',...
'4 ... P-S conversion point for continuous gradient',...
'5 ... VSP P-P and P-S for continuous gradient',...
'6 ... Multiples in continuous gradient media',...
'7 ... Show velocity model',...
'8 ... Display raytrace help file',...
'9 ... Stop this foolishness');

disp(options)
r=input('Type the number of your selection->');
if(isempty(r)) r=0; end
vp=[];

while(r~=size(options,1))

switch(r)
case {1}

%
% OBC primaries
%

%OBC recording
%make pwave model
zp=[0 200 300 450 750 1000 1700 2500 3500 4000];
vp=[1500 1600 2000 2250 2700 2100 3200 3750 4000 4200];
%make s-wave model
zs=zp;
vs=[0 300 700 1000 1250 1500 1800 1500 2100 1900];
zsrc=50;
zrec=200;
zd=3000;
xoff=1000:100:3000;

%simulate source in the middle of the water layer and receiver on the bottom
figure;subplot(2,1,1);flipy
[t,p]=traceray_pp(vp,zp,zsrc,zrec,zd,xoff,10,-1,10,1,1,-gcf);
title('OBC simulation, P-P mode, water depth 200 meters')
line(xoff,zrec*ones(size(xoff)),'color','b','linestyle','none','marker','v')
line(0,zsrc,'color','r','linestyle','none','marker','*')
xlabel('meters');ylabel('meters')
grid
subplot(2,1,2);flipy;
plot(xoff,t);grid
xlabel('meters');ylabel('seconds')

figure;subplot(2,1,1);flipy
pos=get(gcf,'position');
set(gcf,'position',[1.1*pos(1) .9*pos(2) pos(3:4)])
[t,p]=traceray_ps(vp,zp,vs,zs,zsrc,zrec,zd,xoff,10,-1,10,1,1,-gcf);
title('OBC simulation, P-S mode, water depth 200 meters')
line(xoff,zrec*ones(size(xoff)),'color','b','linestyle','none','marker','v')
line(0,zsrc,'color','r','linestyle','none','marker','*')
grid;xlabel('meters');ylabel('meters');
subplot(2,1,2);flipy;
plot(xoff,t);grid;
xlabel('meters');ylabel('seconds');

%
% find ccp in OBC case
%
case {2}
%make pwave model
zp=[0 200 300 450 750 1000 1700 2500 3500 4000];
vp=[1500 1600 2000 2250 2700 2100 3200 3750 4000 4200];
%make s-wave model
zs=zp;
vs=[0 300 700 1000 1250 1500 1800 1500 2100 1900];

%loop over depth and show conversion point
zd=250:50:3000;
zd=fliplr(zd);
zsrc=50;
zrec=200;
xoff=1500;

for kk=1:length(zd);
	if(kk==1)dflag=1;else;dflag=2;end
	[t,p]=traceray_ps(vp,zp,vs,zs,zsrc,zrec,zd(kk),xoff,10,-2,30,1,1,dflag);
end
title('OBC simulation, P-S mode, fixed offset CCP determination')
line(xoff,zrec,'color','b','linestyle','none','marker','v')
line(0,zsrc,'color','r','linestyle','none','marker','*')
grid;

%
% linear increase of velocity with depth
%
case{3}

%
% continuous gradient demo
%
zp=0:10:4000;
vp=1800+.6*zp;
vs=.5*vp;
zs=zp;
zsrc=100;
zrec=500;
zd=3000;
xoff=1000:100:3000;

figure;subplot(2,1,1);flipy;
[t,p]=traceray_pp(vp,zp,zsrc,zrec,zd,xoff,10,-1,10,1,1,-gcf);
title(['Vertical gradient simulation, P-P mode zsrc=' num2str(zsrc) ' zrec=' num2str(zrec)])
line(xoff,zrec*ones(size(xoff)),'color','b','linestyle','none','marker','v')
line(0,zsrc,'color','r','linestyle','none','marker','*')
grid;xlabel('meters');ylabel('meters');
subplot(2,1,2);flipy;
plot(xoff,t);grid;
xlabel('meters');ylabel('seconds');

figure;subplot(2,1,1);flipy;
pos=get(gcf,'position');
set(gcf,'position',[1.1*pos(1) .9*pos(2) pos(3:4)])
[t,p]=traceray_ps(vp,zp,vs,zs,zsrc,zrec,zd,xoff,10,-1,10,1,1,-gcf);
title(['Vertical gradient simulation, P-S mode zsrc=' num2str(zsrc) ' zrec=' num2str(zrec)])
line(xoff,zrec*ones(size(xoff)),'color','b','linestyle','none','marker','v')
line(0,zsrc,'color','r','linestyle','none','marker','*')
grid;xlabel('meters');ylabel('meters');
subplot(2,1,2);flipy;
plot(xoff,t);grid;
xlabel('meters');ylabel('seconds');

%
% find ccp for continuous gradient case
%

case{4}
zp=0:10:4000;
vp=1800+.6*zp;
vs=.5*vp;
zs=zp;
%loop over depth and show conversion point
zsrc=100;
zrec=500;
zd=fliplr(600:50:3000);
%zd=1000;
xoff=1500;
for kk=1:length(zd);
	if(kk==1)dflag=1;else;dflag=2;end
	[t,p]=traceray_ps(vp,zp,vs,zs,zsrc,zrec,zd(kk),xoff,10,-2,30,1,1,dflag);
end
title(['Vertical gradient simulation, P-S mode zsrc=' num2str(zsrc) ' zrec=' num2str(zrec)])
line(xoff,zrec,'color','b','linestyle','none','marker','v')
line(0,zsrc,'color','r','linestyle','none','marker','*')
grid

%
%
% vsp simulation
%
%
case{5}
zp=0:10:4000;
vp=1800+.6*zp;
vs=.5*vp;
zs=zp;
zrec=500:100:2500;
zsrc=0;
zd=3000;
xoff=1500;
figure;subplot(2,1,1);flipy
t=zeros(size(zrec));
for kk=1:length(zrec);
	if(kk==1)dflag=-gcf;else;dflag=2;end
	[t(kk),p]=traceray_pp(vp,zp,zsrc,zrec(kk),zd,xoff,10,-2,30,1,1,dflag);
end
title([' VSP Vertical gradient simulation, P-P mode '])
line(xoff,zrec,'color','b','linestyle','none','marker','v')
line(0,zsrc,'color','r','linestyle','none','marker','*')
grid;xlabel('meters');ylabel('meters');
subplot(2,1,2);flipy;
plot(t,zrec);xlabel('seconds');ylabel('depth (meters)')
grid

zrec=500:100:2500;
zsrc=0;
zd=3000;
figure;subplot(2,1,1);flipy;
pos=get(gcf,'position');
set(gcf,'position',[1.1*pos(1) .9*pos(2) pos(3:4)])
t=zeros(size(zrec));
for kk=1:length(zrec);
	if(kk==1)dflag=-gcf;else;dflag=2;end
	[t(kk),p]=traceray_ps(vp,zp,vs,zs,zsrc,zrec(kk),zd,xoff,10,-2,30,1,1,dflag);
end
title([' VSP Vertical gradient simulation, P-S mode '])
line(xoff,zrec,'color','b','linestyle','none','marker','v')
line(0,zsrc,'color','r','linestyle','none','marker','*')
grid;xlabel('meters');ylabel('meters');
subplot(2,1,2);flipy;
plot(t,zrec);xlabel('seconds');ylabel('depth (meters)');
grid;

%
% multiples
%

case {6}
zp=0:10:4000;
vp=1800+.6*zp;
vs=.5*vp;
zs=zp;
xoff=1000:100:3000;

raycode=[0 1;1500 1;1000 1;2300 1;2000 1;3000 1;2000 1;2300 1;1000 1; 1500 1; 0 1];
figure;
subplot(2,1,1);flipy
[t,p]=traceray(vp,zp,vs,zs,raycode,xoff,10,-1,10,1,1,-gcf);
title('A P-P-P-P-P-P-P-P-P-P mode in vertical gradient media');
line(xoff,zeros(size(xoff)),'color','b','linestyle','none','marker','v')
line(0,0,'color','r','linestyle','none','marker','*')
grid
subplot(2,1,2);flipy
plot(xoff,t);
grid;
xlabel('offset');ylabel('time')

raycode=[0 1;1500 2;1000 2;2300 2;2000 2;3000 1;2000 1;2300 1;1000 1; 1500 2; 0 1];
figure;
subplot(2,1,1);flipy
pos=get(gcf,'position');
set(gcf,'position',[1.1*pos(1) .9*pos(2) pos(3:4)])
[t,p]=traceray(vp,zp,vs,zs,raycode,xoff,10,-1,10,1,1,-gcf);
title('A P-S-S-S-S-P-P-P-P-S mode in vertical gradient media');
line(xoff,zeros(size(xoff)),'color','b','linestyle','none','marker','v')
line(0,0,'color','r','linestyle','none','marker','*')
grid
subplot(2,1,2);flipy
plot(xoff,t);
grid;
xlabel('offset');ylabel('time')



case {7}

if(isempty(vp))
	disp(' ')
	disp('******** run another option first to define velocities ********')
	disp(' ')
else
	figure;
	drawvint(zp,vp,'b');
	drawvint(zs,vs,'r');
	legend('Vp','Vs')
	xlabel('meters/sec');ylabel('depth (meters)')
	flipy;grid
end

case {8}
help raytrace

otherwise
disp(' ')
disp('****** invalid selection *********')
disp(' ')

end
disp(' ')
disp(options)
r=input('Type the number of your selection->');
if(isempty(r)) r=0; end
end
disp(' ')
disp('You should look at the source file for this demo')
disp('to see how its done. Also type "help raytrace" for more info.') 