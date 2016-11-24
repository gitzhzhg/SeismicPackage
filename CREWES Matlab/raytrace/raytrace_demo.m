% RAYTRACE_DEMO: interactive demonstration of v(z) raytracing capabilities
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
'7 ... Controlled incidence angle P-P and P-S reflections',...
'8 ... Show velocity model',...
'9 ... Display raytrace help file',...
'0 or CR ... Stop this foolishness');

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
%Trace P-P rays and plot in upper subplot
[t,p,L,raycoord]=traceray_pp(vp,zp,zsrc,zrec,zd,xoff,100,-1,10,1,1,2);
%put source and receiver markers
line(xoff,zrec*ones(size(xoff)),'color','b','linestyle','none','marker','v')
line(0,zsrc,'color','r','linestyle','none','marker','*')
%annotate plot
title('OBC simulation, P-P mode, water depth 200 meters')
xlabel('meters');ylabel('meters');grid
%plot traveltime versus offset in lower subplot
subplot(2,1,2);flipy;
plot(xoff,t);grid;xlabel('meters');ylabel('seconds')
xlim([0 max(xoff)])

figure;subplot(2,1,1);flipy
%Trace P-S rays and plot in upper subplot 
[t,p,L,raycoord]=traceray_ps(vp,zp,vs,zs,zsrc,zrec,zd,xoff,10,-1,10,1,1,2);
%put source and receive markers
line(xoff,zrec*ones(size(xoff)),'color','b','linestyle','none','marker','v')
line(0,zsrc,'color','r','linestyle','none','marker','*')
%annotate plot
title('OBC simulation, P-S mode, water depth 200 meters')
grid;xlabel('meters');ylabel('meters');
subplot(2,1,2);flipy;
plot(xoff,t);grid;xlabel('meters');ylabel('seconds');
xlim([0 max(xoff)])

pos=get(gcf,'position');
set(gcf,'position',[1.1*pos(1) .9*pos(2) pos(3:4)])

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


zd=250:50:3000;
zd=fliplr(zd);
zsrc=50;
zrec=200;
xoff=1500;

%loop over depth and show conversion point
for kk=1:length(zd);
	if(kk==1)dflag=1;else;dflag=2;end
	[t,p]=traceray_ps(vp,zp,vs,zs,zsrc,zrec,zd(kk),xoff,10,-2,30,1,1,dflag);
end
%draw source and receiver symbols
line(xoff,zrec,'color','b','linestyle','none','marker','v')
line(0,zsrc,'color','r','linestyle','none','marker','*');
%annotate plot
title('OBC simulation, P-S mode, fixed offset CCP determination');grid;

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
[t,p]=traceray_pp(vp,zp,zsrc,zrec,zd,xoff,10,-1,10,1,1,2);
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
[t,p]=traceray_ps(vp,zp,vs,zs,zsrc,zrec,zd,xoff,10,-1,10,1,1,2);
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

zrec=500:100:2500;%Receiver depths
%we assume a vertical borehole
zsrc=0;%source depth
zd=3000;%reflector depth
xoff=1500;%horizontal offset between source and any receiver
%build the velocity model
zp=0:10:4000;vp=1800+.6*zp;vs=.5*vp;zs=zp;
%P-P offset VSP
figure;subplot(2,1,1);flipy
t=zeros(size(zrec)); %preallocate t
%loop over receiver depth
for kk=1:length(zrec);
	[t(kk),p]=traceray_pp(vp,zp,zsrc,zrec(kk),zd,xoff,10,-2,30,1,1,2);
end
%draw source and receiver symbols
line(xoff,zrec,'color','b','linestyle','none','marker','v')
line(0,zsrc,'color','r','linestyle','none','marker','*')
%annotation
title([' VSP Vertical gradient simulation, P-P mode '])
grid;xlabel('meters');ylabel('meters');
%plot traveltime versus depth
subplot(2,1,2);
plot(t,zrec);xlabel('seconds');ylabel('depth (meters)')
grid;flipy;ylim([0 3000])

zrec=500:100:2500;%desired receiver depths
zsrc=0;%source depth
zd=3000;%reflector depth
%P-S offset VSP
figure;subplot(2,1,1);flipy;
t=zeros(size(zrec));%preallocate t
for kk=1:length(zrec);
	[t(kk),p]=traceray_ps(vp,zp,vs,zs,zsrc,zrec(kk),zd,xoff,10,-2,30,1,1,2);
end
%draw source and reciver symbols
line(xoff,zrec,'color','b','linestyle','none','marker','v')
line(0,zsrc,'color','r','linestyle','none','marker','*')
%annotate plot
title([' VSP Vertical gradient simulation, P-S mode '])
grid;xlabel('meters');ylabel('meters');
%plot traveltime versus depth
subplot(2,1,2);
plot(t,zrec);xlabel('seconds');ylabel('depth (meters)');
grid;flipy;ylim([0 3000])

pos=get(gcf,'position');
set(gcf,'position',[1.1*pos(1) .9*pos(2) pos(3:4)])

%
% multiples
%

case {6}
zp=0:10:4000;
vp=1800+.6*zp;
vs=.5*vp;
zs=zp;


%define the ray code for a pure P multiple
raycode=[0 1;1500 1;1300 1;2000 1;1800 1;3000 1;2000 1;2300 1;1000 1;...
			 1500 1; 300 1];
figure;subplot(2,1,1);flipy
%trace the rays
xoff=1000:100:3000;
[t,p]=traceray(vp,zp,vs,zs,raycode,xoff,10,-1,10,1,1,2);
%Source and receiver symbols
line(xoff,raycode(end,1)*ones(size(xoff)),'color','b','linestyle','none','marker','v')
line(0,raycode(1,1),'color','r','linestyle','none','marker','*')
%annotate
title('A P-P-P-P-P-P-P-P-P-P mode in vertical gradient media');grid
%Plot traveltimes
subplot(2,1,2);flipy
plot(xoff,t);grid;xlabel('offset');ylabel('time')
xlim([0 3000])

%define the ray code for a P-S multimode
raycode=[0 1;1500 2;1300 2;2000 2;1800 2;3000 1;2000 1;2300 1;1000 1;...
			 1500 2; 300 1];
figure;subplot(2,1,1);flipy
%trace the rays
xoff=1000:100:3000;
[t,p]=traceray(vp,zp,vs,zs,raycode,xoff,10,-1,10,1,1,2);
%source and receiver symbols
line(xoff,raycode(end,1)*ones(size(xoff)),'color','b','linestyle','none','marker','v')
line(0,raycode(1,1),'color','r','linestyle','none','marker','*')
%annotate
title('A P-S-S-S-S-P-P-P-P-S mode in vertical gradient media');grid
%Plot traveltimes
subplot(2,1,2);flipy
plot(xoff,t);grid;xlabel('offset');ylabel('time')
xlim([0 3000])

pos=get(gcf,'position');
set(gcf,'position',[1.1*pos(1) .9*pos(2) pos(3:4)])

case {7}
%
% continuous gradient constant incidence angle demo
%

%get the angles
angs=input('Enter min and max angles separated by a space (e.g.: 0 45)>>','s');
ang=sscanf(angs,'%g%g',[1,2]);
if(isempty(ang))
    angmin=45;
    angmax=45;
else
    angmin=ang(1);
    if(length(ang)>1)
        angmax=ang(2);
    else
        angmax=angmin;
    end
end
numangles=10;%we trace this many rays
angles=linspace(angmin,angmax,numangles);
if(max(diff(angles))>5)
    numangles=20;
    angles=linspace(angmin,angmax,numangles);
end
%build velocities
zp=0:10:4000;
vp=1800+.6*zp;
vs=.5*vp;
zs=zp;
zsrc=100;
zrec=0;
zd=3000;
%xoff=1000:100:3000;

figure;subplot(2,1,1);flipy;
[t,p,xoff]=angleray_pp(vp,zp,zsrc,zrec,zd,angles,2);
title(['Controlled incidence angle, P-P mode, angmin=' num2str(angmin) ' angmax=' num2str(angmax)])
line(xoff,zrec*ones(size(xoff)),'color','b','linestyle','none','marker','v')
line(0,zsrc,'color','r','linestyle','none','marker','*')
grid;xlabel('meters');ylabel('meters');
subplot(2,1,2);flipy;
plot(xoff,t);grid;
xlabel('meters');ylabel('seconds');

figure;subplot(2,1,1);flipy;
pos=get(gcf,'position');
set(gcf,'position',[1.1*pos(1) .9*pos(2) pos(3:4)])
[t,p,xoff]=angleray_ps(vp,zp,vs,zs,zsrc,zrec,zd,angles,2);
title(['Controlled incidence angle, P-S mode, angmin=' num2str(angmin) ' angmax=' num2str(angmax)])
line(xoff,zrec*ones(size(xoff)),'color','b','linestyle','none','marker','v')
line(0,zsrc,'color','r','linestyle','none','marker','*')
grid;xlabel('meters');ylabel('meters');
subplot(2,1,2);flipy;
plot(xoff,t);grid;
xlabel('meters');ylabel('seconds');     

case {8}

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

case {0}
%help raytrace
break

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