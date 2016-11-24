%Demo shootrayvxz
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

if(~exist('vrand')) %clear vrand to make a new model
	disp('Enter the following parameters (don''t screw it up because you can''t go back)')
	disp('A carriage return gets the default (shown in parenthesis)')
	r=input('number of elements in x and z directions (100) ->');
	nx=r; if(isempty(nx)) nx=100; end
	r=input('grid size in meters (10) ->');
	dg=r; if(isempty(dg)) dg=10; end
	r=input('low velocity in m/s (2700) ->');
	vlow=r; if(isempty(vlow)) vlow=2700; end
	r=input('high velocity in m/s (3800) ->');
	vhigh=r; if(isempty(vhigh)) vhigh=3800; end
	r=input('size of random velocity fluctuations in m/s (500) ->');
	vdel=2*r; if(isempty(vdel)) vdel=1000; end
	
	% Define geometry
	%nx=100;
	nz=nx;
	%dg=10;
	nsmooth=1;
	xb=(0:nx+nsmooth-2)*dg;zb=(0:nz+nsmooth-2)*dg;
	x=(0:nx-1)*dg;z=(0:nz-1)*dg;
	
	%vlow=2700;vhigh=3800;vdel=1000;
	v=vlow*ones(nx+nsmooth-1,nz+nsmooth-1);
	xpoly=[max(xb)/2 2*max(xb)/3 1.5*max(xb)/2.5 max(xb)/pi];
	zpoly=[max(zb)/3 max(zb)/2 2.1*max(zb)/2.5 .4*max(zb)];
	v=afd_vmodel(dg,v,vhigh,xpoly+max(xb)/4,zpoly);
	v=afd_vmodel(dg,v,vhigh,zpoly-max(xb)/4,xpoly-max(zb)/4);
	xpoly=[min(xb) .9*max(xb)  .6*max(xb) min(xb)];
	zpoly=[.5*max(zb) .6*max(zb) max(zb) .7*max(zb)];
	v=afd_vmodel(dg,v,vhigh,xpoly,zpoly);
	vrand=vdel*(rand(nx+nsmooth-1,nz+nsmooth-1)-.5);
	v=v+vrand;
	
	disp(['V(x,z) Raytracing demo'])
	disp(' ')
	disp(' ')
	plotimage(v-mean(v(:)),z,x)
	xlabel('meters');ylabel('meters')
	disp(' ')
	disp(' ')
	disp(['Consider this velocity model'])
	disp(['Solid black is ' int2str(round(max(v(:)))) ' m/s'])
	disp(['Solid white is ' int2str(round(min(v(:)))) ' m/s'])
end

msg='Enter smoother length(meters) (0<=smoother<=500) or -1 or <cr> to end->';

r=input(msg);
if(isempty(r)) r=-1; end

while(r>=0)

smooth=r;

% Define smoother
nsmooth=2*round(.5*smooth/dg)+1;%odd number
xb=(0:nx+nsmooth-2)*dg;zb=(0:nz+nsmooth-2)*dg;
x=(0:nx-1)*dg;z=(0:nz-1)*dg;
ixcenter=1+(nsmooth-1)/2:nx+(nsmooth-1)/2;
izcenter=1+(nsmooth-1)/2:nz+(nsmooth-1)/2;

vlow=2700;vhigh=3800;vdel=1000;
v=vlow*ones(nx+nsmooth-1,nz+nsmooth-1);
xpoly=[max(xb)/2 2*max(xb)/3 1.5*max(xb)/2.5 max(xb)/pi];
zpoly=[max(zb)/3 max(zb)/2 2.1*max(zb)/2.5 .4*max(zb)];
v=afd_vmodel(dg,v,vhigh,xpoly+max(xb)/4,zpoly);
v=afd_vmodel(dg,v,vhigh,zpoly-max(xb)/4,xpoly-max(zb)/4);
xpoly=[min(xb) .9*max(xb)  .6*max(xb) min(xb)];
zpoly=[.5*max(zb) .6*max(zb) max(zb) .7*max(zb)];
v=afd_vmodel(dg,v,vhigh,xpoly,zpoly);
v(ixcenter,izcenter)=v(ixcenter,izcenter)+vrand;
% run a smoother over it
vun=v; %save the unsmoothed model
t1=clock;
v=conv2(vun,ones(nsmooth,nsmooth)/(nsmooth*nsmooth),'valid');
t2=clock;
deltime=etime(t2,t1);
disp(['smoothing time ' num2str(deltime) ' seconds']);
plotimage(v-mean(v(:)),z,x)
xlabel('meters');ylabel('meters')
title(['Raytracing after ' int2str(smooth) ' m smoother'])
%install the velocity model
rayvelmod(v,dg);

%estimate tmax,dt,tstep
tmax=max(z)/vlow;dt=.001;tstep=0:dt:tmax;

%specify a fan of rays
angles=[-70:2.5:70]*pi/180;
x0=round(nx/2)*dg;z0=0;
indx=near(x,x0);indz=near(z,z0);
v0=v(indz,indx);

%trace the rays
t1=clock;
tsnap=tmax;
for k=1:length(angles)
	r0=[x0 z0 sin(angles(k))/v0 cos(angles(k))/v0];
	[t,r]=shootrayvxz(tstep,r0);
	line(r(:,1),r(:,2),ones(size(t)),'color','r');
    tmp=max(t);
    %check for a ray that hits the top
    ind=find(tmp==t);
    if(r(ind,2)<dg) tmp=tsnap; end % toss this one
    if(tmp<tsnap)
        tsnap=tmp; %snapshot time will be the minimum maximum found on any ray
    end
end
t2=clock;
deltime=etime(t2,t1);
disp(['raytrace time ' num2str(deltime) ' seconds']);

%finitedifference simulation
%dt=.004; %temporal sample rate
%dtstep=.001;%time step size
%tmax=2*zmax/vlow; %maximum time
r=input('Enter 1 to use the smooth model for finite dif. CR for unsmoothed');
tsnap
if(isempty(r))
	iuse=(nsmooth-1)/2+1:nx+(nsmooth-1)/2;
	snap1=zeros(size(vun(iuse,iuse)));
	xshot=max(x)/3;
	snap2=snap1; ix=near(x,x0);
	snap2(1:2,ix(1)-1:ix(1)+1)=1;
    dtuse=dt/2;
	[w,tw]=wavemin(dtuse,40,tsnap);
	snapshotn=afd_snapn(dg,dtuse,vun(iuse,iuse),snap1,snap2,tsnap,2,2,w);
else
	snap1=zeros(size(v));
	xshot=max(x)/3;
	snap2=snap1; ix=near(x,x0);
	snap2(1:2,ix(1)-1:ix(1)+1)=1;
	[w,tw]=wavemin(dt,40,tsnap);
	snapshotn=afd_snapn(dg,dt,v,snap1,snap2,tsnap,2,2,w);
end
% [shotf,shot,t]=afd_shotrec(dx,dtstep,dt,tmax, ...
% 		vun,snap1,snap2,x,zeros(size(x)),[5 10 40 50],0,lap);
% figure(velfig)
% line(xshot,0,1,'marker','*','markersize',6,'color','r');
% text(xshot+.02*(max(x)-min(x)),0,1,'shot point','color','r');
%plot the seismogram
plotimage(snapshotn,z,x)
xlabel('offset');ylabel('seconds');
%plot rays on top
for k=1:length(angles)
	r0=[x0 z0 sin(angles(k))/v0 cos(angles(k))/v0];
	[t,r]=shootrayvxz(tstep,r0);
    ind=near(t,tsnap);
    iuse=1:ind(1);
	line(r(iuse,1),r(iuse,2),ones(size(t(iuse))),'color','r');
end

r=input(msg);
if(isempty(r)) r=-1; end


end

disp(' ')
disp('You should look at the source file for this demo')
disp('to see how its done. Also type "help raytrace" for more info.') 