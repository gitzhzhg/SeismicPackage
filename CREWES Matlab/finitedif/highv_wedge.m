% HIGHV_WEDGE: model an anticline beneath a high velocity wedge
%
% high velocity wedge
% Just run the script
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
dx=5; %cdp interval
xmax=2500;zmax=1000; %maximum line length and maximum depth
xpinch=1500; % wedge pinchout coordinates
zwedge=zmax/2; % wedge maximum depth
x=0:dx:xmax; % x coordinate vector
z=0:dx:zmax; % z coordinate vector
vhigh=4000;vlow=2000; % high and low velocities

%initialize velocity matrix as a constant matrix full of vlow
vel=vlow*ones(length(z),length(x));

% define the wedge as a three point polygon
dx2=dx/2;
xpoly=[-dx2 xpinch -dx2];zpoly=[-1 -1 zwedge];

% install the wedge in the velocity matrix
vel=afd_vmodel(dx,vel,vhigh,xpoly,zpoly);

% define an anticline beneath the wedge

x0=xpinch/2;z0=zwedge+100; % x and z of the crest of the anticline
a=.0005; % a parameter that determines the steepness of the flanks
za=a*(x-x0).^2+z0; % model the anticline as a parabola

% build a polygon that models the anticline
ind=near(za,zmax+dx);
xpoly=[x(1:ind) 0 ];zpoly=[za(1:ind) za(ind)];

%install the anticline in the velocity model
vel=afd_vmodel(dx,vel,vhigh,xpoly,zpoly);

% bottom layer
xpoly=[0 xmax xmax 0];zpoly=[.9*zmax .9*zmax zmax+dx zmax+dx];

vel=afd_vmodel(dx,vel,vhigh,xpoly,zpoly);

%plot the velocity model
plotimage(vel-.5*(vhigh+vlow),z,x)
xlabel('meters');ylabel('meters')
velfig=gcf;
msg=str2mat(' Type 1 for a shot record (AFD_SHOTREC),','2 for a shot record (AFD_SHOTREC_ALT),',...
        '3 for a VSP (AFD_SHOTREC), 4 for exploding reflector (AFD_EXPLODE),',...
        'A blank line will exit');
disp(msg)
r=input(' response -> ')

while ~isempty(r)
switch(r)
case{1}
	%do a shot record with afd_shotrec
    lap=0;
    while(lap~=1 && lap ~=2)
        disp(' Type 1 for 2nd order Laplacian, 2 for 4th order Laplacian')
        lap=input(' response-> ');
    end
	dt=.004; %temporal sample rate
    dtstep=.0005;%time step size
	tmax=2*zmax/vlow; %maximum time
	snap1=zeros(size(vel));
	xshot=max(x)/3;
	snap2=snap1; ix=near(x,xshot);
	snap2(1,ix(1))=1;
	[shotf,shot,t]=afd_shotrec(dx,dtstep,dt,tmax, ...
 		vel,snap1,snap2,x,zeros(size(x)),[5 10 40 50],0,lap);
	figure(velfig)
	line(xshot,0,1,'marker','*','markersize',6,'color','r');
	text(xshot+.02*(max(x)-min(x)),0,1,'shot point','color','r');
	%plot the seismogram
	plotimage(shotf,t,x-xshot)
   xlabel('offset');ylabel('seconds');
   if(lap==1)
       title('Shot record from AFD_SHOTREC, second order laplacian')
   else
       title('Shot record from AFD_SHOTREC, fourth order laplacian')
   end
   case{2}
	%do a shot record with afd_shotrec_alt
    lap=0;
    while(lap~=1 && lap ~=2)
        disp(' Type 1 for 2nd order Laplacian, 2 for 4th order Laplacian')
        lap=input(' response-> ');
    end
    xshot=max(x)/3;
    zshot=0.0;
	dt=.004; %temporal sample rate
    dtstep=.0005;%time step size
	tmax=2*zmax/vlow; %maximum time
    %minimum phase wavelet
    [w,tw]=wavemin(dtstep,20,tmax,5);
	[seis,t]=afd_shotrec_alt(dx,dtstep,dt,tmax, ...
 		vel,xshot,zshot,x,zeros(size(x)),w,tw,lap);
	figure(velfig)
	line(xshot,0,1,'marker','*','markersize',6,'color','r');
	text(xshot+.02*(max(x)-min(x)),0,1,'shot point','color','r');
	%plot the seismogram
	plotimage(seis,t,x-xshot)
   xlabel('offset');ylabel('seconds');
    if(lap==1)
       title('Shot record from AFD_SHOTREC_ALT, second order laplacian')
   else
       title('Shot record from AFD_SHOTREC_ALT, fourth order laplacian')
   end
case{3}
	%do a vsp reflector model
	lap=0;
    while(lap~=1 && lap ~=2)
        disp(' Type 1 for 2nd order Laplacian, 2 for 4th order Laplacian')
        lap=input(' response-> ');
    end
	dt=.004; %temporal sample rate
	dtstep=.0005;%time step size
	tmax=2*zmax/vlow; %maximum time
	snap1=zeros(size(vel));
	xshot=max(x)/4;
	snap2=snap1; ix=near(x,xshot);
	snap2(1,ix(1))=1;
	izrec=near(z,max(z)/4,3*max(z)/4);
	zrec=z(izrec);
	xrec=(max(x)/3)*ones(size(zrec));
	[vspf,vsp,t]=afd_shotrec(dx,dtstep,dt,tmax, ...
 		vel,snap1,snap2,xrec,zrec,[5 10 40 50],0,lap);
	figure(velfig)
	line(xshot,0,1,'marker','*','markersize',6,'color','g');
	text(xshot+.02*(max(x)-min(x)),0,1,'vsp shot','color','g');
	line(xrec,zrec,ones(size(zrec)),'marker','v','markersize',6,'color','g');
	%plot the vsp
	plotimage(vspf',zrec,t)%vsp's are normally plotted with depth as vert coordinate
   ylabel('depth');xlabel('seconds');
case{4}
	%do an exploding reflector model
	lap=0;
    while(lap~=1 && lap ~=2)
        disp(' Type 1 for 2nd order Laplacian, 2 for 4th order Laplacian')
        lap=input(' response-> ');
    end
	dt=.004; %temporal sample rate
	dtstep=.001;
	tmax=2*zmax/vlow; %maximum time
	[seisfilt,seis,t]=afd_explode(dx,dtstep,dt,tmax, ...
 		vel,x,zeros(size(x)),[5 10 40 50],0,lap);
	%plot the seismogram
	plotimage(seisfilt,t,x)
   xlabel('meters');ylabel('seconds')
otherwise
	disp('invalid selection')
end

disp(msg)
r=input(' response ->')

end