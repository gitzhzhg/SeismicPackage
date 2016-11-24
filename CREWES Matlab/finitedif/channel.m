% CHANNEL: model a channel beneath a few layers
%
% low velocity channel beneath a v(z) medium
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

% Just run the script
disp(' enter channel width in meters (between 20 and 1000, <cr> to end)')

width=input(' response -> ')

while(~isempty(width))
disp(' enter 1 for exploding reflector, 2 for shot record')
record_type=input(' response -> ')
if(record_type==1)
    disp(' enter spatial grid size in m. A number between 2 and 20 is a good choice')
    dx=input('response -> ');
    if(dx==0) dx=5; end
    xmax=2500;zmax=1000;%grid size, max line length, max depth
    x=0:dx:xmax;z=0:dx:zmax; % x and z coordinate vector
    vhigh=4000;vlow=2000;vrange=vhigh-vlow; % high and low velocities
    vel=vlow*ones(length(z),length(x));%initialize velocity matrix
    z1=100;z2=200;v1=vlow+vrange/5;%first layer
    xpoly=[-dx xmax+dx xmax+dx -dx];zpoly=[z1 z1 z2 z2];
    vel=afd_vmodel(dx,vel,v1,xpoly,zpoly);%install first layer
    z3=271;v2=vlow+2*vrange/5;zpoly=[z2 z2 z3 z3];%second layer
    vel=afd_vmodel(dx,vel,v2,xpoly,zpoly);%install second layer
    z4=398;v3=vlow+pi*vrange/5;zpoly=[z3 z3 z4 z4];%third layer
    vel=afd_vmodel(dx,vel,v3,xpoly,zpoly);%install third layer
    zpoly=[z4 z4 zmax+dx zmax+dx];%last layer
    vel=afd_vmodel(dx,vel,vhigh,xpoly,zpoly);%install last layer
    thk=50;vch=vlow+vrange/6;%channel
    xpoly=[xmax/2-width/2 xmax/2+width/2 xmax/2+width/2 xmax/2-width/2];
    zpoly=[z4 z4 z4+thk z4+thk];
    vel=afd_vmodel(dx,vel,vch,xpoly,zpoly);%install channel
    plotimage(vel-.5*(vhigh+vlow),z,x);%plot the velocity model

    %do a finite-difference exploding reflector model
    dt=.004; %temporal sample rate
    lap=2;%laplacian flag
    dtstep=2*sqrt(3/8)*dx/max(vel(:));
    dtstep=floor(dtstep/.0001)*.0001; %modelling step size

    tmax=2*zmax/vlow; %maximum time
    [seisfilt,seis,t]=afd_explode(dx,dtstep,-dt,tmax, ...
            vel,x,zeros(size(x)),[10 15 40 50],0,lap);
    %plot the seismogram
    plotimage(seisfilt,t,x);
    %compute times to top and bottom of channel
    tchtop=2*(z1/vlow + (z2-z1)/v1 + (z3-z2)/v2 + (z4-z3)/v3);
    tchbot=tchtop+2*(thk/vch);
    %annotate times
    h1=drawpick(xmax/2,tchtop,0,width);
    h2=drawpick(xmax/2,tchbot,0,width);
    title(['Channel indicated by red lines']) 
elseif(record_type==2)
    disp(' enter spatial grid size in m. A number between 2 and 20 is a good choice')
    dx=input('response -> ');
    if(dx==0) dx=5; end
    xmax=2500;zmax=1000;%grid size, max line length, max depth
    x=0:dx:xmax;z=0:dx:zmax; % x and z coordinate vector
    vhigh=4000;vlow=2500;vrange=vhigh-vlow; % high and low velocities
    vel=vlow*ones(length(z),length(x));%initialize velocity matrix
    z1=100;z2=200;v1=vlow+vrange/5;%first layer
    xpoly=[-dx xmax+dx xmax+dx -dx];zpoly=[z1 z1 z2 z2];
    vel=afd_vmodel(dx,vel,v1,xpoly,zpoly);%install first layer
    z3=271;v2=vlow+2*vrange/5;zpoly=[z2 z2 z3 z3];%second layer
    vel=afd_vmodel(dx,vel,v2,xpoly,zpoly);%install second layer
    z4=398;v3=vlow+pi*vrange/5;zpoly=[z3 z3 z4 z4];%third layer
    vel=afd_vmodel(dx,vel,v3,xpoly,zpoly);%install third layer
    zpoly=[z4 z4 zmax+dx zmax+dx];%last layer
    vel=afd_vmodel(dx,vel,vhigh,xpoly,zpoly);%install last layer
    thk=50;vch=vlow+vrange/6;%channel
    xpoly=[xmax/2-width/2 xmax/2+width/2 xmax/2+width/2 xmax/2-width/2];
    zpoly=[z4 z4 z4+thk z4+thk];
    vel=afd_vmodel(dx,vel,vch,xpoly,zpoly);%install channel
    plotimage(vel-.5*(vhigh+vlow),z,x);%plot the velocity model

    %do a finite-difference exploading reflector model
    dt=.004; %temporal sample rate
    lap=2;%laplacian flag
    dtstep=sqrt(3/8)*dx/max(vel(:));
    dtstep=floor(dtstep/.0001)*.0001; %modelling step size

    tmax=2*zmax/vlow; %maximum time
    xrec=x;
    zrec=zeros(size(x));
    snap1=zeros(size(vel));
    snap2=snap1;
    snap2(1,round(length(x)/2))=1;
    [seisfilt,seis,t]=afd_shotrec(dx,dtstep,-dt,tmax,vel,snap1,snap2,...
        xrec,zrec,[10 15 40 50],0,lap);
    %plot the seismogram
    plotimage(seisfilt,t,xrec);
    %compute times to top and bottom of channel
    tchtop=2*(z1/vlow + (z2-z1)/v1 + (z3-z2)/v2 + (z4-z3)/v3);
    tchbot=tchtop+2*(thk/vch);
    %annotate times
    h1=drawpick(xmax/2,tchtop,0,width);
    h2=drawpick(xmax/2,tchbot,0,width);
    title(['Channel indicated by red lines']) 

else
    disp('invalid record type')
end

disp(' enter channel width in meters (between 20 and 1000, <cr> to end)')
width=input(' response -> ')

end

%a shot record left of the channel
%snap1=zeros(size(vel));
%snap2=snap1;
%snap2(1,75)=1;
%[shotf,shot,tshot]=afd_shotrec(dx,dtstep,-dt,tmax,...
%	vel,snap1,snap2,x,zeros(size(x)),[10 15 40 50],0,1);

%plot the seismogram
%plotimage(shotf,tshot,x)


%a shot record over the channel
%snap1=zeros(size(vel));
%snap2=snap1;
%snap2(1,125)=1;
%[shotf2,shot2,tshot]=afd_shotrec(dx,dtstep,-dt,tmax,...
%	vel,snap1,snap2,x,zeros(size(x)),[10 15 40 50],0,1);


%plot the seismogram
%plotimage(shotf2,tshot,x)