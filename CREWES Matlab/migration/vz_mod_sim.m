% VZ_MOD_SIM: script to demo vz_fkmod
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

clear
velflag=1; %set to 0 for a constant velocity. Must be zero for stolt, 1 gets linear v(z)

dt=.004;
dx=4;
nxold=512;
ntold=512;
x=(0:(nxold-1))*dx;
t=((0:(ntold-1))*dt)';
fdom=40;
xmax=max(x);
tmax=max(t);
%define diffractor locations
xpts=linspace(0,xmax,10);
tpts=[.1 .2 .3 .4 .5 .6 .7 .8 .9 .98]*tmax;

%pad x and t to the next powers of 2
t=padpow2(t,1);
nt=length(t);
t=((0:nt-1)*dt)';
x=padpow2(x,1);
nx=length(x);
x=(0:nx-1)*dx;
tmax=max(t);
xmax=max(x);

fnyq=1./(2*dt);
vconst=2000;

if( velflag==0)
	v=vconst*ones(size(t));% constant erm velocity
	vrms=v;
	vins=v;
	vave=v;
	tit2='Constant velocity';
elseif(velflag==1)

% instantaneous velocity linear with depth
%determine vo such that the middle value of vrms is vconst
	c=.6;
	vo=vconst/sqrt((exp(2*c*t(nt/2))-1)./(2*c*t(nt/2)));
	vrms(2:nt)=vo*sqrt((exp(2*c*t(2:nt))-1)./(2*c*t(2:nt)));
	vrms(1)=vo;
	vrms=vrms(:); %force column vector
	 
	vins=vrms2vint(vrms,t);%compute local instantaneous velocities
	vave=vint2vave(vins,t);
	tit2='linear v(z)';
else
	error('invalid velflag');
end

disp(tit2)


z=vave.*t;

% %make a synthetic section with a single live  trace
% %arrange to get a complete and unaliased semi-circle at the bottom
% fmax = .5*fnyq;
% %diam=vrms(nt)*t(nt);
% dx = vrms(1)/(4*fmax);
% ntr=512; %power of two traces
% x=dx*(0:ntr-1);
% 
% tmax=t(nt);
% tint=.15; %spikes every tint
% tspike= tint:tint:tmax;
% ntspike= round(tspike/dt+1);
% 
% seis= zeros(nt,ntr);
% seis(ntspike,ntr/2)=ones(size(ntspike))';
% xo=x(ntr/2);
% 
% wlet=ormsby(5,10,fmax,1.1*fmax,2*tint,dt);
% seis(:,ntr/2)= convz(seis(:,ntr/2),wlet);

%make a synthetic with a regular grid of impulses

seis=zeros(nt,nx);

fdom=30;
tlen=.1;
[w,tw]=ricker(dt,fdom,tlen);

%points
tmp=zeros(size(t));
ind=round(tpts/dt)+1;
tmp(ind)=ones(size(ind));

tmp=convz(tmp,w);

for k=1:length(xpts)
	ix=round(xpts/dx)+1;
	seis(:,ix(k))=tmp;
end

%plotimage(seis,t,x);

%model
params=nan*ones(1,13);
params(5:6)=[0 0];
params(12:13)=[0 1];
params(3)=90;
%beginning modeling
params(9)=2;
params(10)=.5;
params(13)=20;
seismod=vz_fkmod(seis,vins,t,x,params);
% plotimage(seismod,t,x);
% title(['v(z) fkmod ' tit2]);

% Truncate and migrate
% clear seis;
% seismod=seismod(1:ntold,1:nxold);
% t=((0:ntold-1)*dt)';
% tpad=(nt-ntold)*dt;
% nt=length(t);
% x=(0:nxold-1)*dx;
% xpad=(nx-nxold)*dx;
% nx=length(x);
% plotimage(seismod,t,x);
% title(['Aperture reduced v(z) fkmod ' tit2]);
% params=nan*ones(1,13);
% params(5:6)=[tpad xpad];
% params(12:13)=[0 1];
% params(3)=90;
% params(9)=2;
% params(10)=.5;
% params(13)=20;
% seismig=vz_fkmig(seismod,vins(1:nt),t,x,params);
% plotimage(seismig,t,x);
% title(['v(z) fkmigration ' tit2]);

%don't truncate but still migrate
ntold=nt;
nxold=nx;
seismod=seismod(1:ntold,1:nxold);
t=((0:ntold-1)*dt)';
tpad=(nt-ntold)*dt;
nt=length(t);
x=(0:nxold-1)*dx;
xpad=(nx-nxold)*dx;
nx=length(x);
plotimage(seismod,t,x);
title(['Not Aperture reduced v(z) fkmod ' tit2]);
params=nan*ones(1,13);
params(5:6)=[tpad xpad];
params(12:13)=[0 1];
params(3)=90;
params(9)=2;
params(10)=.5;
params(13)=20;
seismig=vz_fkmig(seismod,t,x,vins(1:nt),params);
plotimage(seismig,t,x);
title(['v(z) fkmigration ' tit2]);