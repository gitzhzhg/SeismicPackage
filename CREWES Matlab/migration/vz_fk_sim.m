% VZ_FK_SIM: script to demo vz_fkmig
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
vzflag=3; %set this flag to 0 for stolt (v=const) theory, 1 gets vz_fk, 2 gets vz_fkmig_mixed,
% % 3 gets phase shift
wkbjflag=0; %set this flag to 1 for wkbj v(z) algorithm, 0 gets rms
velflag=1; %set to 0 for a constant velocity. Must be zero for stolt, 1 gets linear v(z)

dt=.004;
nt=512;
% t=xcoord(0,dt,nt)';
t=dt*(0:nt-1)';
fnyq=1./(2*dt);
vconst=2000;

if( velflag==0)
    %constant velocity case
	v=vconst*ones(size(t));% constant erm velocity
	vrms=v;
	vins=v;
	vave=v;
elseif(velflag==1)

%variable velocity case
% instantaneous velocity linear with depth
%determine vo such that the middle value of vrms is vconst
	if(vzflag==0) error('stolt must have constant velocity'); end
	c=.6;
	vo=vconst/sqrt((exp(2*c*t(nt/2))-1)./(2*c*t(nt/2)));
    vrms=zeros(nt,1);
	vrms(2:nt)=vo*sqrt((exp(2*c*t(2:nt))-1)./(2*c*t(2:nt)));
	vrms(1)=vo;
	vrms=vrms(:); %force column vector
	 
	vins=vrms2vint(vrms,t);%compute local instantaneous velocities
	vave=vint2vave(vins,t);
else
	error('invalid velflag');
end


z=vave.*t;

%make a synthetic section with a single live  trace
%arrange to get a complete and unaliased semi-circle at the bottom
fmax = .5*fnyq;
%diam=vrms(nt)*t(nt);
dx = vrms(1)/(4*fmax);
ntr=512; %power of two traces
x=dx*(0:ntr-1);

tmax=t(nt);
tint=.15; %spikes every tint
tspike= tint:tint:tmax;
ntspike= round(tspike/dt+1);

seis= zeros(nt,ntr);
seis(ntspike,ntr/2)=ones(size(ntspike))';
xo=x(ntr/2);

wlet=ormsby(5,10,fmax,1.1*fmax,2*tint,dt);
seis(:,ntr/2)= convz(seis(:,ntr/2),wlet);
%plotimage(seis,t,x);

%migrate
params=nan*ones(1,13);
params(5:6)=[.5 0];
params(12:13)=[0 1];
params(3)=90;
%beginning migration
if(vzflag==1)
	%choose wkbj or rms
	if(wkbjflag~=1)
		params(9)=1;
		params(13)=20;
		seismig=vz_fkmig(seis,t,x,vrms,params);
		plotimage(seismig,t,x);
		title('v(z) fkmigration - RMS algorithm - Full Fourier');

	else
		params(9)=2;
		params(10)=.5;
		params(13)=20;
		seismig=vz_fkmig(seis,t,x,vins,params);
		plotimage(seismig,t,x);
		title('v(z) fkmigration - WKBJ algorithm - Full Fourier');
		
	end

	%plotimage(seismig,t,x);
	%title('v(z) fkmigration');
elseif(vzflag==2)
	%choose wkbj or rms
	if(wkbjflag~=1)
		params(9)=1;
		params(1)=nan;
		params(13)=20;
		seismig=vz_fkmig(seis,t,x,vrms,params);
		plotimage(seismig,t,x);
		title('v(z) fkmigration - RMS algorithm - Mixed domain');
		
	else
		params(1)=nan;
		params(9)=2;
		params(10)=.5;
		params(13)=20;
		seismig=vz_fkmig(seis,t,x,vins,params);
		plotimage(seismig,t,x);
		title('v(z) fkmigration - WKBJ algorithm - Mixed domain');
		
	end

	%plotimage(seismig,t,x);
	%title('v(z) fkmigration');
elseif(vzflag==0)
	params(13)=50;
	seismig=fkmig(seis,t,x,vconst,params);
	plotimage(seismig,t,x);
	title('constant velocity Stolt Result');
elseif(vzflag==3)
	disp('phase shift')
	psparms=nan*ones(1,4);
	psparms(1:2)=params(5:6);
	psparms(3)=20;
	psparms(4)=params(3);
	seismig=ps_migt(seis,t,x,vins,psparms);
%     frange=[2 80];
%     seismig=pspi_stack_tmig(seis,t,x,vrms,x(1),t,frange);
	plotimage(seismig,t,x);
	title('Phase shift');
else
	error('invalid vzflag');
end

%create wavefront circles
%ncirc=length(tspike);
%tx = zeros(ncirc,ntr);
%for k=1:ncirc;
% tx(k,:) = sqrt(tspike(k)^2 - (2*(x-xo)/vrms(ntspike(k))).^2);
% ind=find(imag(tx(k,:))~=0);
% if(~isempty(ind)) tx(k,ind)=nan*ind; end
% line(x,tx(k,:),ones(size(x)),'color','r','linewidth',1.5);
%end
%txconst= zeros(size(tx));
%for k=1:ncirc;
% txconst(k,:) = sqrt(tspike(k)^2 - (2*(x-xo)/vconst).^2);
% ind=find(imag(txconst(k,:))~=0);
% if(~isempty(ind)) txconst(k,ind)=nan*ind; end
% line(x,txconst(k,:),ones(size(x)),'color','c','linewidth',1.5);
%end