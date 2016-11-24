function [shotmigdec,shotmigcc,illumination]=pspi_shot_cwavez(shot,t,x,velp,vels,xv,zv,xshot,zshot,zt,frange,stab)
% PSPI_SHOT_CWAVEZ: shot record migration by the PSPI algorithm for PS
% events, including surface topography.
%
% [shotmigdec,shotmigcc,illumination]=pspi_shot_cwavez(shot,t,x,velp,vels,x
% v,zv,xshot,zshot,zt,frange,stab);
%
% PSPI_SHOT_CWAVEZ performs 2D depth migration for PS events on a single shot
% record. This version (as compared with PSPI_SHOT_CWAVE) is able to migrate
% from topography. Required inputs are the shot record, which must be regularly
% sampled in both x and t,  both p-wave and s-wave instantaneous velocity 
% models, and the topographic elevations of sources and receivers. 
% The velocity models are depth matrices and the migrated shot will 
% have the same dimensions as either velocity model. Two migrated shot records 
% are returned corresponding to a cross-correlation imaging condition and a 
% deconvolution imaging condition. If the former is to be used, then the 
% shot record should either be gained before migration or after (see GAINCC). 
% The technical content of this code is adapted from PSPI_SHOT.
%
% Please see 'Ferguson_Margrave_2005.pdf' or 'Ferguson and Margrave, 2005,
% Planned seismic imaging using explicit one-way operators, Geophysics, V70,
% NO5, S101 - S109' for details.
%
% shot ... shot record stored as a matrix, one trace per column
% t ... time coordinate vector for shot
% x ... x cooreinate vector for shot (x should be regularly sampled).
% NOTE: size(shot) must equal [length(t), length(x)]
% velp...p-wave velocity model. A matrix in consistent units.
% vels...s-wave velocity model. A matrix in consistent units.
% xv ... x (column) coordinate vector for velocity models
% zv ... z (row) coordinate vector for velocity models
% NOTE: size(velp) must equal [length(zv), length(xv)]
% NOTE: The velocity model depth coodinate defines the depth step: dz=zv(2)-zv(1)
%      Hence the depth coordinate of the migrated shot will be zv. The x
%      coordinates of the shot record must be contained within the x
%      coordinate span of the velocity model. If the shot does not span the
%      entire velocity model then the shot will be automatically padded
%      with zero traces. If a larger xpad is needed, then do it before migration. 
%      Traces are automatically padded in time to minimize operator
%      wrap-around and the pad is re-zero'd every 10 steps.
% xshot ... x coordinate of shot (a scalar).
%
% zshot ... Source elevation. (Datum is the Zero depth in the velocity model)
% zt    ... Vector with topography for each receiver.
%
% frange... two element vector giving the frequency range (min and max) to
% be migrated. Example, migrate all frequencies up to 60 Hz: frange=[0 60];
%  ****** default is all frequencies ******
% NOTE: runtime is linearly proportional to the number of frequencies to be
% migrated. There is nothing to be gained by migrating noise or extremely
% low amplitude frequencies.
% stab ... stability factor used in stabilized deconvolution imaging
%   condition. Must be contained in [0,1] .
%  ****** default is .0001 ******
%
% shotmigdec...depth migrated output using a stabilized deconvolution imaging condition
% shotmigcc...depth migrated output using crosscorrelation imaging contition
% illumination ... illum or shot strength at each image point: 
% NOTE: shotmigdec=shotmigcc./(illumination+stab*max(illumination(:)))
% NOTE: Both shotmigcc and shotmigdec are the same size as the input velocity
%   model.
%
% G.F. Margrave & R.J. Ferguson 2011 (adapted from PSPI_SHOT)
%
% Topography option "Zero velocity layer"  by Sam Gray, 2011
% Theory in Al-Saleh, Margrave and Gray, 2009. 
% Base code name: pspi_shot_cwave
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

%***get sizes of things***
[Nz,Nx]=size(velp);
dz=zv(2)-zv(1);
[nt,nx]=size(shot);
dt=t(2)-t(1);
%*************************
%test shot dimensions
if(length(x)~=nx)
    error('shot x axis incorrect');
end
if(length(t)~=nt)
    error('shot t axis incorrect');
end
%test velp dimensions
if(length(zv)~=Nz)
    error('velocity z axis incorrect');
end
if(length(xv)~=Nx)
    error('velocity z axis incorrect');
end
if(size(vels,1)~=Nz | size(vels,2)~=Nx)
    error('s wave velocity model must be the same size as p wave');
end
%determine if the velocity model spans the shot
xmins=min(x);xmaxs=max(x);
xminv=min(xv);xmaxv=max(xv);
if(xmins<xminv || xmaxs>xmaxv)
    error(['Shot x coordinates fall outside the span of the velocity model.'...
        ' velocity model must be extended']);
end
dx=xv(2)-xv(1);
small=1e-04;
%test to see if shot and velocity are on same grid
if(abs(dx*floor(xmaxs/dx)-xmaxs)>small)
    error('velocity model and shot record are not on the same x grid');
end
%test for regular shot sampling
if(sum(abs(diff(diff(x))))>small)
    error('Shot record must be regularly sampled in x');
end
%check stab
if(nargin>8)
    if(stab<0 || stab>1)
        error('stab must lie in [0,1]');
    end
else
    stab=.0001;
end
if(nargin<7)
    frange=[0 inf];
end

%now, pad the shot with zero traces if needed
npadmin=0;
npadmax=0;
if(xmins>xminv)
    npadmin=round((xmins-xminv)/dx);
end
if(xmaxs<xmaxv)
    npadmax=round((xmaxv-xmaxs)/dx);
end
if(npadmin+npadmax>0)
    shot=[zeros(nt,npadmin) shot zeros(nt,npadmax)];
end
x=xv;
nx=length(x);
%pad the shot out so that number of traces is a power of 2
nx2=2^nextpow2(nx);

if(nx<nx2)
    shot=[shot zeros(nt,nx2-nx)];%pad shot with zeros
    velp=[velp velp(:,end)*ones(1,nx2-nx)];%extend velp with last trace
    vels=[vels vels(:,end)*ones(1,nx2-nx)];%extend vels with last trace
end
x2=(0:nx2-1)*dx;
        
%determine the temporal pad
%pad by enough to hold 10 dz steps
tmax=max(t);
vmin=min(velp(:));

if vmin<=1,
    inozero=find(velp>1);
    vz=velp(inozero);
    vmin=min(vz);
end

tpad=3*10*dz/vmin;%thrice the vertical travel time for 10 steps at slow velocity
npad=round(tpad/dt);
npow=nextpow2(nt+npad);
ntnew=2^npow;%make the new length a power of 2
npad=ntnew-nt;
%keyboard %pad the traces in time
shot=[shot;zeros(npad,nx2)];
t=dt*(0:ntnew-1);

%fk transform the shot record
[shotfk,f,k]=fktran(shot,t,x2);
shotfk=fftshift(shotfk,2);%pspi_ips wants a wrapped wavenumber spectrum

if(frange(1)==0)
    frange(1)=f(2);%don't use 0 Hz
end
if(frange(2)>f(end))
    frange(2)=f(end);
end

%[nf,cd]=size(shotfk);
nf=length(f);
indf=near(f,frange(1),frange(2));%frequencies to migrate
nf2=length(indf);

%***build the source***
temp=zeros(nf,nx2);

zsoui=floor(zshot/dz)+1;
xsoui=round(xshot/dx);

% Selecting the Horizontal segment below the topography that includes 
%           the source
jzts=find(floor(zt/dz)<zsoui);
ljz=length(jzts); 
prex=jzts(1);posx=0;  % The segment is [prex:posx] 
k=1;
while posx==0,
    k=k+1;
    if (jzts(k)-jzts(k-1))~=1||(k==ljz),
        if jzts(k)<=xsoui,
            prex=jzts(k);
        else
            posx=jzts(k);
        end
    end
end
spansou=[prex:posx];

% Green function calculation for the segment selected
for j=indf
    test=prod(velp(zsoui,spansou));
    if(test==0)
        disp('logic failure calculating source');
    end
    temp(j,spansou)=greenseed2z(1,dx*spansou,xshot,f(j),f(end),velp(zsoui,spansou),dz,1);
end
sourcefk=ifft(temp,[],2);
%**********************
%build the piecewise constant velocity models by the Bagaini method
%the first two input parameters are a mystery but seem to work
velp_blocked=Bagaini(length(x)-1,10,velp);
vels_blocked=Bagaini(length(x)-1,10,vels);

%allocate arrays
shotmigcc=zeros(Nz,nx2);
shotmigdec=zeros(Nz,nx2);
illumination=zeros(Nz,nx2);

time1=clock;
timeused=0.0;
ievery=20;

% Selecting topography depths
zmax=max(zt);zmin=min(zt);
maxzt=ceil(zmax/dz);
minzt=ceil(zmin/dz);

for j=minzt+1:Nz-1
    if((rem(j,ievery)-2)==0)
        disp([' pspi prestack mig working on depth ',num2str(j),' of ',num2str(Nz),...
            ' time left ~ ' int2str(timeremaining) '(s)'])
    end
    if(rem(j,10)==0)
        shotfk=ps_rezero(shotfk,f,dx,tmax);
    end
    if j<=zsoui, % Receiver only extrapolation if higher than the source. 
        zj=dz*j;       
        ftemp=pspi_ipsz(shotfk(indf,:),f(indf),dx,vels(j,:),vels_blocked(j,:),dz,zj,zt);     
        shotfk(indf,:)=ifft(ftemp,[],2);
        if((rem(j,ievery)-2)==0)
            disp([' Rough topography '])
        end
    else
        if j<maxzt,        % Case rough topography
            zj=dz*j;
            ftemp=pspi_ipsz(shotfk(indf,:),f(indf),dx,vels(j,:),vels_blocked(j,:),dz,zj,zt);
            stemp=pspi_ipsz(sourcefk(indf,:),f(indf),dx,velp(j,:),velp_blocked(j,:),-dz,zj,zt);
            if((rem(j,ievery)-2)==0)
             disp([' Rough topography '])
            end
        elseif j>maxzt,  % Case standard: flat topography.
            %step the data down
            ftemp=pspi_ips(shotfk(indf,:),f(indf),dx,vels(j,:),vels_blocked(j,:),dz);
            %step the source model down
            if(j==1)
                stemp=pspi_ips(sourcefk(indf,:),f(indf),dx,velp(j,:),velp_blocked(j,:),0);
            else
                stemp=pspi_ips(sourcefk(indf,:),f(indf),dx,velp(j,:),velp_blocked(j,:),-dz);
            end
            %ftemp and stemp are in the (x,f) domain.  
        end
        %imaging conditions
        rcc=ftemp.*conj(stemp);%trivial reflectivity estimate
        illum=stemp.*conj(stemp);%illumination is the shot power
        rdec=rcc./(illum+stab*max(abs(illum(:))));%stabilized decon reflectivity estimate
        %At this point, rcc and rdec are frequency and wavenumber dependent
        %Sum rcc and rdec over temporal frequencyn to get the final estimates
        shotmigcc(j+1,:)=real(sum(rcc)+sum(rcc(1:nf2-1,:)))/(2*nf2-1)/2/pi;
        shotmigdec(j+1,:)=real(sum(rdec)+sum(rdec(1:nf2-1,:)))/(2*nf2-1)/2/pi;
        illumination(j+1,:)=real(sum(illum)+sum(illum(1:nf2-1,:)))/(2*nf2-1);
        %transform back to (k,f) domain
        shotfk(indf,:)=ifft(ftemp,[],2);
        sourcefk(indf,:)=ifft(stemp,[],2);
    end
    timenow=clock;
    timeused=etime(timenow,time1)+timeused;
    time1=timenow;
    timeremaining=(Nz-1)*timeused/j-timeused;
    %disp([' elapsed time ',int2str(timeused),' (s), estimated time remaining '...
    %    ,int2str(timeremaining),' (s)']);
end
shotmigcc=shotmigcc(:,1:nx);
shotmigdec=shotmigdec(:,1:nx);
illumination=illumination(:,1:nx);
disp(['shot migrated in ' int2str(timeused) '(s)'])