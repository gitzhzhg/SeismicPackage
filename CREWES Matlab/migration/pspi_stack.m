function [zosmig,exzos]=pspi_stack(zos,t,x,vel,xv,zv,frange,zcheck,irezero)
% PSPI_STACK: zos (zero offset section) migration by the PSPI algorithm
%
% [zosmig,exzos]=pspi_stack(zos,t,x,vel,xv,zv,frange,zcheck,irezero)
%
% PSPI_STACK performs 2D depth migration for PP events on a stacked section
% (or zero-offset section [zos]). Required inputs are the zos, which must
% be regularly sampled in both x and t, and a p-wave instantaneous velocity
% model. The velocity model is a depth matrix and the migrated zos will
% have the same dimensions as the velocity model. The wavefield
% extrapolation is done by PSPI_IPS. An option exists to output extrapolated
% sections for a specified set of depths. The extrapolated sections are
% downware-continued zos's and represent an estimate of what would have
% been recorded at the specified depths.
%
% Please see 'Ferguson_Margrave_2005.pdf' or 'Ferguson and Margrave, 2005,
% Planned seismic imaging using explicit one-way operators, Geophysics, V70,
% NO5, S101 - S109' for details.
%
% zos ... zos record stored as a matrix, one trace per column
% t ... time coordinate vector for zos
% x ... x coordinate vector for zos (x should be regularly sampled).
% NOTE: size(zos) must equal [length(t), length(x)]
% NOTE: the x sample interval for data and velocity model should be the
% same. 
% vel...velocity model. A matrix in consistent units.
% xv ... x (column) coordinate vector for velocity model
% zv ... z (row) coordinate vector for velocity model
% NOTE: size(vel) must equal [length(zv), length(xv)]
% NOTE: The velocity model depth coodinate defines the depth step:
%      dz=zv(2)-zv(1). Currently zv must be regularly sampled.
%      Hence the depth coordinate of the migrated zos will be zv. The x
%      coordinates of the zos record must be contained within the x
%      coordinate span of the velocity model. If the zos does not span the
%      entire velocity model then the zos will be automatically padded
%      with zero traces. If a larger xpad is needed, then do it before migration. 
%      Traces are automatically padded in time to minimize operator
%      wrap-around and the pad is re-zero'd every 10 steps.
% frange... two element vector giving the frequency range (min and max) to
% be migrated. Example, migrate all frequencies up to 60 Hz: frange=[0 60];
% (all frequencies can be specified by [0 inf])
%  ****** default is all frequencies ******
% NOTE: runtime is linearly proportional to the number of frequencies to be
% migrated. There is nothing to be gained by migrating noise or extremely
% low amplitude frequencies.
% zcheck... vector of depths at which extrapolated sections are desired
%  ******default is [] *******
% irezero... rezero the temporal pad every this many depth steps. This 
%       guards against a form of temporal wraparound. If the data are rick
%       in low frequencies, then this may need to be set to 1.
%  ****** default = 5 ********
%
% zosmig...depth migrated output 
% Note: zosmig will be the same size as the velocity model so it can be
%       plotted using xv and zv: plotimage(zosmig,zv,xv)
% exzos ...cell array of extrapolated sections, one for each zcheck.  These
%       are downward continued zos's that simulate what would have been
%       recorded at each depth in zcheck.
% Note: Each extrapolated section will be a time section that is larger
%       than the input zos because of the migration pad that is
%       automatically padded. To plot the first entry in exzos use
%       something like:
%   tzos=dt*(0:size(exzos{1},1)-1);% note the curly brackets around the 1
%   xzos=dx*(0:size(exzos{1},2)-1);
%   plotimage(exzos{1},tzos,xzos)
% Better yet, load them into plotgathers with something like
%   titles=cell(size(exzos));
%   for k=1:length(exzos)
%       titles{k}=['Extrapolated zos for ' int2str(zcheck(k)) ' m'];
%   end
%   plotgathers(exzos,tzos,xzos,'distance (m)','time (s)',titles);
%
% R. J. Ferguson, 2009
% G.F. Margrave 2013
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
[Nz,Nx]=size(vel);
dz=zv(2)-zv(1);
[nt,nx]=size(zos);
dt=t(2)-t(1);
small=1e-04;
dx=xv(2)-xv(1);
dxs=x(2)-x(1);
%*************************
%test zos dimensions
if(length(x)~=nx)
    error('zos x axis incorrect');
end
if(length(t)~=nt)
    error('zos t axis incorrect');
end
%test vel dimensions
if(length(zv)~=Nz)
    error('velocity z axis incorrect');
end
if(length(xv)~=Nx)
    error('velocity x axis incorrect');
end
if(abs(dxs-dx)>small)
    error('velocity model and zos must have same x sample size')
end
%determine if the velocity model spans the zos
xmins=min(x);xmaxs=max(x);
xminv=min(xv);xmaxv=max(xv);
if(xmins<xminv || xmaxs>xmaxv)
    error(['zos x coordinates fall outside the span of the velocity model.'...
        ' Velocity model must be extended']);
end


%test to see if zos and velcoty are on same grid
if(abs(dx*floor(xmaxs/dx)-xmaxs)>small)
    error('Velocity model and zos record are not on the same x grid');
end
%test for regular zos sampling
if(sum(abs(diff(diff(x))))>small)
    error('zos record must be regularly sampled in x');
end

%establish frange default
if(nargin<7)
    frange=[0 inf];
end
if(nargin<8)
    zcheck=[];
end
if(nargin<9)
    irezero=5;
end

%exploding reflector model requires dividing velocity by 2
vel=vel/2;

%now, pad the zos with zero traces if needed
npadmin=0;
npadmax=0;
if(xmins>xminv)
    npadmin=round((xmins-xminv)/dx);
end
if(xmaxs<xmaxv)
    npadmax=round((xmaxv-xmaxs)/dx);
end
if(npadmin+npadmax>0)
    zos=[zeros(nt,npadmin) zos zeros(nt,npadmax)];
end
x=xv;
nx=length(x);
%pad the zos out so that number of traces is a power of 2
nx2=2^nextpow2(nx);

if(nx<nx2)
    zos=[zos zeros(nt,nx2-nx)];%pad zos with zeros
    vel=[vel vel(:,end)*ones(1,nx2-nx)];%extend vel with last trace
end
x2=(0:nx2-1)*dx;
        
%determine the temporal pad
%pad by enough to hold 10 dz steps
tmax=max(t);
vmin=min(vel(:));
tpad=2*10*dz/vmin;%twice the vertical travel time for 10 steps at slow velocity
npad=round(tpad/dt);
npow=nextpow2(nt+npad);
ntnew=2^npow;%make the new length a power of 2
npad=ntnew-nt;
%pad the traces in time
zos=[zos;zeros(npad,nx2)];
t=dt*(0:ntnew-1);

%fk transform the zos record
[zosfk,f]=fktran(zos,t,x2);
zosfk=fftshift(zosfk,2);%pspi_ips wants a wrapped wavenumber spectrum

if(frange(1)==0)
    frange(1)=f(2);%don't use 0 Hz
end
if(frange(2)>f(end))
    frange(2)=f(end);
end

indf=near(f,frange(1),frange(2));%frequencies to migrate
nf2=length(indf);

%**********************
%build the piecwise constant velocity model by the Bagaini method
%the first two input parameters are a mystery but seem to work
vel_blocked=Bagaini(length(x)-1,10,vel);

%allocate array
zosmig=zeros(Nz,nx2);
%delete any zchecks beyond max(zv)
ind= zcheck<max(zv);
zcheck=zcheck(ind);

time1=clock;
timeused=0.0;

jcheck=-1;
if(~isempty(zcheck))
    jcheck=round(zcheck(1)/dz)+1;
    exzos=cell(1,length(zcheck));
    ncheck=0;
end
for j=1:Nz-1
    if((rem(j,30))==0)
        disp([' pspi_stack working on depth ',num2str(j),' of ',num2str(Nz),...
            ' time left ~ ' int2str(timeremaining) '(s)'])
    elseif(rem(j,irezero)==0)
        disp([' pspi_stack working on depth ',num2str(j),' of ',num2str(Nz)])
        %re-zero the zero pad
        zosfk=ps_rezero(zosfk,f,dx,tmax);
    end
    %step the data down
	ftemp=pspi_ips(zosfk(indf,:),f(indf),dx,vel(j,:),vel_blocked(j,:),dz);
    %ftemp is in the (x,f) domain.

    %imaging condition
    zosmig(j+1,:)=real(sum(ftemp)+sum(ftemp(1:nf2-1,:)))/(2*nf2-1);
    %transform back to (k,f) domain
	zosfk(indf,:)=ifft(ftemp,[],2);
    %see if we are saving this extrapolated zos
    if(j==jcheck)
        ncheck=ncheck+1;
        exzos{ncheck}=zos2t(zosfk,f,dx);
        if(ncheck<length(zcheck))
            jcheck=round(zcheck(ncheck+1)/dz)+1;
        end
    end
    timenow=clock;
    timeused=etime(timenow,time1)+timeused;
    time1=timenow;
    timeremaining=(Nz-1)*timeused/j-timeused;
    %disp([' elapsed time ',int2str(timeused),' (s), estimated time remaining '...
    %    ,int2str(timeremaining),' (s)']);
end
zosmig=zosmig(:,1:nx);%remove zero pad
disp(['zos migrated in ' int2str(timeused) '(s)'])

function zos_t=zos2t(zos_fk,f,dx)
% convert the zos back to time
%
% zos_t=zos2t(zos_fk,f,dx,tmax)
%
% Designed for use with ips and pspi_mig etc.
%
% zos_fk ... fk spectrum of data with k axis wrapped and positive f's only
% f ... f coordinate vector for fdata
% dx ... spatial sample size for fdata
% zos_t ... fkspectrum of data with the zero pad re-zero'd
% 

%figure out tmax and dt
c=size(zos_fk,2);
df=f(2)-f(1);
% Tmax=1/df; %true maximum time including any zero pad
fmax=f(end);
dt=.008;
fnyq=.5/dt;
while(fnyq<fmax)
    dt=dt/2;
    fnyq=.5/dt;
end
%Tmax=Tmax-dt;%necessary fiddle
%make f and k axes
fnew=0:df:fnyq;
kx=fftshift(1/2/c/dx*(-c:2:c-2));
data=zeros(length(fnew),c);
indf=near(fnew,f(1),f(end));
data(indf,:)=zos_fk;
data(end,:)=0;%real Nyquist
%inverse fk transform
zos_t=ifktran(data,fnew,kx,0,0);