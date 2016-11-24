function [zosmig,exzos]=pspi_stack_tmig(zos,t,x,vel,xv,tau,frange,taucheck)
% PSPI_STACK_TMIG: zos (zero offset section) TIME migration by the PSPI algorithm
%
% [zosmig,exzos]=pspi_stack_tmig(zos,t,x,vel,xv,tau,frange,taucheck)
%
% PSPI_STACK_TMIG performs 2D time migration for PP events on a stacked
% section (or zero-offset section [zos]). Required inputs are the zos,
% which must be regularly sampled in both x and t, and a p-wave RMS
% velocity model. The velocity model is a time matrix and the migrated zos
% will have the same dimensions as the unmigrated zos. The extrapolation is
% done in tau (migrated time) and the tau step is determined by the time
% axis of the velocity model. If the tau step is larger than the time
% sample rate of the imput zos, then the imaging condition extracts tau
% strips from each extrapolated section and blends them together. The
% wavefield extrapolation is done by PSPI_IPSF. An option exists to output
% extrapolated sections for a specified set of Tau's. 
%
%
% zos ... zos record stored as a matrix, one trace per column
% t ... time coordinate vector for zos
% x ... x coordinate vector for zos (x should be regularly sampled).
% NOTE: size(zos) must equal [length(t), length(x)]
% NOTE: the x sample interval for data and velocity model should be the same. 
% vel...velocity model in time. Velocities should be RMS not interval. 
%       Velocity model must be a either a matrix if lateral variation is
%       desired or a single column vector.  Remember, this is time
%       migration so only very weak lateral gradients will be correctly
%       handled.
% NOTE: For each tau level, extrapolation is always from the surface
% (tau=0) to tau using the rms velocity. This means that the steepest dips
% are incorrect (relative to recursive phase shift) and that there is no
% requirement that the vrms model correspond to meaningful interval
% velocities.
% xv ... x (column) coordinate vector for velocity model
% tau ... time coordinate for the velocity model. 
% NOTE: (tau(2)-tau(1))/(t(2)-t(1) must be an integer.
% NOTE: size(vel) must equal [length(tau), length(xv)]
% NOTE: The velocity model time coodinate defines the tau step:
%      dtau=tau(2)-tau(1) and the maximum tau that is stepped to. 
%      Currently tau must be regularly sampled. When dtau>dt (the usual
%      case, dt=t(2)-t(1)) samples between two tau levels will be
%      interpolated from the extrapolated sections above and below.
% NOTE: The x coordinates of the zos record must be contained within
%      the x coordinate span of the velocity model. If the zos does not
%      span the entire velocity model then the zos will be automatically
%      padded with zero traces. If a larger xpad is needed, then do it
%      before migration. Traces are automatically padded in time to
%      reduce operator wrap-around.
% frange... two element vector giving the frequency range (min and max) to
%      be migrated. Example, migrate all frequencies up to 60 Hz: frange=[0 60];
%     (all frequencies can be specified by [0 inf])
%  ****** default is all frequencies ******
% NOTE: runtime is linearly proportional to the number of frequencies to be
%      migrated. There is nothing to be gained by migrating noise or extremely
%      low amplitude frequencies.
% taucheck... vector of tau values at which time-extrapolated sections are
%       desired. Note, any tauchecks greater than max(tau) will not be output.
%       By default, these time-extrapolated sections are fully migrated
%       from taucheck and up (earlier). However, if the taucheck values are
%       entered as negative numbers, then the taucheck levels are at
%       abs(taucheck) and the sign is a flag to output the
%       tau-extrapoolated section as over extrapolated above taucheck and
%       under extrapollated below (exactly as they are computed
%       internally).
%  ******default is [] *******
%
% zosmig...time migrated output 
% NOTE: zosmig will be the same size as the input zos so it can be
%       plotted using x and t: plotimage(zosmig,t,x)
% exzos ...cell array of extrapolated sections, one for each zcheck.  These
%       are downward continued zos's that simulate what would have been
%       recorded at each depth in zcheck.
% NOTE: Each extrapolated section will be a time section that is larger
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
% G.F. Margrave 2014
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
[Ntv,Nxv]=size(vel);
dtau=tau(2)-tau(1);
[nt,nx]=size(zos);
dt=t(2)-t(1);
small=1e-04;
dxs=x(2)-x(1);
%allow for input of a single vrms versus time function
if(Nxv==1 || Ntv==1)%allow for either as a row vector may be input
    vel=vel(:);%force a column
    vel=vel*ones(1,nx);%replicate across x
    [Ntv,Nxv]=size(vel);
    xv=x;
end

dx=xv(2)-xv(1);    
%*************************
%test zos dimensions
if(length(x)~=nx)
    error('zos x axis incorrect');
end
if(length(t)~=nt)
    error('zos t axis incorrect');
end
%test vel dimensions
if(length(tau)~=Ntv)
    error('velocity tau axis incorrect');
end
if(length(xv)~=Nxv)
    error('velocity x axis incorrect');
end
if(abs(dxs-dx)>small)
    error('velocity model and zos must have same x sample size')
end
%test dtau and dt for compatibility
if(rem(dtau,dt)~=0)
    error('dtau must be an integer multiple of dt')
end
%determine if the velocity model spans the zos
xmins=min(x);xmaxs=max(x);
xminv=min(xv);xmaxv=max(xv);
if(xmins<xminv || xmaxs>xmaxv)
    error(['zos x coordinates fall outside the span of the velocity model.'...
        ' Velocity model must be extended']);
end


%test to see if zos and velocity are on same grid
if(abs(dx*floor((xmaxs-xmins)/dx)-(xmaxs-xmins))>small)
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
    taucheck=[];
end
tauflags=sign(taucheck);
taucheck=abs(taucheck);

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
% tmax=max(t);
% vmin=min(vel(:));
tpad=100*dtau;%twice the vertical travel time for 10 steps at slow velocity
npad=round(tpad/dt);
npow=nextpow2(nt+npad);
ntnew=2^npow;%make the new length a power of 2
npad=ntnew-nt;
%pad the traces in time
zos=[zos;zeros(npad,nx2)];
t=dt*(0:ntnew-1);

%fk transform the input zos
[zosfk,f]=fktran(zos,t,x2);
zosfk=fftshift(zosfk,2);%pspi_ips wants a wrapped wavenumber spectrum

if(frange(1)==0)
    frange(1)=f(2);%don't use 0 Hz
end
if(frange(2)>f(end))
    frange(2)=f(end);
end

indf=near(f,frange(1),frange(2));%frequencies to migrate
% nf2=length(indf);

%**********************
%build the piecwise constant velocity model by the Bagaini method
%the first two input parameters are a mystery but seem to work
vel_blocked=Bagaini(length(x)-1,10,vel);

%allocate output array
zosmig=zeros(ntnew,nx2);

%delete any tauchecks beyond max(tau)
ind= taucheck<=max(tau);
taucheck=taucheck(ind);
tauflags=tauflags(ind);

time1=clock;
timeused=0.0;

jcheck=-1;
if(~isempty(taucheck))
    jcheck=round(taucheck(1)/dtau);%no plus 1 here because the first step goes to level dtau
    exzos=cell(1,length(taucheck));
    ncheck=0;
else
    exzos=[];
end
dtaudt=dtau/dt;%an integer
istrip=(-dtaudt:dtaudt)';%width of a full image strip, an odd number of samples
tmp=linspace(0,1,dtaudt+1);
%
% at the imaging condition, a strip of samples of time width 2*dtaudt is
% extracted from the current extrapolation.
weights=[tmp';fliplr(tmp(1:dtaudt))']*ones(1,nx2);%these are to be applied in the imaging condition
zosmfx=zeros(size(zosfk));%needed in imaging

if(jcheck==0)
    ncheck=ncheck+1;
    exzos{ncheck}=zos;
    jcheck=round(taucheck(2)/dtau);
end

for j=1:Ntv-1
%for j=20:35
    dtaustep=j*dtau;%we step from the surface each time so the step gets larger
    if((rem(j,30))==0)
        disp([' pspi_stack_tmig working on tau level ',num2str(j),' of ',num2str(Ntv),...
            ' time left ~ ' int2str(timeremaining) '(s)'])
    elseif(rem(j,10)==0)
        disp([' pspi_stack_tmig working on tau level ',num2str(j),' of ',num2str(Ntv)])
    end
%     if(rem(j,10)==0)
%         zosfk=ps_rezero(zosfk,f,dx,tmax);
%     end
    %step the data down
    jvel=near(tau,dtaustep);
	zosmfx(indf,:)=pspi_ipsf(zosfk(indf,:),f(indf),dx,vel(jvel,:),vel_blocked(jvel,:),dtaustep.*mean(vel(j,:)));
    %zosmfk is in the (x,f) domain and frequencies indicated by indf are nonzero

    %imaging
    %first inverse transform over time
    ttemp=ifftrl(zosmfx,f);
    %grab a strip of width 2*dtau from the current extrapolation
    zosmig((j+1)*dtaudt+istrip,:)=zosmig((j+1)*dtaudt+istrip,:)+ttemp((j+1)*dtaudt+istrip,:).*weights;
    %transform back to (k,f) domain
% 	zosfk(indf,:)=ifft(ftemp,[],2);
    %see if we are saving this extrapolated zos
    if(j==jcheck)
        ncheck=ncheck+1;
        if(tauflags(ncheck)==-1)
            exzos{ncheck}=ttemp;
        else
            exzos{ncheck}=[zosmig(1:(j+1)*dtaudt+istrip(end),:);ttemp((j+1)*dtaudt+istrip(end)+1:end,:)];
        end
        if(ncheck<length(taucheck))
            jcheck=round(taucheck(ncheck+1)/dtau);
        end
    end
    timenow=clock;
    timeused=etime(timenow,time1)+timeused;
    time1=timenow;
    timeremaining=(Ntv-1)*timeused/j-timeused;
    %disp([' elapsed time ',int2str(timeused),' (s), estimated time remaining '...
    %    ,int2str(timeremaining),' (s)']);
end
zosmig=zosmig(1:nt,1:nx);%remove zero pad
disp(['zos migrated in ' int2str(timeused) '(s)'])

% function zos_t=zos2t(zos_fk,f,dx)
% % convert the zos back to time
% %
% % zos_t=zos2t(zos_fk,f,dx,tmax)
% %
% % Designed for use with ips and pspi_mig etc.
% %
% % zos_fk ... fk spectrum of data with k axis wrapped and positive f's only
% % f ... f coordinate vector for fdata
% % dx ... spatial sample size for fdata
% % zos_t ... fkspectrum of data with the zero pad re-zero'd
% % 
% 
% %figure out tmax and dt
% c=size(zos_fk,2);
% df=f(2)-f(1);
% % Tmax=1/df; %true maximum time including any zero pad
% fmax=f(end);
% dt=.008;
% fnyq=.5/dt;
% while(fnyq<fmax)
%     dt=dt/2;
%     fnyq=.5/dt;
% end
% %Tmax=Tmax-dt;%necessary fiddle
% %make f and k axes
% fnew=0:df:fnyq;
% kx=fftshift(1/2/c/dx*(-c:2:c-2));
% data=zeros(length(fnew),c);
% indf=near(fnew,f(1),f(end));
% data(indf,:)=zos_fk;
% %inverse fk transform
% zos_t=ifktran(data,fnew,kx,0,0);
% 