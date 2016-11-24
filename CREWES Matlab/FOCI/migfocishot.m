function [seismigd,seismigx,zmig,migtime,ttables,seisout,shotout,tout]=migfocishot(seis,x,t,v,xv,dz,xshot,zshot,wlet,tw,fmin,fmax,nfor,ninv,nop,pow,params)
% migfocishot ... depth migration of a single 2D shot record by the FOCI method
%
% [seismigd,seismigx,zmig,migtime,ttables,seisout,shotout,tout]=migfocishot(seis,x,t,v,xv,dz,xshot,zshot,wavelet,tw,fmin,fmax,nfor,ninv,nop,pow,params)
%
% Shot record migration by the FOCI method with spatial resampling to allow shorter
% operator lengths. The algorithm includes a used specified temporal pad
% but applies no spatial (i.e. x) pad. The user must do this extermally.
% Typical pad might be zero trace spans on to the left and right of the
% shot whose size is about 1/2 the shot, thus doubling the shot size.
%
% seis ... input shot record, regularly sampled in x and t
% x ... x coordinates of the columns of seis
% t ... t coordintes of the rows of seis
% v ... velocity matrix. One row for each depth step to be taken. 
% xv ... x coordinates for v
% NOTE: seis and v must be assigned x oordinates with the same sample
%   interval and origin. The velocity matrix must span at least as large an
%   x range as seis. The vertical coordinate of v is assumed to be (k-1)*dz where
%   k is row number.
% dz ... depth step size
% xshot ... x coordinate of the shot (may be an array)
% zshot ... z coordinate of shot (same size array as xshot) Should be < dz for
%           best results. NOT FUNCTIONAL. 
% wlet ... source waveform
% tw ... time coordinate vector for wlet.
% fmin ... minimum frequency (Hz) to migrate
% fmax ... maximum frequency (Hz) to migrate
% nfor ... the forward operator length (in samples)
% ninv ... the inverse operator length (in samples)
% nop ... the final operator length, 0 gets nfor+ninv-1 but no windowing.
%         if nop<nfor+ninv-1 then the operator is designed as nfor+ninv-1
%         in length and then windowed (nicely) to length nop.
% pow ... vector of length 1 or 2 giving the "pow" values (see
%         extrap_design) for the first and possibly second operator 
%         table. The first operator table is invoked most of the time while
%         the second gets invoked as controlled by params(2). This can be
%         used to do evanescent filtering only every so often. Setting
%         pow=[.01 1] will do this. Setting pow equal to a single number
%         causes only a single operator to get built.
% NOTE: The params vector controls a lot of options. To get the default
% behavior, either do not provide params on input, or provide it as a vector
% of nan's. For example, setting params=nan(ones(1,10)) will get the
% default behavior. Setting params=nan*ones(1,10); params(1)=.5; gets
% default behavior in all but the first parameter.
% params(1) ... size in seconds of tpad
%   ********* default max(t) **************
% params(2) ... operator table 2 is invoked every this many steps
%   ********** default 10 ****************
% params(3) ... rezero the temporal pad every this many steps
%         NOT PRESENTLY FUNCTIONAL
%   ********* default 10 **************
% params(4) ... eta, a new frequency chunk is defined whenever the
%             evanescent wavenumber falls below eta*nyquist
%   ********* default .5 ********
% params(5) ... beta, a new frequency chunk is resampled such that its highest
%           evanescent wavenumber is beta*nyquist
%   ********* default .9 *********
% params(6) ... stab, the stability constant for the deconvolution imaging
%           condition
%   ********* default .01 ********
% params(7) ... makeTable, if 0, the operator tables from the previous call
%           to this function (if any) will be used. If 1, then new tables
%           are made regardless. If -1, then the existing tables are used
%           (if any) but cleared at the function termination. NOTE: If
%           existing tables are used, no effort is made to ensure that
%           these tables are consistent with the input parameters. Tables
%           should be remade whenever dx,dz,v,fmin,fmax,nfor,ninv,nop,or pow are
%           changed.
%   ********* default 0 *********
% params(8) ... vcrit. This is the critical velocity that governs spatial
%           resampling. If 0 or nan, then the critical velocity is computed as the
%           minimum value in the velocity model. Programming this ensures
%           that all shot records get the same resampling.
%  ********** default 0 *********
% params(9) ... source modelling options. If 1, then the source is computed
%           as the theoretically correct 2D Greens function with spatial
%           antialias filtering. if 2, then similar to 1 except that there
%           is not antialias filtering. If 3, then the source is a simple
%           impulse (delta function. Only 1 is theoretically correct, the
%           others are errors. This is included to demonstrate these common
%           errors.
%  ********** default 1 *********
% params(10) ... specifies depth at which the forward modelled shot and the
%           downward continued data are desired to be output for analysis. 
%           A value of nan means no additional output is created.
%           If programmed as the negative of the desired depth (e.g. -1000)
%           then the migration is terminated at this depth and not
%           completed. If programmed, then abs(params(10)) must be >= 2*dz
%  ********** default nan **********
%
% seismigd ... migrated shot imaged with the deconvolution imaging
%               condition
% seismigx ... migrated shot imaged with the crosscorrelation imaging
%               condition
% zmig ... depth coordinate for the above two products
% NOTE: The migrated records will be almost the same size and coordinates as v
%       except that the depth coordinate will have one extra sample.
% migtime ... computation time required (excluding time to build tables)
% ttables ... time required to build tables
% seisout ... output downward continued data at the depth specified by
%       params(10)
% shotout ... output forward modelled shot at the depth specified by
%       params(10)
% tout ... time coordinate vector for seisout and shotout (includes pad)
%
% G.F. Margrave, CREWES/POTSI 2004
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

if((min(xv)>min(x))||(max(xv)<max(x)))
    error('velocity model must be at least as large as input shot record')
end

%pad seis to be the same size as v
indx=near(xv,x(1),x(end));
seisp=zeros(length(t),length(xv));
seisp(:,indx)=seis;

%establish some other things
x=xv;
%zv=dz*(0:size(v,1))';
%nt=length(t);
nx=length(x);
dt=t(2)-t(1);
dx=x(2)-x(1);
ipct=0;
dipnom=80;


if(length(params)==8) %early code did not have params(9:10)
    params=[params nan nan];
end

if(~isnan(params(1)))
    tpad=params(1);
else
    tpad=max(t);
end
if(~isnan(params(2)))
    ievery=params(2);
else
    ievery=10;
end
if(~isnan(params(3)))
    izero=params(3);
else
    izero=10;
end
if(~isnan(params(4)))
    eta=params(4);
else
    eta=.5;
end
if(~isnan(params(5)))
    beta=params(5);
else
    beta=.9;
end
if(~isnan(params(6)))
    stab=params(6);
else
    stab=.01;
end
if(~isnan(params(7)))
    makeTable=params(7);
else
    makeTable=0;
end
if(length(params)>7)
    if(~isnan(params(8)))
        vcrit=params(8);
    else
        vcrit=0;
    end
end
if(isnan(params(9)))
    params(9)=1;
end
isource=params(9);
if(isource ~= 1 && isource ~= 2 && isource ~= 2)
    error('params(9) must be either 1, 2, or 3');
end
zcheck=params(10);
if(abs(zcheck)<2*dz)
    error('Invalid value for params(10)');
end
    
small=100*eps;

%transform from t-f
%tmax=t(end);
tmax2=t(end)+tpad;
%nt=length(t);
nt2=round(tmax2/dt)+1;
nt2=2^(nextpow2(nt2));
%t2=(0:nt2-1)*dt;

[seisf,f]=fftrl(seisp,t,0,nt2);
shotf=zeros(size(seisf));

nf=length(f);
f=f(:);
%adjust fmin and fmax to samples
df=f(2)-f(1);
if(fmin==0) fmin=df; end
fmin=df*floor(fmin/df);
fmax=df*ceil(fmax/df);
ifmin=fmin/df+1;
ifmax=fmax/df+1;

if(isource==1)
    disp('Hankel function source with antialias filtering');
elseif(isource==2)
    disp('Hankel function source without antialias filtering');
elseif(isource==3)
    disp('Impulse response of wavefield extrapolator source');
end


%%%%Implement source options
% Seed the Green's function one step down in the shot model
for ishot=1:length(xshot)
    for ifreq=ifmin:ifmax
        if(isource==1)
            shotf(ifreq,:)=greenseed2(1.0,x,xshot(ishot),f(ifreq),fmax,v(1,:),dz);
        elseif(isource==2)
            shotf(ifreq,:)=greenseed2(1.0,x,xshot(ishot),f(ifreq),fmax,v(1,:),dz,0);
        elseif(isource==3)
            ix=near(x,xshot(ishot)); %note, this option needs to be extrapolated on the first step
            shotf(ifreq,ix)=1;
        end
    end
end
%zero traces with offset greater than offmax/2
offset=abs(x-mean(xshot));
offmax=max(offset);
ind=offset>offmax/2;
shotf(:,ind)=0;

% for ishot=1:length(xshot)
%     for ix=1:nx
%         r=sqrt((zshot(ishot)-dz)^2 + (xshot(ishot)-x(ix))^2);
%         shotf(2:end,ix)=shotf(2:end,ix)+sqrt(v(1,ix)*r./(-i*f(2:end)))...
%                                 .*exp(-i*2*pi*f(2:end)*r/v(1,ix))/(4*pi*r);
%         shotf(end,ix)=real(shotf(end,ix));
%     end
% end
%apply wavelet
iz=near(tw,0);
shot=ifftrl(shotf,f);
for ix=1:nx
    shot(:,ix)=convz(shot(:,ix),wlet,iz);
end
shotf=fftrl(shot,t);

shotf=shotf/norm(shotf(:));
seisf=seisf/norm(seisf(:));

tstart0=clock;
%break into frequency chunks. Each chunk gets its own operator tables.

if(~vcrit)
    vcrit=min(v(:));
end

[seischunk,fchunk,xchunk,ichunk]=create_chunks(seisf,f,x,dt,vcrit,eta,beta,fmin,fmax,1);
shotchunk=create_chunks(shotf,f,x,dt,vcrit,eta,beta,fmin,fmax,0);

nchunks=length(seischunk);

%resample velocity for each chunk
for k=1:nchunks
    xr=xchunk{k};
    vchunk{k}=vdesample(v,x,xr(2)-xr(1));  
end

%build operator tables for each chunk
persistent table1chunk table2chunk ktablechunk ntablechunk
if (makeTable==1 || isempty(table1chunk))  
    disp(['Building operator tables (total propagation operators)'])
    for k=1:nchunks
        ftmp=fchunk{k};
        xtmp=xchunk{k};
        dxtmp=xtmp(2)-xtmp(1);
        f1=max([min(ftmp), fmin]);
        f2=min([max(ftmp), fmax]);
        kmin=f1/(max(max(v)));
        kmax=f2/(min(min(v)));
        dk=df/mean(mean(v));
        nopmax=max(nop);
        if(nopmax==0) nopmax=max(nfor)+max(ninv)-1; end
        if(length(pow)==2)
            [table1,ktable,ntable]=focitable_r(dxtmp,dz,kmin,kmax,dk,...
                [nfor nfor],[ninv ninv],[nop nop],ipct,dipnom,pow(1));
            [table2,ktable,ntable]=focitable_r(dxtmp,dz,kmin,kmax,dk,...
                [nfor nfor],[ninv ninv],[nop nop],ipct,dipnom,pow(2));
        else
            [table1,ktable,ntable]=focitable_r(dxtmp,dz,kmin,kmax,dk,...
                [nfor nfor],[ninv ninv],[nop nop],ipct,dipnom,pow);
            table2=table1;
        end
        table1chunk{k}=table1;
        table2chunk{k}=table2;
        ktablechunk{k}=ktable;
        ntablechunk{k}=ntable;
        
    end
    tnow=clock;
	ttables=etime(tnow,tstart0);
	disp(['Tables completed ' num2str(ttables,4) ' seconds'])
else
    disp('Using operator tables from previous call')
    ttables=0;
end


clear seis seisp seisf shot shotf

nsteps=size(v,1)+1; %this allows for the first depth sample to be from the input data
if(~isnan(zcheck))
    if(zcheck<0)
        zcheck=abs(zcheck);
        nsteps=round(zcheck/dz);
        zcheck=nsteps*dz;
    else
        zcheck=round(zcheck/dz)*dz;
    end
end
      
seismigd=zeros(size(v,1)+1,nx);
seismigx=zeros(size(v,1)+1,nx);

%seisf2=assemble_chunks(seischunk,fchunk,xchunk,ichunk,dx,nf,nx);
%[seista,ta]=ifftrl(seisf2,f);

disp('beginning extrapolation ...')
tstart=clock;
%tic
amax=0;
shotout=[];
seisout=[];
for kk=2:nsteps
    vcritchunk=zeros(1,nchunks);
    for kchunk=1:nchunks
        vr=vchunk{kchunk};
        vcritchunk(kchunk)=min(vr(kk-1,:));
        seistmp=seischunk{kchunk};
        seistmp2=zeros(size(seistmp));
        shottmp=shotchunk{kchunk};
        shottmp2=zeros(size(shottmp));
        ktable=ktablechunk{kchunk};
        ntable=ntablechunk{kchunk};
        ftmp=fchunk{kchunk};
        %fmin=min(ftmp);
        %fmax=max(ftmp);
        iusef=between(fmin,fmax,ftmp,2);
        dk=ktable(2)-ktable(1);
        kmin=ktable(1);
        if(rem(kk-2,ievery)==0)%want evanescent filtering on the first step
            table=table2chunk{kchunk};          
        else
            table=table1chunk{kchunk};
        end
        
        xtmp=xchunk{kchunk};
        nxtmp=length(xtmp);
        if(iusef~=0)
            for ix=1:nxtmp
                vx=vr(kk-1,ix);
                k=ftmp(iusef)/vx;
                jk=round((k-kmin)/dk)+1;
                ind=find(jk<1);
                if(~isempty(ind))
                    jk(ind)=1;
                end
                ind=find(jk>length(ntable));
                if(~isempty(ind))
                    jk(ind)=length(ntable);
                end
                nop=size(table,2);
                ixmin=max(1,ix-(nop-1)/2);
                ixmax=min(nxtmp,ix+(nop-1)/2);
                ihmin=max(1,(nop+1)/2-ix+1);
                ihmax=min(nop,(nop+1)/2+(nxtmp-ix));
                if (kk==1 && isource~=3)
                    %don't extrapolate source on first step because it was seeded one dz down
                    shottmp2=shottmp; 
                else
                    shottmp2(iusef,ix)=sum(shottmp(iusef,ixmin:ixmax).*conj(table(jk,ihmin:ihmax)),2);
                end
                seistmp2(iusef,ix)=sum(seistmp(iusef,ixmin:ixmax).*table(jk,ihmin:ihmax),2);                
            end
        end
        seischunk{kchunk}=seistmp2;
        shotchunk{kchunk}=shottmp2;
    end
    seisf=assemble_chunks(seischunk,fchunk,xchunk,ichunk,dx,nf,nx,vcritchunk);  
    shotf=assemble_chunks(shotchunk,fchunk,xchunk,ichunk,dx,nf,nx,vcritchunk); 
    iuse=between(fmin,fmax,f,2);
    tmp=max(abs(shotf(:)))^2;
    if(tmp>amax); amax=tmp; end
    %amax=1;
    if(kk==10)
        disp('yo')
    end
    for ix=1:nx
        seismigx(kk,ix)=(1/pi)*real(sum(seisf(iuse,ix).*conj(shotf(iuse,ix)),1));
        seismigd(kk,ix)=(1/pi)*real(sum((seisf(iuse,ix).*conj(shotf(iuse,ix)))...
            ./(shotf(iuse,ix).*conj(shotf(iuse,ix))+stab*amax),1));
    end
    %test for checkpoint depth
    if(abs(kk*dz-zcheck)<small)
        seisout=ifftrl(seisf,f);
        shotout=ifftrl(shotf,f);
        tout=dt*(0:size(seisout,1)-1)';
    end
    if(rem(kk,10)==0)
        disp(['Completed ' int2str(kk) ' extrapolation steps out of ' int2str(nsteps)])
        tnow=clock;
        tused=etime(tnow,tstart);
        tperstep=tused/(kk-1);
        tneeded=tperstep*(nsteps-1);
        disp(['Time per step ' num2str(tperstep,2) '(s) time remaining ' num2str(tneeded-tused,4)])
    end
end
%seismigx=seismigx/norm(seismigx(:));
%seismigd=seismigd/norm(seismigd(:));
migtime=etime(clock,tstart);
zmig=(0:size(seismigx,1)-1)*dz;
disp(['completed shot migration in ' num2str(migtime,4) ' seconds '] );
if(makeTable==-1)
    clear table1chunk table2chunk ktablechunk ntablechunk
end