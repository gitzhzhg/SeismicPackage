function [seisout,zout,tmig,t_tables]=migfoci(seisin,x,t,v,dz,fmin,fmax,nfor,ninv,nop,pow,params)
% MIGFOCI... post stack depth migration by the FOCI method
% [seisout,zmig]=migfoci(seisin,x,t,v,dz,fmin,fmax,nfor,ninv,nop,pow,params)
%
% Exploding reflector migration by the FOCI method.
%
% seisin ... input time section, regularly sampled in x and t
% x ... x coordinates of the columns of seisin
% t ... t coordintes of the rows of seisin
% v ... velocity matrix, same number of columns as seisin.
%       One row for each depth step to be taken. These should be the real
%       physical velocities. The algorithm divides by two. 
% dz ... depth step size
% fmin ... minimum frequency (Hz) to migrate
% fmax ... maximum frequency (Hz) to migrate
% nfor ... vector of length 2 giving the forward operator lengths at
%          kmin and kmax. (kmin=fmin/vmax , kmax=fmax/vmin)
% ninv ... similar to nfor but for the inverse operator
% nop ... similar to nfor but specifying the windowed length of final op
%         [0 0] gets no windowing
% pow ... vector of length 1 or 2 giving the "pow" values (see
%         extrap_design) for the first and possibly second operator 
%         table. The first operator table is invoked most of the time while
%         the second gets invoked as controlled by params(2). This can be
%         used to do evanescent filtering only every so often. Setting
%         pow=[.01 1] will do this. Setting pow equal to a single number
%         caused only a single operator to get built.
% params(1) ... size in seconds of tpad
%   ********* default max(t) **************
% params(2) ... operator table 2 is invoked every this many steps
%   ********** default 10 ****************
% params(3) ... rezero the temporal pad every this many steps
%   ********* default 10 **************
%
% seisout ... migrated seismic depth section
% zmig ... depth coordinate vector for seisout
%
%
% G.F. Margrave, CREWES Project, U of Calgary, 1996
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


dt=t(2)-t(1);
dx=x(2)-x(1);
ipct=1;
dipnom=80;

v=v/2;

psopt=0; %set to 1 for phase shift
if(psopt)
    disp('PHASE SHIFT (don''t be fooled)')
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

[nz,nx]=size(v);
if(nx~=size(seisin,2))
    error('v and seis must have same number of columns')
end

%transform from t-f
tmax=t(end);
tmax2=t(end)+tpad;
nt=length(t);
nt2=round(tmax2/dt)+1;
nt2=2^(nextpow2(nt2));
t2=(0:nt2-1)*dt;

[seisf,f]=fftrl(seisin,t,0,nt2);
f=f(:);

%adjust fmin and fmax to samples
df=f(2)-f(1);
if(fmin==0) fmin=df; end
fmin=df*(floor(fmin/df)+1);
fmax=df*(ceil(fmax/df)+1);

%build operator table
kmin=fmin/(max(max(v)));
kmax=fmax/(min(min(v)));
dk=(f(2)-f(1))/mean(mean(v));
nopmax=max(nop);
if(nopmax==0) nopmax=max(nfor)+max(ninv)-1; end
disp(['Building ' int2str(round((kmax-kmin)/dk)+1) ' by ' int2str(nopmax) ' operator tables'])
tstart0=clock;
if(length(pow)==2)
	[table1,ktable,ntable]=focitable_r(dx,dz,kmin,kmax,dk,nfor,ninv,nop,ipct,dipnom,pow(1));
	[table2,ktable,ntable]=focitable_r(dx,dz,kmin,kmax,dk,nfor,ninv,nop,ipct,dipnom,pow(2));
else
    [table1,ktable,ntable]=focitable_r(dx,dz,kmin,kmax,dk,nfor,ninv,nop,ipct,dipnom,pow);
    table2=table1;
end
tnow=clock;
t_tables=etime(tnow,tstart0);
disp(['Tables completed ' num2str(t_tables,4) ' seconds'])

nz=size(v,1)+1; %this allows for the first depth sample to be from the input data

seisout=zeros(nz,nx);
seisout(1,:)=seisin(1,:);

disp('beginning extrapolation ...')
tstart=clock;
for kk=2:nz
    if(psopt)
        seisfk=ifft(seisf,[],2);
        kx=fftshift(freqfft(x));
        vee=mean(v(kk-1,:))*(1+ipct*i);
        kz=sqrt(((f/vee).^2)*ones(size(kx))-ones(size(f))*(kx.^2));
        seisf=fft(seisfk.*exp(i*2*pi*dz*real(kz)-abs(2*pi*dz*imag(kz))),[],2);
    else
        if(rem(kk-2,ievery)==0)%want evanescent filtering on the first step
            seisf=extrapfoci(seisf,f,x,v(kk-1,:),table2,ktable,ntable,[fmin fmax nan]);
            %seisf=extrapfoci_f(seisf,f,x,v(kk-1,:),dz,table2,ktable,ntable,[fmin fmax nan]);
            %disp('                        evanescent')
        else
            seisf=extrapfoci(seisf,f,x,v(kk-1,:),table1,ktable,ntable,[fmin fmax nan]);
            %seisf=extrapfoci_f(seisf,f,x,v(kk-1,:),dz,table1,ktable,ntable,[fmin fmax nan]);
            %disp('                        non-evanescent')
        end
    end
    
    if((rem(kk-2,izero)==0)&(tmax<tmax2)&kk~=2) %rezero the zero pad
        [seist, tex]=ifftrl(seisf,f);
        seist(nt+1:nt2,:)=0;
        seisf=fftrl(seist,t2);
    end
        
    
    %image (exploding reflector)
    seisout(kk,:)=real(seisf(1,:)+seisf(end,:))+2*real(sum(seisf(2:end-1,:))); %include neg f's
    
    if(rem(kk,10)==0)
        disp(['Completed ' int2str(kk) ' extrapolation steps out of ' int2str(nz)])
        tnow=clock;
        tused=etime(tnow,tstart);
        tperstep=tused/(kk-1);
        tneeded=tperstep*(nz-1);
        disp(['Time per step ' num2str(tperstep,2) '(s) time remaining ' num2str(tneeded-tused,4)])
    end
end
tnow=clock;
tmig=etime(tnow,tstart);
disp(['Completed migration ' num2str(tmig,4) ' seconds'])
disp(['Plus ' num2str(t_tables) ' seconds for tables'])

zout=dz*(0:nz-1);