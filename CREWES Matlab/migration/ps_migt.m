function [out,tt]=ps_migt(seis,t,x,v,params)
% PS_MIGT: time migration by Gazdag phase shift
%
% [out,cputime]=ps_mig(seis,t,x,v,params);
%
% Stationary phase shift time migration: v(t).
%
%  out...migrated output
%  cputime...computer time required for migration
%  seis...input seismic data
%  t...time axis of input data
%  x...x axis of input data
%  v...v(t): program divides by 2 for one way vel 
%                - make sure units correspond with t, and x  
% params(1) ... tpad in seconds
%     ************ default = 0.0 **********
% params(2) ... xpad in x units
%     ************ default = 0.0 **********
% params(3) ... nprint , print a progress statement every n dt steps
% ********** default = 20 *********
% params(4) ... dip,  steepest dip to migrate (+tve degrees)
% *********** default = 90 ***********
%
% by R.J. Ferguson, CREWES
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

if(nargin<5) params=nan*ones(1,4); end
if(length(params)~=4)
	error(' incorrect number of params')
end
if(isnan(params(1))) tpad=0.0;
else tpad=params(1);
end
if(isnan(params(2))) xpad=0.0;
else xpad=params(2);
end
if(isnan(params(3))) nprint=20;
else nprint=params(3);
end
if(isnan(params(4))) dip=90;
else dip=params(4);
end

tstart=clock;
%flops(0);
tt=0;%total time
%tflops=0;%total flops

v=v/2;

%resolve tpad
dt= t(2)-t(1);
t2 = t(1):dt:t(length(t))+tpad;%requested pad
t3=padpow2(t2,0);%next power of 2
tpad = (length(t3)-length(t))*dt;%actual pad
ntpad=tpad/dt;
disp([' time pad of ' int2str(ntpad) ' samples'])

%resolve xpad
dx= x(2)-x(1);
x2 = x(1):dx:x(length(x))+xpad;%requested pad
x3=padpow2(x2,0);%next power of 2
xpad = (length(x3)-length(x))*dx;%actual pad
nxpad=xpad/dx;
disp([' x pad of ' int2str(nxpad) ' samples'])

%f-k transform
[spec,f,kx]=fktran_mc(seis,t,x,length(t)+ntpad,length(x)+nxpad);
clear seis

%***force into row vectors***
f=f(:);
kx=kx(:)';
t=t(:);
%****************************

%***find sizes of things***
[rsp,csp]=size(spec);
[rf,cf]=size(f);
[rkx,ckx]=size(kx);
[rv,cv]=size(v);
[rt,ct]=size(t);
nt=num2str(rt);
%**************************

%***check dimensions***
if rf~=rsp;error('  f and spec conflict');end;
if ckx~=csp;error('  kx and spec conflict');end;
if rt~=rv;error('  t and vel conflict');end;
%**********************

%***initialize***
out=zeros(rt,ckx);
sym=flipdim(spec(2:rsp-1,:),2);
sym2=sym(:,csp);
sym(:,2:csp)=sym(:,1:csp-1);
sym(:,1)=sym2;
sym=conj(sym);
temp=(sum(spec)+sum(sym))/(2*rsp-2);
temp=fftshift(temp);
out(1,:)=fft(temp,[],2);
et=etime(clock,tstart);
tt=tt+et;%total time so far
%tflops=flops;%total flops so far
%****************

%***migration loop***
%   Symmetry of the 2D Fourier transform is exploited to image each depth
%   (the sym stuff below).
for j=2:rt
    %flops(0);
    t0=clock;
  dt=t(j)-t(j-1);
  spec=pstime(spec,v(j-1),kx,f,dt,dip);
  psimager= exp(i*2*pi*f*t(j));
  sym=flipdim(spec(2:rsp-1,:),2);
  sym2=sym(:,csp);
  sym(:,2:csp)=sym(:,1:csp-1);
  sym(:,1)=sym2;
  sym=conj(sym.*psimager(2:rsp-1,ones(1,size(sym,2))));
  temp=(sum(spec.*psimager(:,ones(1,size(spec,2))))+sum(sym))/(2*rsp-2);
  temp=fftshift(temp);
  out(j,:)=fft(temp,[],2);
    et=etime(clock,t0);
    etm=fix(et/60);
    ets=et-60*etm;
    tt=(tt+et);
    ttm=fix(tt/60);
    tts=tt-60*ttm;
    rmt=fix((rt*et-tt)/60);%minutes
    rst=rt*et-tt-60*rmt;%seconds
    %flps=flops;
    %tflops=tflops+flps;
    if(rem(j-1,nprint)==0)
    	ps_stats(nt,j-1,etm,ets,ttm,tts,rmt,rst);
    end
end
t0=clock;
%flops(0);
%out=ifktran_mc(spec,f,kx);
out=real(out(1:length(t),1:length(x)));% toss the zero pad
et =etime(clock,t0);
tt=tt+et;
%tflops=tflops+flops;
disp(['total compute time ' num2str(tt)])
%disp(['total flops ' num2str(tflops)])
%********************