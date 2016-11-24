function out=ps_mig(spec,f,kx,vel,z,dip)
%out=ps_mig(spec,f,kx,vel,z,dip);
%
%Stationary phase shift migration: v(z).
%
%  out...migrated output
%  spec...input spectrum
%  f...frequency axis of input spectrum
%  kx...kx axis of input spectrum
%  vel...v(x,z): program averages the x direction and divides
%                by 2 for one way vel - make sure units
%                correspond with t, x, z and f
%  z...depths
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

%  dip...steepest dip to migrate (+tve degrees)
v=mean(vel.').'/2;
%***force into row vectors***
f=f(:);
kx=kx(:)';
z=z(:);
%****************************
%***find sizes of things***
[rsp,csp]=size(spec);
[rf,cf]=size(f);
[rkx,ckx]=size(kx);
[rv,cv]=size(v);
[rz,cz]=size(z);
nz=num2str(rz);
%**************************
%***check dimensions***
if rf~=rsp;error('  f and spec conflict');end;
if ckx~=csp;error('  kx and spec conflict');end;
if rz~=rv;error('  z and vel conflict');end;
%**********************
%***initialize***
out=zeros(rz,ckx);
sym=flipdim(spec(2:rsp-1,:),2);
sym2=sym(:,csp);
sym(:,2:csp)=sym(:,1:csp-1);
sym(:,1)=sym2;
sym=conj(sym);
temp=(sum(spec)+sum(sym))/(2*rsp-2);
temp=fftshift(temp);
out(1,:)=fft(temp,[],2);
tt=0;%total time
%tflops=0;%total flops
%****************
%***migration loop***
%   Symetry of the 2D Fourier transform is exploited to image each depth
%   (the sym stuff below).
for j=2:rz
%    flops(0);
    t0=clock;
  dz=z(j)-z(j-1);
  spec=ps(spec,v(j-1),kx,f,dz,dip);
  sym=flipdim(spec(2:rsp-1,:),2);
  sym2=sym(:,csp);
  sym(:,2:csp)=sym(:,1:csp-1);
  sym(:,1)=sym2;
  sym=conj(sym);
  temp=(sum(spec)+sum(sym))/(2*rsp-2);
  temp=fftshift(temp);
  out(j,:)=fft(temp,[],2);
    et=etime(clock,t0);
    etm=fix(et/60);
    ets=et-60*etm;
    tt=(tt+et);
    ttm=fix(tt/60);
    tts=tt-60*ttm;
    rmt=fix((rz*et-tt)/60);%minutes
    rst=rz*et-tt-60*rmt;%seconds
%    flps=flops;
%    tflops=tflops+flps;
    if(rem(kz,20)==0)
    	ps_stats(nz,j,etm,ets,ttm,tts,rmt,rst);
    end
end
out=real(out);
disp(['total compute time ' num2str(tt)])
%disp(['total flops ' num2str(tflops)])
%********************