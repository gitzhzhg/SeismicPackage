function sint=interpbl(t,s,tint,n,m)
%INTERPBL ... bandlimited sinc function interpolation
%
% sint=interpbl2(t,s,tint,n,m)
%
% The sampling theorem gives the reconstruction formula to recover the
% continuous signal from its samples provided that the continuous signal is
% bandlimited (i.e. has compact support in the frequency domain). This
% formula is sint(k)=sum(sinc.*s) where sinc is a properly positioned sinc
% function (it must be shifted to have its maximum at the time of the
% interpoolated sample), s are the samples, and sint is the interpolated
% value. The sum is theoretically over an infinite set of samples and the
% sample weights from from the sinc function. To make this practical, the
% sinc function must be truncated so that the sum becomes finite. The sinc
% should also be smoothly tapered. Here, the half-length of the sinc
% function is specified by the input argument 'n' meaning the sinc extends
% for n points on either side of the maximum. So, if n=4, we say we are
% doing an 8 point interpolation. The truncated sinc is also windowed by a
% Gaussian whose standard deviation is n/2 so that the Gaussian is two
% standard deviations down at the truncation points.
%
% t ... vector of times of the input samples. t must be regularly spaced.
% s ... input samples from the bandlimited signal(s). This can be one trace
%       or a gather of traces. If the latter, then the interpolation sites
%       must be the same on all traces in the gather.
% *** the length of t must match one dimension of s. Preferably the row
% *** dimensions of both are the same.
% tint ... vector of times at which the interpolated values are desired
%       (the interpolation sites). tint does not need to be regular
%       although it can be.
% n ... half-length of the sinc function. n=4 gives similar performance to
%       a spline while n=8 is better.
%   ************ default n=8 *********
% m ... Gaussian window will be m styandard deviations down at the sinc
%       truncation point
%   ************ default m=2 *********
%
% sint ... output array of interpolated samples. There will be one column
% per input trace and the number of rows will match the length of tint.
%
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

%check for nan's in the interpolation times
ind=isnan(tint);
tint_original=0;
if(any(ind))
    inotnan=~isnan(tint);
    tint_original=tint;
    tint=tint(inotnan);
end

if(nargin<4)
    n=8;
end
if(nargin<5)
    m=2;
end
if(sum(abs(diff(diff(t))))>10000*eps)
    error('input times must fall on a regular grid')
end

if(t(1)~=0)
    t1=t(1);
    t=t-t1;
    tint=tint-t1;
end

tmin=min(t);
tmax=max(t);

dt=t(2)-t(1);
nt=length(t);
[nr,nc]=size(s);
if(nr==nt)
    %the preferred case
    s2=[zeros(n,nc); s; zeros(n,nc)];%pad with zeros
    ntr=nc;
elseif(nc==nt)
    s2=[zeros(n,nr); s'; zeros(n,nr)];%pad with zeros
    ntr=nr;
else
    error('input variables t and s have incompatible sizes');
end

t2=(0:size(s2,1)-1)*dt-n*dt;
inc=50;
dt2=dt/inc;
tsinc=(-n*dt:dt2:n*dt)';
one_over_sigma=m/tsinc(end);%inverse of standard deviation of gaussian taper   
sink=sinc(tsinc/dt).*exp(-(one_over_sigma*tsinc).^2);%the sinc function table
nint=length(tint);
sint=zeros(nint,ntr);
small=100*eps;
for k=1:nint
    if(tint(k)<tmin || tint(k)>tmax)
        sint(k,:)=zeros(1,ntr);
    else
        kint=(tint(k)-t2(1))/dt+1;%fractional sample number (in s2) of the interpolation site
        if(abs(kint-round(kint))<small)
            %means the interpolation site is on an input sample
            sint(k,:)=s2(round(kint),:);
        else
            kl=floor(kint)+(-n+1:1:0);%n points to the left of tint(k)
            klsinc=(kl-1)*inc+1-round(tint(k)/dt2);%corresponding point in sink
            kr=ceil(kint)+(0:1:n-1);%n points to the right of tint(k)
            krsinc=(kr-1)*inc+1-round(tint(k)/dt2);%corresponding points in sink
            op=sink([klsinc krsinc]);%table lookup
            sint(k,:)=sum(op(:,ones(1,ntr)).*s2([kl kr],:),1);%actual interpolation
        end
    end
end
if(length(tint_original)>1)
    sint2=zeros(size(tint_original));
    sint2(inotnan)=sint;
    sint=sint2;
end