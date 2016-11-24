function prr=tvprr(r,rp,t,twin,tinc)
% TVPRR: time-variant measurement of portion refelctivity resolved (PRR) (used to assess deconvolution)
%
% prr=tvprr(r,rp,t,twin,tinc)
% 
% PRR, or portion reflectivity resolved, is a measurment of the quality of a deconvolved
% seismic trace compared to a real reflectivity. Usually the deconvolved trace, rp, is created
% by deconvolving an estimated wavelet from a seismic trace, while the real reflectivity, r, is
% computed from well logs. PRR is defined as prr=1-sum((r-rp).^2)/sum(r.^2)). The term being
% subtracted from 1 is the energy of the residual reflectivity (r-srp) divided by the energy of
% the true reflectivity. To make this process time-variant, a Gaussian window is defined, with
% a given width and center time, and applied to both r and rp befor the calculation. Repeating
% this for a complete set of window locations spaiing the length of the trace completes the
% process. Prior to the start of the calculation, rp is scaled to minimize the sum((r-rp)) (as
% performed on the entire trace - no windowing). This allows for the likely case that rp is off
% by a constant scalar.
%
% r ... input real reflectivity
% rp ... input estimated reflectivity
% t ... time coordinate for both r and rp
% NOTE: r, rp, and t must all be identically sized column vectors. This usually means that rp
% must be shortened before calling this function.
% twin ... half-width of Gaussian window in seconds.
% *********** default (t(end)-t(1))/10 **********
% tinc ... increment between adjacent windows in seconds. Note that tinc should be a good deal
%       less that twin.
% *********** default twin/4 ***********
%
% prr ... output prr vector. It has mesurements every tinc seconds but has been interpolated to
%       be the same size as t.
%
% G.F. Margrave, Devon Canada, 2016
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

if(nargin<4)
    twin=(t(end)-t(1))/10;
end
if(nargin<5)
    tinc=twin/4;
end

if(length(r)~=length(rp))
    error('r and rp must have the same length');
end
if(length(t)~=length(r))
    error('r and t must have the same length');
end

tmin=t(1);
t=t(:);
r=r(:);
rp=rp(:);

% %do a preliminary least-squares scaling
% [tmp,a]=lsqsubtract(r,rp);
% rp=a*rp;

% determine number of windows. tinc will be adjusted to make the
% last window precisely centered on tmax
tmax=t(end);
nwin=(tmax-tmin)/tinc+1; %this will generally be fractional
nwin=round(nwin);
tinc=(tmax-tmin)/(nwin-1); %redefine tinc
tprr=zeros(nwin,1);
prr2=zeros(nwin,1);
nt=length(t);
dt=t(2)-t(1);
n2=round(2*twin/dt);
nuse=-n2:n2;
for k=1:nwin
    %build the gaussian
    tnot=(k-1)*tinc+tmin;
    inot=round(tnot/dt)+1;
    iuse=nuse+inot;
    if(iuse(1)<1)
        iuse=1:iuse(end);
    end
    if(iuse(end)>nt)
        iuse=iuse(1):nt;
    end
    tprr(k)=tnot;
    gwin=exp(-((t(iuse)-tnot)/twin).^2);
    %window and measure correlation
    rw=r(iuse).*gwin;
    rpw=rp(iuse).*gwin;
    [tmp,a]=lsqsubtract(rw,rpw);
    rpw=a*rpw;
    prr2(k)=1-sum((rw-rpw).^2)/sum(rw.^2);
end

prr=interp1(tprr,prr2,t);