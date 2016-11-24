function [w,tw]=waveest_roywhite(s,ts,r,tr,t1,t2,wlen,stab,mu,fsmo,method)
% WAVEEST_ROYWHITE Roy White method of estimation of embedded wavelet
%
% [w,tw]=waveest_roywhite(s,ts,r,tr,t1,t2,wlen,stab,mu,method)
%
% Roy White's method assumes the convolutional model s=w*r. Cross correlate both sides with r
% to get w*rxr=sxr. Thus, the wavelet can be estimated by deconvolving rxr from sxr. In method
% 'one', rxr is assumed to be a Delta function and hence no deconvolution is required. In
% method 'two', rxr is deconvolved in the frequency domain and a stability constant is employed
% in the denominator. The correlation spectra are also smoothed by frequency domain boxcar
% convolution. Gamma-squared weighting is employed. This is the method advocated by Walden and
% White (1998). In method 'three', rxr is deconvolved in the time domain and a smoothness
% constraint (as is done in matchs) is employed. Method three is new here. In all three
% methods, the final wavelet scale factor (overall amplitude) is determined by least-squares
% subtraction.
%
% See also: waveest_match and waveest_simple
%
% s ... seismic trace
% ts ... time coordinate for s
% r ... reflectivity
% tr ... time coordinate for r
% NOTE: tr should fall entirely between ts(1) and ts(end)
% t1 ... start time of estimation window
% t2 ... end time of estimation window
% wlen ... length (seconds) of desired wavelet
% stab ... stability constant to avoid zero divide (method two)
%  *********** default .01 ***********
% mu ... smoothness parameter (method three)
%  *********** default 1 ***********
% fsmo ... size of frequency smoother in Hz (method two)
%  *********** default = 5 **********
% method ... either 'one' or 'two' or 'three'
%  *********** default 'three' ************
%
% w ... estimated wavelet
% tw ... time coordinate for w
%
% by: G.F. Margrave, Devon Canada, 2016
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

if(nargin<8)
    stab=.01;
end
if(nargin<9)
    mu=1;
end
if(nargin<10)
    fsmo=5;
end
if(nargin<11)
    method='three';
end
if(tr(1)<ts(1) || tr(end)>ts(end))
    error('tr must lie within ts')
end
dt=ts(2)-ts(1);
n2=round(.5*wlen/dt);
n=2*n2+1;%guarantees n is odd
inds=near(ts,t1,t2);
indr=near(tr,t1,t2);
m=length(inds);
m2=floor(length(inds)/2);

switch method
    case 'one'
        mw=mwindow(length(inds),20);%window helps a little
        %mw=ones(length(ind),1);
        w=ccorr(s(inds).*mw,r(indr).*mw,n);
        s1=convz(r(indr),w);
        [~,a]=lsqsubtract(s(inds),s1);
        w=a*w';
        tw=dt*(-n:n)';
    case 'two'
        %frequency domain, with convolutional smoothing and gamma-squared weighting
        mw=mwindow(length(inds),20);%window helps a little
        %mw=gwindow(length(inds));
        %mw=ones(length(ind),1);
        ccsr=ccorr(s(inds).*mw,r(indr).*mw,n2);
        ccrr=ccorr(r(indr).*mw,r(indr).*mw,n2);
        ccss=ccorr(s(inds).*mw,s(inds).*mw,n2);
        CCSR=fft(ccsr);
        CCRR=fft(ccrr);
        CCSS=fft(ccss);
        %spectral smoothing
        df=1/(length(CCSR)*dt);
        nsmo=round(fsmo/df);
        if(nsmo>1)
            CCSR=specsmooth(CCSR,nsmo);
            CCRR=specsmooth(CCRR,nsmo);
            CCSS=specsmooth(CCSS,nsmo);
        end
%         CCRR=convz(CCRR,ones(1,5)/5);
        ACCRR=abs(CCSR).^2;
        Amax=max(ACCRR);
        gammaN2=(CCRR.*CCSS)./(abs(CCSR).^2+stab*Amax);
        %gammaN2=CCSS./CCRR;
%       gammaN2=ones(size(CCRR));
        CCmax=max(CCRR);
        W=gammaN2.*CCSR./(CCRR+stab*CCmax);

        w=real(fftshift(ifft(W)));
        s1=convz(r(indr),w);
        [~,a]=lsqsubtract(s(inds),s1);
        w=a*w';
        tw=dt*(-n2:n2)';
    case 'three'
        %time domain with smoothness
        mw=mwindow(length(inds),20);%window helps a little
        %mw=gwindow(length(inds));
        %mw=ones(length(ind),1);
        ccsr=ccorr(s(inds).*mw,r(indr).*mw,m2)';
        ccrr=ccorr(r(indr).*mw,r(indr).*mw,m2)';
        CCRR=convmtx(ccrr,n);
        nn=size(CCRR,1)-size(ccsr,1);
        nh=fix(nn/2);
        ccsrp=[zeros(nh,1);ccsr;zeros(nn-nh,1)];
        % generate a 2nd derivative operator
        D=zeros(n,n);
        for k=2:n-1
            D(k,k-1:k+1)=[1 -2 1];
        end
        D(1,1:2)=[1 -1];
        D(n,n-1:n)=[1 -1];
        D2=D'*D;
        
        CRCR=CCRR'*CCRR;%CCRR is equal to CCRR';
        
        B=CRCR+mu*D2;
        
        w=inv(B)*CCRR'*ccsrp; %#ok<MINV>
        s1=convz(r(indr),w);
        [~,a]=lsqsubtract(s(inds),s1);
        w=a*w;
        tw=dt*(-n2:n2)';
        
end

function Ssmo=specsmooth(S,nsmo)
% S ... input complex two-sided wrapped spectrum
% nsmo  ... width of smoother in samples
% Ssmo ... output one-sides spectrum after comvolutional smoothing

% unwrap spectrum
S2=fftshift(S);
%smooth
S2s=convz(S2,ones(nsmo,1))/nsmo;
%wrap spectrum
Ssmo=fftshift(S2s);