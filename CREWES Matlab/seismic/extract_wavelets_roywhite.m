function [wavelets,tws]=extract_wavelets_roywhite(s,t,r,t0s,twins,wlen,mu,stab,fsmo,method)
% EXTRACT_WAVELETS_ROYWHITE: extract time variant embedded wavelets with Roy White's method
%
% [wavelets,tws]=extract_wavelets_roywhite(s,t,r,t0s,twins,wlen,mu,stab,fsmo,method)
%
% Given a seismic trace and a corresponding reflectivity, this function estimates the
% time-varying embedded wavelets at user-defined times and windows using the method atributed
% to Roy White (Walden and White, 1998). The wavelets are not constrained to be causal. This
% function calls waveest_roywhite to estimate the wavelet in each time zone after isolating
% trace segments in the defined windows. Roy White's method assumes the convolutional model
% s=w*r. Cross correlate both sides with r to get w*rxr=sxr. Thus, the wavelet can be estimated
% by deconvolving rxr from sxr. In method 'one', rxr is assumed to be a Delta function and
% hence no deconvolution is required. In method 'two', rxr is deconvolved in the frequency
% domain and a stability constant is employed in the denominator. The correlation spectra are
% also smoothed by frequency domain boxcar convolution. Gamma-squared weighting is employed.
% This is the method advocated by Walden and White (1998). In method 'three', rxr is
% deconvolved in the time domain and a smoothness constraint (as is done in matchs) is
% employed. Method three is new here. In all three methods, the final wavelet scale factor
% (overall amplitude) is determined by least-squares subtraction.
%
% s ... input seismic trace
% t ... time coordinate for s
% r ... reflectivity (time domain)
% NOTE: s,t, and r must all be exactly the same size. This usually means
%       that you must isolate the postion of your seismic that correlates to r.
% t0s ... vector of wavelet extraction times. These are the center times of
%       the extraction windows.
% twins ... vector of window widths. May be a vector the same length as t0s
%       or a single entry if the windows are the same length.
% NOTE: t0s and twins must be vectors of the same length, or twins must be
%       a scalar.
% wlen ... length of the estimated wavelet expressed as a fraction of
%          twins. Since the wavelets are least-squares match filters, it is
%          possible to prescribe this length and this acts as a control.
%          Allowing too long a wavelet will match anything to anything
%          else, while too short a wavelet can lead to overly pessimistic
%          results.
% *********** default = 0.2 ************
% mu ... tradeoff parameter between wavelet smoothness and data fitting.
%       Lower means less smooth with 0 being no smoothness constraint. 
%       THis only matters for method 'three'. Should be a nonnegative
%       number.
%  *********** default 1 ***********
% stab ... stability constant to avoid zero divide. Method 'two' only.
%  *********** default .01 ***********
% fsmo ... frequency domain smoother size in Hz. Method 'two' only.
% ************ default 5 ***********
% method ... either 'one' or 'two' or 'three'. See waveest_roywhite.
%  *********** default 'three' ************
%
%
% wavelets ... cell array of extracted wavelets, the same length as t0s
% tws      ... cell array of wavelet time coordinates, the same length as t0s
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

if(nargin<10)
    method='three';
end
if(nargin<9)
    fsmo=5;
end
if(nargin<8)
   stab=.01;
end
if(nargin<7)
    mu=1;
end
if(nargin<6)
    wlen=0.2;
end

nt=length(t);
if(length(s)~=nt)
    error('length of s and t not equal');
end
if(length(r)~=nt)
    error('length of r and t not equal');
end
if(length(twins)==1)
    twins=twins*ones(size(t0s));
end
if(length(t0s)~=length(twins))
    error('length of t0s and twins not equal')
end


%ensure column vectors
r=r(:);
s=s(:);
t=t(:);

nwaves=length(t0s);
wavelets=cell(1,nwaves);
tws=wavelets;
for k=1:nwaves
    t1=t0s(k)-.5*twins(k);
    t2=t0s(k)+.5*twins(k);
    mlen=wlen*twins(k);
    [wavelets{k},tws{k}]=waveest_roywhite(s,t,r,t,t1,t2,mlen,stab,mu,fsmo,method);
end
% for k=1:nwaves
%     %build gaussian
%     sig=twins(k)*sigma;
%     %sigma=twins(k)*10;
%     mlen=wlen*twins(k);
%     g=exp(-(t-t0s(k)).^2/sig^2);
%     sg=s.*g;
%     rg=r.*g;
%     [wavelets{k},tws{k}]=matchs(rg,sg,t,mlen,icausal,mu);
% end