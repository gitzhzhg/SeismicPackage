function [trout,tvs_op]=gabordeconb(trin,t,twin,tinc,tsmo,fsmo,ihyp,ord,stab,phase,p,gdb,taperpct)
% GABORDECON: seismic deconvolution using the Gabor transform with Fourier operator design.
%
% [trout,tvs_op]=gabordeconb(trin,t,twin,tinc,tsmo,fsmo,ihyp,ord,stab,phase,p,gdb)
%
% GABORDECONB deconvolves a seismic trace using the Gabor transform as described
%     by Margrave and Lamoureux (2001). This version uses modified Gaussian windowing
%     and designs the operator from the Burg-Gabor spectrum of the data but applies 
%     it to the Fourier-Gabor Spectrum. Spectral smoothing may be boxcar or 
%     hyperbolic. The operator may be split between analysis and synthesis windows
%     or applied entirely in either one.
%
% trin ... input trace
% t ... time coordinate for trin
% twin ... size of gaussian temporal window (sec)
% tinc ... temporal increment between windows (sec)
% tsmo ... size of temporal smoother (sec)
% fsmo ... size of frequency smoother (Hz)
% ihyp ... 1 for hyperbolic smoothing, 0 for ordinary boxcar smoothing
%    Hyperbolic smoothing averages the gabor magnitude spectrum along
%    curves of t*f=constant.
% ************** Default = 1 ***********
% ord ... Order of the Burg spectrum. See BURG for more information
% ************** Default = 10 ***********
% stab ... stability constant
%   ************* Default = 0 **************
% phase ... 0 for zero phase, 1 for minimum phase
%   ************* Default = 1 **************
% p ... exponent that determines the analysis and synthesis windows. If g
%   is a modified Gaussian forming a partition, then the analysis window is
%   g.^p and the synthesis window is g.^(1-p). p mus lie in the interval
%   [0,1]. p=1 means the synthesis window is unity and p=0 means the
%   analysis window is unity.
% ***************** default p=1 ********************************
% ********* values other than 1 not currently recommended ******
% gdb ... number of decibels below 1 at which to truncate the Gaussian
%   windows. Used by fgabor. This should be a positive number. Making this
%   larger increases the size of the Gabor transform but gives marginally
%   better performance. Avoid values smaller than 20. Note that many gdb
%   values will result in identical windows because the truncated windows
%   are always expanded to be a power of 2 in length.
% ************** default = 60 ***************
%
% trout ... deconvolved result
% tvs_op ... complex-valued gabor spectrum of the operator. That is,
%   GaborSpectrum(trout) = GaborSpectrum(trin).*tvs_op
%
% by G.F. Margrave, May 2001 updated July 2009, July 2011, Feb 2013
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

%hidden parameters
% taperpct = size of taper applied to the end of the trace (max time)
%                 expressed as a percent of twin
% ************** default = 200% *********************
% taperpct has a mysteriously beneficial effect. Setting it to zero
% degrades the result in the latter half of the trace

if(nargin<7); ihyp=1; end
if(nargin<8); ord=10; end
if(nargin<9); stab=0; end
if(nargin<10); phase=1; end
if(nargin<11); p=1; end
if(nargin<12); gdb=60; end
if(nargin<13); taperpct=200; end

normflag=1;%means we will normalize the Gaussians

[m,n]=size(trin);
if((m-1)*(n-1)~=0)
    error('gabordeconb can currently handle only single traces. You need to write a loop')
end
if(m==1)
    trin=trin.';
    %force column vector
end

%scale taperpct to the trace length
taperpct=taperpct*twin/max(t);
%taper trace
if(taperpct>0)
   trin=trin.*mwhalf(length(trin),taperpct);
end

%compute the tvs
[tvs,trow,fcol]=fgabor(trin,t,twin,tinc,p,gdb,normflag);

%Burg model. We simply take the inverse Foruier transform of the tvs,
%thereby recovering the Gabor slices, and then compute the Burg spectrum of
%each slice. This is not implemented with great efficiency but do
% I care?
tvsb=zeros(size(tvs));
dtin=t(2)-t(1);
for k=1:length(trow)
    tmp=ifftrl(tvs(k,:),fcol);
    tvsb(k,:)=burg(tmp,t,ord);
    %tvsb(k,:)=sqrt(pburg(tmp,ord,1/dtin));
end

%stabilize
%find the minimum maximum
tmp=max(abs(tvsb),[],2);%Find the maxima of each spectrum
amax=abs(min(tmp));

dt=trow(2)-trow(1);
df=fcol(2)-fcol(1);
nt=round(tsmo/dt)+1;
nf=round(fsmo/df)+1;

if(~ihyp)
	% smooth with a boxcar
	tvs_op=conv2(abs(tvsb)+stab*amax,ones(nt,nf),'same');
else
    %hyperbolic smoothing
    tvsh=hypersmooth(abs(tvsb),trow,fcol,100);
    %estimate wavelet
    %w=mean(abs(tvs)./tvsh);
    %w=convz(w,ones(1,nf))/nf;
    %tvs_op=tvsh.*w(ones(length(trow),1),:);
    %estimate wavelet
    w=abs(tvsb)./tvsh;
    w=conv2(w,ones(nt,nf),'same');
    %final operator
    tvs_op=w.*tvsh;
    second_iter=0;
    if(second_iter)
        tmp=abs(tvs)./tvs_op;
        tmp2=hypersmooth(tmp,trow,fcol,100);
        tvs_op=tvs_op.*tmp2;
    end
%     w=mean(abs(tvs)./tvsh);
%     w=convz(w,ones(1,nf))/nf;
%     tvs_op=tvsh.*w(ones(length(trow),1),:);
    %stabilize
    amax=max(tvs_op(:));
    ind=find(tvs_op<stab*amax);
    if(~isempty(ind))
        tvs_op(ind)=stab*amax;
    end
end

%phase if required and invert
if phase==1
  %   tvs_op=exp(-conj(hilbert(log(tvs_op)'))).'; %wrong method
    L1=1:length(fcol);L2=length(fcol)-1:-1:2;
    symspec=[tvs_op(:,L1) tvs_op(:,L2)];%create neg freqs for Hilbert transform
    symspec2=hilbert(log(symspec')).';%transposing to force hilbert to work along frequency
    tvs_op=exp(-conj(symspec2(:,L1)));%toss negative freqs
else
    tvs_op=1 ./tvs_op;
end

%deconvolve

tvs=tvs.*tvs_op;

%inverse transform
trout=igabor(tvs,trow,fcol,twin,tinc,p);

trout=trout(1:length(trin));%truncate to length of input
trout=balans(trout,trin);%balance rms power to that of input