function [tvsq,t]=tvsqf(dt,f,q,flow,fhigh)
% [tvsq,t]=tvsqf(dt,f,q,flow,fhigh)
% [tvsq,t]=tvsqf(dt,f,q,flow)
% [tvsq,t]=tvsqf(dt,f,q)
% 
% TVSQF computes the time variant amplitude and phase spectrum
%    for a particular attenuation constant or 'Q'
% 
% dt= input time sample rate (seconds... recommended > .05)
%     this is only approximate. The actual dt achieved will 
%     vary depending on f.
% f= input frequency coordinate vector (0 to Nyquist) (Create this
%     with FFTRL
% Note: f should be a row vector
% q= Quality factor desired
% flow= lowest frequency of interest
%      ******** default= 5hz *********
% fhigh= highest frequency of interest  
%      ******** default= .95*f(length(f)) 
% tvsq= ouput time variant spectral matrix (complex)
% t= output time coordinate vector for tvsq. (The frequency 
%    coordinate vector is f).
%
% The matrix tvsq has an amplitude spectrum given by:
%      abs(tvsq)= exp(-pi*t*f/q)*bandpass(flow,fhigh)
% and a phase spectrum which is the hilbert transform (over f)
% of the log of this amplitude spectrum.
%
% by G.F. Margrave, May 1991
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
% set defaults
 if nargin<=4
   fhigh=.95*f(length(f));
 end
 if nargin<=3
  flow=5.0;
 end
% compute time coordinate vector
% the vector should be such that a FFT gives the same df as
% the frequency axis of the tvsq
 nf=length(f);
 df=f(2)-f(1);
% compute the length of a complex time series with this df
 tmax=(nf-1.5)/((nf-1)*df);
 nt=round(tmax/dt)+1;
% move to next power of 2
 nt=2.^nextpow2(nt);
 dt= 1./(nt*df);
 tmax=(nt-1)*dt;
 t=0.:dt:tmax;
% compute the amplitude spectrum of the bandpass filter
 nflow=round(flow/df);
 hlow=hanning(2*nflow+1)';
 nfhigh=nf-round(fhigh/df);
 hhigh=hanning(2*nfhigh+1)';
 nones= nf-nflow-nfhigh;
 Filt=[hlow(1:nflow) ones(1,nones) hhigh(nfhigh+2:length(hhigh))];
% compute the log amplitude spectrum of the bandlimited Q filter
% and then its hilbert transform
 tvsq=zeros(length(t),length(f));
 for jt=1:length(t)
  temp= (-pi*t(jt)/q)*f + log(Filt); % log amplitude spectrum (one sided)
% reconstruct the symmetric spectrum
  L1=1:length(temp);L2=length(temp)-1:-1:2;
  symspec=[temp(L1) conj(temp(L2))];
%  symspec=symspec+i*zeros(size(symspec));
  symspec= (conj(hilbm(symspec))); % hilbert transform
% now exponentiate
  tvsq(jt,:)=exp(symspec(1:length(temp)));
  tvsq(jt,1)=2.*eps;tvsq(jt,length(f))=2.*eps;
 end
 