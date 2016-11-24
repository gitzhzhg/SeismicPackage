function [w,tw]=wavemin(dt,fdom,tlength,m,stab)
% WAVEMIN: creates a minimum phase wavelet for impulsive sources
%
% [w,tw]=wavemin(dt,fdom,tlength,m,stab)
% [w,tw]=wavemin(dt,fdom,tlength,m)
% [w,tw]=wavemin(dt,fdom,tlength)
% [w,tw]=wavemin(dt,fdom)
% [w,tw]=wavemin(dt)
% 
% WAVEMIN returns a minimum phase wavelet which simulates a possible noise
% free impulsive source. Function uses TNTAMP to generate a plausible power
% spectrum. This is then inverse Fourier transformed into an
% autocorrelation which is fed into the Levinson recursion (LEVREC).
% Finally, the result from the Levinson recursion is inverted (by frequency
% domain division) to get the final wavelet.  The wavelet is normalized
% such that a sine wave at the dominant frequency is passed with unit
% amplification.
%
% dt= desired temporal sample rate (seconds)
% fdom= dominant frequency in Hz
%  ******** default: 15 Hz *******
% tlength= w length in seconds 
%  ******** default: 127*dt (ie a power of 2) *******
% m = exponent controlling spectral shape. See tntamp for a description.
%       Larger values give more rapid falloff of high frequencies. The
%       default is a moderate decay. A value like 10 or 12 will be extreme.
%       Put another way, a low value for m (like 2) gives a wavelet with a
%       very broad band that is easy to deconvolve. A high number (like 7)
%       is very narrow band and may not even be truly minimum phase. See
%       the example below. Must be between 2 and 7.
% ************ default 4 ************
% stab = white noise factor. In order to stabilize the Levinson recursion,
%       the zero-lag autocorrelation is multiplied by 1+stab. This matters
%       for values of m larger than 2.
% ************ default = .000001 **********
%
% Example: test different m values
% Copy/paste the lines below to the command line to see what m does.
%
% [w2,tw]=wavemin(.001,30,.2,2);
% [w3,tw]=wavemin(.001,30,.2,3);
% [w4,tw]=wavemin(.001,30,.2,4);
% [w5,tw]=wavemin(.001,30,.2,5);
% [w6,tw]=wavemin(.001,30,.2,6);
% [w7,tw]=wavemin(.001,30,.2,7);
% figure
% dbspec(tw,[w2 w3 w4 w5 w6 w7]);
% legend('m=2','m=3','m=4','m=5','m=6','m=7')
% prepfig
% 
% by G.F. Margrave, May 1991, 2013
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

if(nargin<5); stab=.000001; end 
if(nargin<4); m=4; end
if(m<2 || m>7)
    error('m must lie between 2 and 7')
end
 if(nargin<3)
   tlength=127.*dt;
 end
 if(nargin<2)
   fdom=15.; 
 end
 %adjust fdom to account for the weirdness of tntamp
 switch round(m)
     case 2
         f0=-.0731;m0=1.0735;
         fdom2=(fdom-f0)/m0;
     case 3
         f0=.0163;m0=.9083;
         fdom2=(fdom-f0)/m0;
     case 4
         f0=.0408;m0=.8470;
         fdom2=(fdom-f0)/m0;
     case 5
         f0=-.0382;m0=.8282;
         fdom2=(fdom-f0)/m0;
     case 6
         f0=.0243;m0=.8206;
         fdom2=(fdom-f0)/m0;
     case 7
         f0=.0243;m0=.8206;
         fdom2=(fdom-f0)/m0;
 end
  
% create a time vector
  nt=round(2.*tlength/dt)+1;
  nt=2.^nextpow2(nt);
  tmax=dt*(nt-1);
  tw= 0.:dt:tmax;
% create a frequency vector
  fnyq=1./(2*(tw(2)-tw(1)));
  f=linspace(0.,fnyq,length(tw)/2+1);
% create the power spectrum
  tmp=tntamp(fdom2,f,m);
  %tmp=(tmp/max(tmp)).^4;
  powspec=tmp.^2;
% create the autocorrelation
  auto=ifftrl(powspec,f);
  auto(1)=auto(1)*(1+stab);
% run this through Levinson
  nlags=round(tlength/dt)+1;
  b=[1.0 zeros(1,nlags-1)]';
  winv=levrec(auto(1:nlags),b);
% invert the winv
  w=real(ifft(1. ./(fft(winv))));
  tw=(dt*(0:length(w)-1))'; 
% now normalize the w
  w=wavenorm(w,tw,2);