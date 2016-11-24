function [wavelet,tw]=ricker(dt,fdom,tlength)
% RICKER: creates a Ricker wavelet
%
% [wavelet,tw]=ricker(dt,fdom,tlength)
% [wavelet,tw]=ricker(dt,fdom) 
% [wavelet,tw]=ricker(dt) 
% 
% RICKER returns a Ricker wavelet.
%
% dt= desired temporal sample rate in seconds
% fdom= dominant frequency in Hz (default: 15 Hz)
% tlength= wavelet length in seconds (default: 127*dt 
%                                     (ie a power of 2))
% 
% The wavelet is generated from an analog expression for a 
% Ricker wavelet and then normalized. The normalization is such that a
% sinusoid of the dominant frequency is passed with unit amplitude (see
% wavenorm.m)
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

if(nargin<3)
   tlength=127.*dt;
 end
 if(nargin<2)
   fdom=15.; 
 end
% create a time vector
  n=round(tlength/dt)+1;
  nzero=ceil((n+1)/2);%zero time sample is here.
  nr=n-nzero;%number of samples to the right of nzero
  nl=n-nr-1;%number of samples to the right of nzero
  tw=dt*(-nl:nr)';
% Suppose the wavelet length is 9 samples. Then the above means that the
% sample at time 0 is #5 and nl=nr=4. If there are 8 samples, then the zero
% time sample is still the 5th sample but nl=4 and nr=3.
% create the wavelet
  pf=pi^2*fdom^2;
  wavelet=(1-2.*pf*tw.^2).*exp(-pf*tw.^2);

% normalize
% generate a refenence sinusoid at the dominant frequency
wavelet=wavenorm(wavelet,tw,2);