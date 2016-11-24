function wavemin=tomin(wavelet,stab)
% wavemin= tomin(wavelet,stab)
% wavemin= tomin(wavelet)
%
% TOMIN uses the Levinson algorithm to convert an arbitrary
% waveform to its minimum phase equivalent.
%
% wavelet= input waveform to be converted
% stab= stab factor to be used expressed as a fraction of the
%       zero lag of the aurocorellogram
%   *********** default = .0001 ************
% wavemin= output minimum phase equivalent of wavelet
%
% The wavelet is autocorrelated and input to LEVREC.
% The output minimum phase wavelet is the frequency domain 
% inverse of the output from LEVREC.
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
if(nargin<2)
   stab=.0001;
 end
% generate the auto
  nlags=length(wavelet);
  a=auto(wavelet,nlags,0);
  a(1)=a(1)*(1.+stab);
  a= a/a(1);
% run this through Levinson
  b=[1.0 zeros(1,nlags-1)]';
  winv=levrec(a(1:nlags),b);
% invert the wavelet
  wavemin=real(ifft(1. ./(fft(winv'))));
% now normalize the wavelet
  powrms=sqrt(wavemin*wavemin');
  wavemin=wavemin/powrms;
 
 		
    