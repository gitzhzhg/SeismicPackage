function winv=toinvf(wavelet,stab,n)
% TOINVF FFT based waveform inversion with low-pass zero-phase filter
% winv=toinvf(wavelet,stab,n)
% winv=toinvf(wavelet,stab)
% winv= toinvf(wavelet)
%
% TOINVF uses FFT to invert an arbitrary waveform over its
% entire frequency band except for a zero phase high frequency
% attenuation. A stab factor is supplied to avoid
% instabilities caused by division by small numbers.
%
% wavelet= input waveform to be converted
% stab= stab factor to be used expressed as a fraction of the
%       peak of the amplitude spectrum 
%   *********** default = .001 ************
% n= length of inverse (number of points)
%  ********** default= length(wavelet) ***********
% winv= output inverse wavelet. Winv will be the same length as
%       wmin. It is occaisionally advisable to pad wmin with zeros
%       before inverting.
%
% note: this inverse is not usually causal, use CONVZ to apply
% it (default time zero to length(winv)/2)
%
% by G.F. Margrave, June 1991
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

 if nargin<3, n=length(wavelet); end
 if(nargin<2)
   stab=.001;
 end
% prepare the wavelets
 lw=max([length(wavelet),n]);
 wavelet=padpow2(wavelet,1);
 while (length(wavelet)/lw)<1.5
  wavelet=padpow2(wavelet,1);
 end
  imp=impulse(wavelet);
% invert the wavelet
  Wlet=fft(wavelet);
  Imp=fft(imp);
  bground=zeros(size(Wlet))+stab*max(abs(Wlet));
  indicies= find(abs(Wlet)<bground);
  Wlet(indicies)=bground(indicies);
% invert
  Winv=Imp./Wlet;
% band limit it
  mw=mwindow(length(Winv)+1,10);
  mw=fftshift(mw(1:length(Winv)));
  Winv=Winv.*mw;
% back to time
  winv=real(ifft(Winv));
  n2=length(winv)/2;
  winv1=winv(round(n2-n/2+1):round(n2+n/2)).*hanning(n)';
  winv=winv1;

 




 		
    