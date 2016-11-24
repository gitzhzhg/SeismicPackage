function [wzero,tw]=tozero(wavelet,twin)
% TOZERO Converts wavelet to zero-phase equivalent
% [wzero,tw]=tozero(wavelet,twin)
% wzero=tozero(wavelet)
%
% TOZERO uses FFT to capture the amplitude spectrum of an
% arbitrary wavelet and outputs a zero-phase equivalent.
%
% wavelet= input waveform to be converted
% twin= input time coordinate
% wzero= output zero-phase wavelet
% tw= output time coordinate. twin must be supplied for this
%     to be computed
%
% note: this result is never causal, use CONVZ to apply
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

if nargin<2, twin=xcoord(0.,.002,wavelet); end
 lw=length(wavelet);
 [rw,cw]=size(wavelet);
 wavelet=wavelet(:);
 wavelet=padpow2(wavelet);
% fft
 Wlet=fft(wavelet);
 Imp=fft(impulse(wavelet));
 A=abs(Wlet);
 wzero=real(ifft(A.*Imp));
% grab central lw samples
 lw2=length(wavelet);
 wzero=wzero( round(lw2/2-lw/2+1):round(lw2/2+lw/2) ).*mwindow(lw);
% make time vector
 if nargin==2
   n=floor(length(wzero)/2);
   dt=twin(2)-twin(1);
   tw=xcoord(-(n-1)*dt,dt,wzero);
 end
 if(rw==1);wzero=wzero';end


 




 		
    