function [wall,tw]=toall(wavelet,twin,stab)
% TOALL Captures phase spectrum of a wavelet and outputs an all pass equivalent
% [wall,tw]=toall(wavelet,twin,stab)
% wall=toall(wavelet,twin)
% wall=toall(wavelet)
%
% TOALL uses FFT to capture the phase spectrum of an arbitrary
% wavelet and outputs an 'all pass' equivalent. An all-pass wavelet has
% unit amplitude spectrum at all frequencies.
%
% wavelet= input waveform to be converted
% twin=input time coordinate
% ************* default = not used and no output time coordinate
%               will be supplied *********
% stab=stab factor
% ************* default =.0001 ***********
%
% winv= output all pass wavelet
% tw= output time coordinate (only possible if twin is supplied)
%
% note: this result is not usually causal, use CONVZ to apply
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

if nargin<3, stab=.0001; end
 if nargin<2, twin=xcoord(0.,.002,wavelet); end
 lw=length(wavelet);
 wavelet=padpow2(wavelet);
% move time zero to middle of trace
 izero=near(twin,0.0);
 lw2=length(wavelet);
 middle=round(lw2/2);
 ishift=middle-izero;
 wavelet=[wavelet(lw2-ishift+1:lw2);wavelet(1:lw2-ishift)];
% fft
 Wlet=fft(wavelet);
 A=abs(Wlet);
 wall=real(ifft(Wlet./(A+stab*max(A))));
% grab center samples and use hanning window
 wall=wall(round(lw2/2-lw/2+1):round(lw2/2+lw/2)).*hanning(lw);
 if nargin>1
   n=length(wall)/2;
   dt=twin(2)-twin(1);
   tw=xcoord(-(n-1)*dt,dt,wall);
 end

 

 




 		