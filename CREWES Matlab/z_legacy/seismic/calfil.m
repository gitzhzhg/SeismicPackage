function trout=calfil(trin,t,aec_length,flow,fhigh,nfilt)
% trout=calfil(trin,t,aec_length,flow,fhigh,nfilt)
% 
% CALFIL emulates Chevron's time variant spectral whitening 
% program of the same name. A set of gaussian filters are
% designed to slice a frequency range in such a way that the
% linear sum of slices returns the original spectrum. Each slice
% is then ifft'd into the time domain and aec'd. Finally, the 
% slices are summed in the time domain.
%
% trin= input trace 
% t= time coordinate vector for trin
% aec_length= length (seconds) of the aec operator
% flow= lowest frequency to be whitened (Hz)
% fhigh= highest frequency to be whitened (Hz)
% nfilt= number of Gaussian filter slices
% trout= output trace
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
  
%forward fft
 m=length(trin);
 trout=zeros(size(trin));
 trin=padpow2(trin);
 t=xcoord(t(1),t(2)-t(1),length(trin));
 [spec,f]=fftrl(trin,t);
% loop over slices
 fwidth=(fhigh-flow)/(nfilt-1);
 fnot=flow;
 for n=1:nfilt
   g=gauss(f,flow+(n-1)*fwidth,fwidth); % make gaussian
   Slice=spec.*g; % make slice
   slice=ifftrl(Slice,f); % inverse transform
   slice=aec(slice,t,aec_length); % aec
   trout=trout+slice(1:m); % accumulate slices
 end
 trout=balans(trout,trin);
    
 
  
  