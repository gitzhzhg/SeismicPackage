function winv=toinv(wavelet,stab,n)
% winv= toinv(wavelet,stab,n)
% winv= toinv(wavelet,stab)
% winv=toinv(wavelet)
%
% TOINV uses match filtering to invert an arbitrary waveform.
% For a band limited inverse, simply follow this by a filter. 
% A stab factor is supplied to avoid instabilities caused by 
% division by small numbers.
%
% Note: This inverse is always causal and should be applied
% with CONV or CONVM. Sometimes a better, though 
% non-causal, inverse is obtained by TOINVF.
%
% wavelet= input waveform to be converted
% stab= stab factor to be used expressed as a fraction of the
%       peak of the amplitude spectrum 
%       **** default = .001 ****
% n= length of inverse operator
%    **** default = length(wavelet) ****
% winv= output inverse wavelet
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
 if nargin<3, n=length(wavelet);end
 if(nargin<2)
   stab=.001;
 end
 Wlet=fft(wavelet);
 bground=stab*max(abs(Wlet));
 wavelet=real(ifft(Wlet+bground));
 winv=zeros(1,round(n));
 tw=xcoord(0.,.004,winv);
 winv=match(wavelet,impulse(wavelet,1),tw,max(tw),1);
 
 		
    