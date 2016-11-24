function [winv,twinv]=toinv(wavelet,stab,n,flag)
% TOINV inverts an arbitrary waveform using match filtering
% [winv,twinv]= toinv(wavelet,stab,n,flag)
% [winv,twinv]= toinv(wavelet,stab,n)
% [winv,twinv]= toinv(wavelet,stab)
% [winv,twinv]= toinv(wavelet)
%
% TOINV uses match filtering to invert an arbitrary waveform. For a band
% limited inverse, simply follow this by a filter. A stab factor is
% supplied to avoid instabilities caused by division by small numbers. If a
% causal inverse is designed, it should be applied with convm (or conv
% followed by truncation). A noncausal inverse should be applied with
% convz.
%
%
% wavelet= input waveform to be converted
% stab= stab factor to be used expressed as a fraction of the
%       peak of the amplitude spectrum 
%       **** default = .001 ****
% n= length of inverse operator
%    **** default = round(length(wavelet)/2) ****
% flag=0 ... a fully noncausal operator is desired. (t=0 is in the middle)
%     =1 ... a causal operator (minimum phase) is desired (t=0 is the first sample).
%     =N ... where N>1 means that the operator will have N-1 samples in negative time. This
%     allows a continuum of variation between symmetric and causal. (t=0 will be sample N).
% NOTE: flag=0 is the same as flag=nsamp/2+1 where nsamp is the closest odd number of samples
%   to the requested filter length. This control is about how many samples before a
%   given time and after that time are needed to predict trdesign. Fully causal operators, even
%   if the wavelet is known to be causal, are generally not as good as those where flag is
%   slightly greater than 1 (say 2 through 10).
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
 if nargin<4, flag=1;end
 if nargin<3, n=round(length(wavelet)/2);end
 if(nargin<2)
   stab=.001;
 end
 if(flag==0)
     %make sure n is odd and smaller than length(wavelet)
     n2=floor(n/2);
     n=2*n2+1;
     if(n>length(wavelet))
         n=n-2;
     end
 end
 
 Wlet=fft(wavelet);
 bground=stab*max(abs(Wlet));
 wavelet=real(ifft(Wlet+bground));
 winv=zeros(1,round(n));
 tw=xcoord(0.,.004,winv);
 if(flag==0)
     lhs=impulse(wavelet);
 else
     lhs=impulse(wavelet,flag);
 end
 [winv,twinv]=match(wavelet,lhs,tw,max(tw),flag);

 




 		
    