function [wavelet,twave]=minwave(ampspec,f,stab,n)
% [wavelet,twave]=minwave(ampspec,f,stab)
% [wavelet,twave]=minwave(ampspec,f) 
%
% Given an input amplitude spectrum, minwave computes the 
% corresponding minimum phase wavelet in the time domain.
% ampspec= the input amplitude spectrum. Must be positive definite 
%           everywhere if stab is zero.
% f= frequency coordinate vector
% stab= stab factor. if stab is non-zero (and positive) then 
%       ampspec(f) is altered to ampspec(f)+stab*Max(ampspec)
%       (default is zero)
% n= desired length of the wavelet
% SUGGESTION: create an appropriate f using fftrl. f must be
% a frequency vector from 0 to Nyquist of length 1 greater than
% a power of two. Ampspec must be the same length. 
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
 if(nargin==2)
   stab=0.0;
 end
% test for stab condition
 if(stab>0.0)
  	ampstab=stab*max(ampspec);
 else
    ampstab=0.0;
 end
 
 %time sample rate
 dt=.5/f(end);
 
% modify spectrum
 ampspec=ampspec+ampstab;
 
% pad to a power of 2
% a2=padpow2(ampspec);
% if(length(a2)~=length(ampspec))
% 	ind=find(a2==0.0);
% 	a2(ind)=ampstab*ones(size(ind));
% 	ampspec=a2;
% 	f=xcoord(f(1),f(2)-f(1),ampspec);
%end

ampspec=ampspec(:);%force column vector
 
% test for stability
 if(min(ampspec)<=0.0)
	  error('Amplitude spectrum not positive definite')
 end
% 
%
% construct the symmetric power spectrum
powspec=ampspec.^2;
L1=1:length(powspec);L2=length(powspec)-1:-1:2;
symspec=[powspec(L1);(powspec(L2))];
% generate the autocorrelation by ifft
a=ifft(symspec);
N=floor(length(a)/2);
if(nargin<4)
  n=N;
end
if(n>N)
  n=N;
end
% levinson recursion
b=[1.0 zeros(1,n-1)];
% do the levinson recursion
x=levrec(a(1:n),b);
% so x is the minimum hase inverse of what we want
wavelet=ifft(1./fft(x));
twave=dt*(0:n-1);

% % do the hilbert transform
%   cmpxspec=hilbm(cmpxspec);
% % cmpxspec=exp(cmpxspec);
%   cmpxspec=exp(conj(cmpxspec));
%   cmpxspec=cmpxspec-mean(cmpxspec);
% % inverse fft
%   wavelet=real(ifft(cmpxspec));
% % time coordinates
%   dt=.5/f(end);
%   twave=0.:dt:(length(wavelet)-1)*dt;