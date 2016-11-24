function [r,t]= ifftrl(spec,f)
%
% [r,t]= ifftrl(spec,f)
%
% Inverse fourier transform to a real trace. This is done in brute
% force fashion to mimic the result of Vern Herbert's complex to
% real FFT. A penalty of a factor of two is paid. The spectrum is
% assumed to be 1 sample longer than a magic length (power of 2).
% This will automatically be the case if it was created by fftrl
%
% spec= input spectrum
% f= input frequency sample vector 
% r= output trace
% t= output time vector
%
% by G.F. Margrave, May 1991
%
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

%test for matrix or vector
[m,n]=size(spec);
itr=0;
if( ~(m-1)*(n-1) )
	if(m==1) spec=spec.'; itr=1; end
	nsamp=length(spec);
	ntr=1;
else
	nsamp=m;
	ntr=n;
end
% form the conjugate symmetric complex spectrum expected by ifft
  L1=1:nsamp;L2=nsamp-1:-1:2;
  symspec=[spec(L1,:);conj(spec(L2,:))];
% transform the array
 r=real(ifft(symspec));
% build the time vector
 dt=1./(2*f(length(f)));
 tmax=(length(r)-1)*dt;
 t=linspace(0.,tmax,length(r));
 
 if(itr==1)
 	r=r.';
 end