function [r,t]= ifftrl(spec,f)
% IFFTRL inverse Fourier transform for real time series
%
% [r,t]= ifftrl(spec,f)
%
% Inverse fourier transform to a real trace. The spectrum is
% assumed to be of length floor(n/2+1) where n is the length of the time
% series to be created. (This is automatically the case if it was created by
% fftrl). This is a slightly ambiguous situation since, for
% example, time series of length 4 and length 5 will both result in 3
% frequency samples. However, only the first case will have a sample at
% Nyquist. Since the sample at Nyquist should be purely real, the algorithm
% tests the sample spec(end) for a nonzero imaginary part. If a zero
% imaginary part is found, it is assumed to be a sample at Nyquist. So, a
% problem could arise if the spectrum has been altered such that the
% Nyquist is no longer purely real. So, a good practice is to always force
% the time series to have an even number of samples (before fftrl) and then
% force the Nyquist sample to be purely real (before ifftrl).
%
% spec= input spectrum
% f= input frequency sample vector (or 2D matrix or 3D array)
% r= output trace
% t= output time vector
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

%test for matrix or vector
[m,n1,n2]=size(spec);
itr=0;
%test for vector
if( ~(m-1)*(n1-1) )
	if(m==1) spec=spec.'; itr=1; end
	nsamps=length(spec);
	nx=1;
    ny=1;
else
    %we have an array
	nsamps=m;
	nx=n1;
    ny=n2;
end

%test for 3D
%  test=size(spec);
%  if(length(test)==3)
%      threeD=1;
%  else
%      threeD=0;
%  end

% form the conjugate symmetric complex spectrum expected by ifft
%test for presence of nyquist
nyq=0;
rnyq=real(spec(end));
inyq=imag(spec(end));
small=100*eps;
if(rnyq==0 && inyq==0) nyq=1;
elseif(rnyq==0 && abs(inyq)<small) nyq=1;
elseif(rnyq==0) nyq=0;
elseif(abs(inyq/rnyq)<small) nyq=1;
end
%if(isreal(spec(end))) nyq=1; end
if(nyq)
    L1=1:nsamps;L2=nsamps-1:-1:2;
else
    L1=1:nsamps; L2=nsamps:-1:2;
end

symspec=[spec(L1,:,:);conj(spec(L2,:,:))];

% if(threeD)
%     symspec=[spec(L1,:,:);conj(spec(L2,:,:))];
% else
%     symspec=[spec(L1,:);conj(spec(L2,:))];
% end

% transform the array, force real result
 r=real(ifft(symspec));

% build the time vector
 n=size(r,1);
 df=f(2)-f(1);
 dt=1/(n*df);
 %dt=1./(2*f(length(f)));
 %tmax=size(r,1)*dt;
 %t=linspace(0.,tmax,size(r,1));
 t=dt*(0:n-1)';
 
 if(itr==1)
 	r=r.';
    t=t';
 end