function fa=ormsby(a,dt,f1,f2,f3,f4)
% Function filters input array in the frequency domain with 
% trapezoidal filter with corner frequencies f1, f2, f3, f4. To reduce end effects
% the function doubles the number of rows of "a" by appending zeros. These zeros
% are removed prior to output. 
% The function is for internal use and performs no error checking
%
% Written by: E. Rietsch: May 2001
% Last updated: July 27, 2006: quadruple (from double) the filter length for spectrum calculation
%
%        fa=ormsby(a,dt,f1,f2,f3,f4)
% INPUT
% a      input array; each column represents a seismic trace
% dt     sample interval in ms
% f1 f2 f3 f4  corner frequencies (0 <= f1 <= f2 <= f3 <= f4 <= fnyquist)
% OUTPUT
% fa     filtered input array

precision=class(a);

[nn,ntr]=size(a);
n=4*nn;
if n > 256
   n=max(fix((nn+3)/2)*2,256);
end

nh=n/2;
fnyquist=500/dt; 
df=fnyquist/nh;

%    Compute trapezoidal window to apply to spectrum
trapez=zeros(n,1,precision);
f=0:df:fnyquist;
idx=find(f >= f1 & f <= f4);
f=f(idx);
eps1000=1000*eps;
b1=(f-f1+eps1000)/(f2-f1+eps1000);
b2=ones(1,length(b1),precision);
b3=(f4-f+eps1000)/(f4-f3+eps1000);
trapez(idx)=min([b1;b2;b3]);
trapez(n:-1:n-nh+2)=trapez(2:nh);

%   Compute FFT, apply trapezoidal window, and perform inverse FFT.
fa=fft(a,n);

%gh=fa.*trapez(:,ones(1,m));
% fh=vmt(fa,trapez);
if nn > ntr
   for ii=1:ntr
      fa(:,ii)=fa(:,ii).*trapez;
   end
else
   for ii=1:nn
      fa(ii,:)=fa(ii,:).*trapez(ii);
   end
end

%   Discard appended zeros and imaginary part (if the input data are real)
if isreal(a)
   fa=real(ifft(fa));
else
   fa=ifft(fa);
end
% fa=fa(1:nn,:);
fa(nn+1:end,:)=[];
