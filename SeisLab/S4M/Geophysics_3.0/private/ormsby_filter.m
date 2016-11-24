function fa=ormsby_filter(a,dt,corner_freq)
% Function filters input array in the frequency domain with 
% trapezoidal filter with corner frequencies [f1, f2, f3, f4]. To reduce end effects
% the function doubles the number of rows of "a" by appending zeros. These zeros
% are removed prior to output. 
% The function is for internal use and performs no error checking
%
% Written by: E. Rietsch
% Last updated: January 24, 2005: bug fix
%
%        fa=ormsby_filter(a,dt,corner_freq)
% INPUT
% a      input array; each column represents a seismic trace
% dt     sample interval in ms
% corner_freq  vector with corner frequencies (0 <= f1 <= corner_freq(2) <= f3 <= f4 <= fnyquist)
% OUTPUT
% fa     filtered input array


[na,m]=size(a);
n=2^nextpow2(max(na,256));
fnyquist=500/dt; 
nh=n/2;
f=linspace(0,fnyquist,nh+1);

%    Compute trapezoidal window to apply to spectrum
trapez=zeros(n,1);
idx=find(f >= corner_freq(1) & f <= corner_freq(4));
f=f(idx);
eps1000=1000*eps;
b1=(f-corner_freq(1)+eps1000)/(corner_freq(2)-corner_freq(1)+eps1000);
b2=ones(1,length(b1));
b3=(corner_freq(4)-f+eps1000)/(corner_freq(4)-corner_freq(3)+eps1000);
trapez(idx)=min([b1;b2;b3]);
trapez(n:-1:n-nh+2)=trapez(2:nh);

%   Compute FFT, apply trapezoidal window, and perform inverse FFT.
fa=fft(a,n);
for ii=1:m
   fa(:,ii)=fa(:,ii).*trapez;
end

fa=ifft(fa);

%   Discard appended zeros and imaginary part (if the input data are real)
if isreal(a)
   fa=real(fa(1:na,:));
else
   fa=fa(1:na,:);
end
