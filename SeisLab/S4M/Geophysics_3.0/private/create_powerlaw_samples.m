function mat=create_powerlaw_samples(nsamp,ntr,power_exponent,seed)
% Create a matrix whose columns represent time series whose power spectrua 
% are proportional to frequency^power_exponent
%
% Written by: E. Rietsch: October 25, 2005
% Last updated: November 27, 2005: streamlined code
%
%          mat=create_powerlaw_samples(nsamp,ntr,power_exponent,seed)
% INPUT
% nsamp    number of samples to create for each matrix column (number 
%          of rows of matrix)
% ntr      number of columns of matrix
% power_exponent exponent such that the power spectrum is proportional to 
%          frequency^power_exponent
% seed     seed of random-number generator (e.g. 99999)
% OUTPUT
% mat      matrix whose columns are random time series satisfying a power law
%
% EXAMPLE
%          mat=create_powerlaw_samples(1001,10,2,11111);
%          s_spectrum(mat,{'scale','dB'})

randn('state',seed);
temp=randn(nsamp,ntr);
temp1=fft(temp);

%	Create factors to multiply "white" spectrum"
f=(0:nsamp)';
f=min(f(1:end-1),f(end:-1:2));
f(2:end)=f(2:end).^(0.5*power_exponent);
f(1)=eps;

for ii=1:ntr
   temp1(:,ii)=temp1(:,ii).*f;
end

mat=real(ifft(temp1));
for ii=1:ntr
   mat(:,ii)=mat(:,ii)*norm(temp(:,ii))/norm(mat(:,ii));
end
