function ftm=ftmatrix(n,m)
% Function computes the Fourier transform matrix with n rows an m columns
% ftm(j,k)=exp(-2*pi*(j-i)*(k-1)/max(n,m));
%            ftm=ftmatrix(n,m)

ii=-(2*pi*(0:(n-1))'/max([n,m]))*i;
jj=0:(m-1);
ftm=exp(ii*jj);


