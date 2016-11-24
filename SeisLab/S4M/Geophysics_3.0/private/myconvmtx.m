function matrix=myconvmtx(w,n)
% Function creates convolution matrix of the form
%          || w1 w2 w3  0  0  0|
%          || 0  w1 w2 w3  0  0|
%          || 0  0  w1 w2 w3  0|
%          || 0  0  0  w1 w2 w3|
% if "w" is a column vector and its transposed if it is a row vector.
% Emulation of "convmtx" of Signal Processing Toolbox
% Written by: E. Rietsch, October 21, 2005
% Last updated:
%
%       matrix=myconvmtx(w,n)
% INPUT
% w     vector (wavelet)
% n     number of rows of matrix (in the example above n = 4)
% OUTPUT
% matrix  convolution matrix

m=length(w);
mc=m+(n-1);
matrix=zeros(n,mc);

index=1:n+1:n*n;

for ii=0:m-1;
   matrix(index)=w(ii+1);
   index=index+n;
end

ntr=size(w,2);
if ntr == 1
   matrix=matrix .';
end
