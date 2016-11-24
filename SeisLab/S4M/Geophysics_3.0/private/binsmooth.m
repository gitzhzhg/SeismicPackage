function avin=binsmooth(in,dist)
% Function computes a binomial-weighted average over "dist" samples of the 
% columns of matrix "in". "dist" must be greater than or equal to 1 and should
% be an odd integer
% SEE ALSO: smooth, anysmooth
%
% Written by: E. Rietsch:
% Last updated: July 9, 2003: modified end condition to assure that mean of columns is not changed
%
%        avin=binsmooth(in,dist)
% INPUT
% in     matrix whose columns should be smoothed
% dist   "distance" in terms of samples over which to average
% OUTPUT
% avout  matrix with averaged columns (same size as "in"). At both ends of the column
%        vectors the averaging is performed over fewer samples
%
% EXAMPLE
%         a=[1,0,0,0,1,0,0,0,1]'; as=binsmooth(a,3)
       
 
if dist == 1
   avin=in;
   return
end

[n,m]=size(in);
if n == 1
   in=in.';
   [n,m]=deal(m,n);
   flip=1;
else
   flip=0;
end

%       Create an odd smoothing filter
idist=2*fix((dist-1)/2);
weights=binom(idist,idist+1)/2^idist;

if length(weights) > n
   avin=mean(in);
   avin=avin(ones(n,1),:);
   return
end

append=idist/2;
avin=zeros(size(in));

for ii=1:m
   temp1=conv(in(:,ii),weights);
   avin(:,ii)=temp1(append+1:end-append);
end

%       Repair wrong scaling at ends of data sets
for ii=1:append
   fact=sum(weights(1:idist/2+1-ii));
   avin(ii,:)=avin(ii,:)+in(ii,:)*fact;
   avin(end-ii+1,:)=avin(end-ii+1,:)+in(end-ii+1,:)*fact;
end

if flip
   avin=avin.';
end
