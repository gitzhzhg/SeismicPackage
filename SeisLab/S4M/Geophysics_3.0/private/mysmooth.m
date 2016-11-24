function avin=mysmooth(in,dist)
% Function computes average over "dist" samples of matrix "in". "dist" must be 
% greater than or equal to 1, but need not be integer.
%
% Written by: E. Rietsch:
% Last updated: July 14, 2003: modified end condition to assure that mean of 
%                              columns is not changed
%
%        avin=mysmooth(in,dist)
% INPUT
% in     matrix whose columns should be averaged
% dist   "distance" in terms of samples over which to average; 
%        no action taken if dist <= 1
% OUTPUT
% avout  matrix with averaged columns (same size as "in"). At both ends of the column
%        vectors the averaging is performed over fewer samples
% SEE ALSO: binsmooth
%
% EXAMPLE
%         a=[1,0,0,0,1,0,0,0,1];
%         b=mysmooth(a,3);
%         disp([a;b])

if dist <= 1
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
in=full(in);

%%       Create an odd smoothing filter
idist=2*fix((dist-1)/2)+1;
xdist=dist-idist;

if xdist == 0
   weights=ones(dist,1)/dist;
else
   weights=ones(idist+2,1);
   weights([1,end])=0.5*xdist;
   weights=weights/dist;
end

if length(weights) > n
   avin=mean(in);
   avin=avin(ones(n,1),:);
   return
end

append=(length(weights)-1)/2;
avin=zeros(n,m);

for ii=1:size(in,2)
   temp1=conv(in(:,ii),weights);
   avin(:,ii)=temp1(append+1:end-append);
end

%%       Repair wrong scaling at ends of data sets
for ii=1:append
   fact=sum(weights(1:append+1-ii));
   avin(ii,:)=avin(ii,:)+in(ii,:)*fact;
   avin(end-ii+1,:)=avin(end-ii+1,:)+in(end-ii+1,:)*fact;
end

if flip
   avin=avin.';
end
