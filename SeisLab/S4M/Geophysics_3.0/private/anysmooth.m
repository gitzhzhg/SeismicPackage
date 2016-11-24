function avin=anysmooth(in,weights,endeffects)
% Function computes a weighted average using symmetric weights
% Written by: E. R.: July 20, 2003
% Last updated: December 12, 2004: Add third input parameter
%
%        avin=anysmooth(in,weights,endeffects)
% INPUT
% in      matrix whose columns are to be smoothed
% weights   an odd number of positive weights (should be symmetric); 
%         if they don't sum to to 1 they are divided by their sum
% endeffects  logical variable; indicates if ends of column vector should be
%         treated differently.  
%         if endeffects == 0 then zeros are deemed to be prepended and appended 
%         to the vector/matrix
%         Default: endeffects=1;
% OUTPUT
% avout   matrix with averaged columns (same size as "in"). 
%         If endeffects == 1 then at both ends of the column
%         vectors the averaging is performed over fewer samples
%         This preserves the mean (mean(in) == mean(avin))
%         
% SEE ALSO    smooth, binsmooth
%
% EXAMPLE
%         a=[1,0,0,0,1,0,0,0,1]';  as=anysmooth(a,[1 2 1])
%        

if nargin < 3
   endeffects=true;
end

nweights=length(weights);

if nweights == 1
   avin=in;
   return
end

if mod(nweights,2) ~= 1
   error('Number of weights must be odd')
end

if sum(weights ~= 1)
   weights=weights/sum(weights);
end

[n,m]=size(in);
if n == 1
   in=in';
   [n,m]=deal(m,n);
   flip=1;
else
   flip=0;
end

%       Create an odd smoothing filter
idist=nweights-1;
% weights=binom(idist,idist+1)/2^idist

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

if endeffects
%       Repair wrong scaling at ends of data sets
   for ii=1:append
      fact=sum(weights(1:idist/2+1-ii));
      avin(ii,:)=avin(ii,:)+in(ii,:)*fact;
      avin(end-ii+1,:)=avin(end-ii+1,:)+in(end-ii+1,:)*fact;
   end
end

if flip
   avin=avin';
end
