function c = correlation(a,b)
%  Function computes the correlation of the two input vectors/matrices
%  (if one of the inputs is a matrix, the other must either
%  be a vector or a matrix with the same number of columns).
%      c(i)=sum(a(i+k)*b(k))
%  The number of rows of c is the nuber of rows of a
%  plus the number of rows of b minus 1.
%
% Written by: E. R.: December 12, 1996
% Last updated: March 30, 2007: Clean-up
%
%       c = correlation(a,b)
% INPUT
% a     column vector or matrix (see restrictions above)
% b     column vector or matrix (see restrictions above)
% OUTPUT
% c     cross-correlation of "a" and "b";
%       the rows of "a" and "b" are aligned (corresponding to "i=0" in the
%       above equation) for samples "cc(nb,:)" where "nb" denotes the
%       number of rows of "b".


[na,ma]=size(a);
[nb,mb]=size(b);
% b=flipud(b);
if ma == 1
   if mb == 1
      c=conv(a,b(end:-1:1));
   else
      c(na+nb-1,mb)=0;
      for ii=1:mb
         c(1:na+nb-1,ii)=conv(a,b(end:-1:1,ii));
      end 
   end

elseif mb == 1
   c(1:na+nb-1,ma)=0;
   for ii=1:ma
      c(1:na+nb-1,ii)=conv(a(:,ii),b(end:-1:1,:));
   end

elseif ma ~= mb
   error('Input data are not compatible.')

else
   c(na+nb-1,ma)=0;
   for ii=1:ma
      c(1:na+nb-1,ii)=conv(a(:,ii),b(end:-1:1,ii));
   end
end
 
