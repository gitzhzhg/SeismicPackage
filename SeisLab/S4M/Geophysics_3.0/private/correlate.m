function c = correlate(a,b,bool)
% Function computes the correlation of the two input vectors/matrices
% (if one of the inputs is a matrix, the other must either
% be a vector or a matrix with the same number of columns).
% c(k)=sum(a(i+k)*b(k))
% The number of rows of c is the number of rows of a
% plus the number of rows of b minus 1.
% Last updated: November 28, 2004
%
%          c = correlate(a,b,bool)
% INPUT
% a        matrix
% b        matrix
% bool     boolean variable; if true the mean is removed from all columns prior to correlation
%          default: bool=false
% OUTPUT

[na,ma]=size(a);
[nb,mb]=size(b);
if nargin > 2
   if bool
      a=rm_mean(a);
      b=flipud(rm_mean(b));
   else
      b=flipud(b);
   end
else
   b=flipud(b);
end

if ma == 1
   if mb == 1
      c=conv(a,b);
   else
      c=zeros(na+nb-1,mb);
      for i=1:mb
         c(1:na+nb-1,i)=conv(a,b(:,i));
      end 
   end
elseif mb == 1
   c=zeros(na+nb-1,ma);
   for i=1:ma
      c(1:na+nb-1,i)=conv(a(:,i),b);
   end
elseif ma ~= mb
   error('Input data are not compatible')
else
   c=zeros(na+nb-1,ma);
   for i=1:ma
      c(1:na+nb-1,i)=conv(a(:,i),b(:,i));
   end
end
  
