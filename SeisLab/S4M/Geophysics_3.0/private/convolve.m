function c = convolve(a,b)
%  Function convolves the two input vectors/matrices
%  (if one of the inputs is a matrix, the other must either
%  be a vector or a matrix with the same number of columns)
%  c = convolve(a,b)
%
[na,ma]=size(a);
[nb,mb]=size(b);
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
