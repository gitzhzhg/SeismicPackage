function [cc,index]=normcorr(a,b)
% Compute maximum normalized correlation coefficients between "a" and "b'.
%
%        [cc,index]=normcorr(a,b)
% INPUT  
% a 	  matrix
% b     vector sliding by a (length of b does not exceed number of rows in a)
% OUTPUT
% cc    maximum correlation coefficients
% index shift at which this maximum is attained

[la,ba]=size(a);
lb=length(b);

if ba == 1 && la == lb	% a and b are vectors with the same number of elements
   cc=b(:)'*a/((norm(b)+eps)*(norm(a)+eps));
   index=0;
   return

else
   aa=[zeros(1,size(a,2));cumsum(a.*a)];
   bb=b(:)/(norm(b)+eps);

   cc=zeros(la-lb+1,ba);
   for ii=1:la-lb+1
      cc(ii,:)=bb'*a(ii:ii+lb-1,:)./(sqrt(aa(ii+lb,:)-aa(ii,:))+eps);
   end
   if size(cc,1) > 1
      [cc,index]=max(cc);
      index=index-1;
   else
     index(1:ba)=0;
   end
end
