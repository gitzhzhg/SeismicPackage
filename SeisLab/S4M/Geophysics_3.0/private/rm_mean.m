function mato=rm_mean(mati)
% Remove column mean from matrix
% Written by: E. R.: March 31, 2004
% Last updated:
%
%         mato=rm_mean(mati)
% INPUT
% mati    matrix
% OUTPUT
% mato    input matrix with column mean removed

m=size(mati,2);

mmat=mean(mati);
mato=zeros(size(mati));
for ii=1:m
   mato(:,ii)=mati(:,ii)-mmat(ii);
end
