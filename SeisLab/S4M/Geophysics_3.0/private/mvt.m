function mato=mvt(mati,vector)
% Matrix vector multiplication (scaling of matrix columns)
% Written by: E. R.: June 9, 2004
% Last updated: 
%
%         mato=mvt(mati,vector)
% INPUT
% mati    input matrix
% vector  input vector (number of entries must equal the number of 
%         columns of "mati")
% OUTPUT
% mato    scaled input matrix

%nvector=length(vector);
[n,m]=size(mati);
if m ~= length(vector)
   error('Matrix and vector are incompatible')
end

mato=zeros(n,m);
for ii=1:m
   mato(:,ii)=mati(:,ii)*vector(ii);
end
