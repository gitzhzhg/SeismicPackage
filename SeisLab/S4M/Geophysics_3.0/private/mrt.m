function mato=mrt(mati,vector)
% Matrix row vector multiplication (scaling of matrix columns)
% Replaces "mvt"
% Written by: E. R.: June 9, 2004
% Last updated: 
%
%         mato=mrt(mati,vector)
% INPUT
% mati    input matrix
% vector  input vector (number of entries must equal the number of 
%         columns of "mati")
% OUTPUT
% mato    scaled input matrix

[n,m]=size(mati);
if m ~= length(vector)
   error('Matrix and vector are incompatible')
end

mato=zeros(n,m);
for ii=1:m
   mato(:,ii)=mati(:,ii)*vector(ii);
end
