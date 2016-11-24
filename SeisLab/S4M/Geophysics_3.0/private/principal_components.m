function [pc,d,coeff]=principal_components(mat)
% Compute the principal components of the columns of a matrix.
%
% Written by: E. Rietsch: January 29, 2008
% Last updated:
%
%          [pc,d,coeff]=principal_components(mat)
% INPUT
% mat      matrix
% OUTPUT
% pc       matrix whose columns are the principal components of "mat";
%          the first principal component is in the first column of "pc", the 
%          second in the second, etc.
% d        singular values associated with the principal components (decreasing)
% coeff    matrix such that mat=pc*coeff
%
% EXAMPLE
%          %    More rows than columns
%          mat=rand(10,3);
%          [pc,d,coeff]=principal_components(mat);
%          max(abs(mat-pc*coeff))
%
%          %    More columns than rows
%          mat1=mat';
%          [pc1,d1,coeff1]=principal_components(mat1);
%          max(abs(mat1-pc1*coeff1))
%
%          %    Use the first two principal components only as an approximation
%          %    to "mat"
%          mat_approx=pc(:,1:2)*coeff(1:2,:)

[nsamp,ntr]=size(mat);
if nsamp > ntr
   [pc,dd,v]=svd(mat,0);
   d=diag(dd);
   coeff=bsxfun(@times,d',v)';

else
   [u,dd,pc]=svd(mat',0);
   d=diag(dd);
   coeff=bsxfun(@times,u,d')';

end
