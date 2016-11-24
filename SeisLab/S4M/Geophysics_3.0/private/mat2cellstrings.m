function cellstring=mat2cellstrings(mat)
% Convert numeric matrix into a cellarray of strings
%
% Written by: E. Rietsch: September 16, 2004
% Last updated:
%
%          cellstring=mat2cellstrings(mat)
% INPUT
% mat      numeric matrix
% OUTPUT
% cellstring

dims=size(mat);
cellstring=cell(dims);
for ii=1:prod(dims);
   cellstring{ii}=num2str(mat(ii));
end
