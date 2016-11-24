function out=rsum(in)
% Compute two-term running sum: 
%
%       out=rsum(in);
% INPUT
% in   input matrix
% OUTPUT
% out  output matrix: out=(in(1:end-1,:)+in(2:end,:)*0.5

out=(in(1:end-1,:)+in(2:end,:))*0.5;