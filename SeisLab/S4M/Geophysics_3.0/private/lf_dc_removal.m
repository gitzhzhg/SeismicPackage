function signal=lf_dc_removal(signal,type)
% Function removes a half-period sine function (or sine-squared function) 
% from the columns of a matrix to make them zero-mean. 
% Intended to make autocorrelations zero-mean by removing more near their center
%
% Written by: E. Rietsch: November 10, 2004
% Last updated: November 28, 2004: Handle signal matrices and vectors
%
%          signal=lf_dc_removal(signal,type)
% INPUT
% signal   column vector or matrix; the DC component of each column is removed
% type     type of DC removal
%          if type == 0: sin(pi*t/T)
%          if type ~= 0: sin(2*pi*t/T)^2
% OUTPUT
% signal   input vector without DC component
%
% EXAMPLE
%          signal=rand(100,5);
%          signal1=lf_dc_removal(signal,0);
%          s_compare(signal,signal1)
%          mytitle('Original (black) vs. after DC removal (red)')


nsamp=size(signal,1);

if  type == 0  
   test=sin(pi*(1:nsamp)'/(nsamp+1));
else
   test=sin(2*pi*(1:nsamp)'/(nsamp+1)).^2;
end

signal=signal-(test/sum(test))*sum(signal);
