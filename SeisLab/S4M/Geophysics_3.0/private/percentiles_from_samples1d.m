function loc=percentiles_from_samples1d(samples,percentiles)
% Function computes location of percentiles in a sample of a one-dimensional
% variable
%
% Written by: E. Rietsch: February 22, 2004
% Last updated: February 16, 2006: Change the criterion for change from percent to fraction in line 15
%
%           loc=percentiles_from_samples1d(samples,percentiles)
% INPUT
% samples   samples of the random variable
% percentiles  percentiles as fractions or in percent
% OUTPUT
% loc       locations such that percentiles(n) of the samples are <= loc(n)

lx=length(samples)-1;
if any(percentiles > 1)
   percentiles=percentiles/100;
end
loc=interp1(0:lx,sort(samples),lx*percentiles);
