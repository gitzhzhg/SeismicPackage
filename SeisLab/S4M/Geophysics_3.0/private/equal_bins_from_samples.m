function edges=equal_bins_from_samples(smin,smax,delta)
% Compute bin edges for histogram calculation; all bins have the same width.
% See also: unequal_bins_from_samples
%
% Written by: E. Rietsch: March 1, 2004
% Last updated: June 10, 2004: handle case when smin == smax (create one bin)
%
%        edges=equal_bins_from_samples(smin,smax,delta)
% INPUT
% smin   smallest sample
% smax   largest sample
% delta  bin width
% OUTPUT
% edges  bin edges

span=smax-smin;
n=ceil(span/delta);
n=max(n,1);
width=n*delta;
delt=(width-span)*0.5;
edges=linspace(smin-delt,smax+delt,n+1);
