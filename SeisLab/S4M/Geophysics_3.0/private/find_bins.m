function binindex=find_bins(x,edges)
% Assign entries of a vector to bins defined by their edges.
% For bins defined by "edges" determine, for each x the index of the bin into 
% which it falls. If an x falls on a bin boundary its index is the average of 
% that of the two bins. The first bin boundary has index 0.5, the last n+0.5
%  where n is the number of bins. Values of x smaller than the first bin get index -inf,
% those greater than the last bin get index inf. It is like a generalization 
% of Matlab function "histc".
%
% Written by: E. Rietsch: February 20, 2007
% Last updated:
%
%          binindex=find_bins(x,edges)
% INPUT
% x        numeric vector sorted in increasing order
% edges    edges of bins (vector with strictly monotonicly increasing entries)
%          the number of bins is one less than the number of edges.
% OUTPUT
% binindex index of bins into which each element of "x" falls
%          same size as "x"
%          if, for a particular "x(n)", the binindex is, say, k then
%          this "x(n)" is between edges(k) and edges(k+1)
%
% EXAMPLE
%      x=[0,1,1,1.5,1.9,2,2.1,3.1,3.1,5.5,2,6,6.1]';
%      edges=(1:6)';
%      binindex=find_bins(x,edges)'
%      [dummy,compare_with]=histc(x,edges);
%      disp(compare_with')


[x,dummy,jj]=unique(x);

a=[x(:);edges(:)];
indicator=[false(length(x),1);true(length(edges),1)];
[a,index]=sort(a);
indicator=indicator(index);
idx=find(indicator);
%binindex=zeros(size(x));
binindex=cumsum(indicator);

for ii=1:length(idx);
   try
      if a(idx(ii)) == a(idx(ii)-1)
         binindex(idx(ii)-1)=binindex(idx(ii))-0.5;
      end
   catch
   end
   try
      if a(idx(ii)) == a(idx(ii)+1)
         binindex(idx(ii)+1)=binindex(idx(ii))-0.5;
      end
   catch
   end
end

binindex=binindex(~indicator);

%	Handle x-values outsid the bins
binindex(binindex==0)=-inf;
binindex(binindex==length(edges))=inf;

%	Re-introduce duplicate values (f they existed
binindex=binindex(jj);
