function variates=centers_from_edges(edges)
% Compute variates from edges of a histogram or PDF
% 
% Written by: E. R.: July 7, 2003
% Last updated: July 30, 2007: Handle inf values at  one or both ends 
%                              of vector "edges" (quick fix)
%
%           variates=centers_from_edges(edges)
% INPUT
% edges     edges of a histogram or of variates of PDF.
% OUTPUT
% variates  column vector of variates (centers of intervals)

%       Modify first and/or last edge it is (they are) -inf or inf, respectively
edges=edges4plotting(edges);

if isinf(edges(1))
   edges(1)=3*edges(2)-2*edges(3);
end
if isinf(edges(end))
   edges(end)=3*edges(end-1)-2*edges(end-2);
end

variates=reshape((edges(1:end-1)+edges(2:end))*0.5,[],1);
