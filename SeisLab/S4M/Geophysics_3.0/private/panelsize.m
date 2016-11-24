function [nrows,ncols]=panelsize(pwells,dim)
% Get the number of columns/rows of the panels of pseudo-wells
%
% Written by: E. Rietsch: January 15, 2004
% Last updated:
%
%         [nrows,ncols]=panelsize(pwells,dim)
% INPUT
% pwells  pseudo-well structure
% dim     optional dimension for which to compute length (like size(a,2))
% OUTPUT
% nrows   number of rows
% ncols   number of columns

mnem=pwells.panel_info{1};
if nargin == 1
   [nrows,ncols]=size(pwells.(mnem));

elseif nargin == 2
   nrows=size(pwells.(mnem),dim);

else
   error('One or two input arguments required')

end
