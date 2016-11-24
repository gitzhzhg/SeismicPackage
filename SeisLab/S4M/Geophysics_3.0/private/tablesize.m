function [nrows,ncols]=tablesize(table,dim)
% Get the number of columns/rows of a table
% Written by: E. Rietsch: August 13, 2003
% Last updated: February 10, 2004: compact and loose table format
%
%         [nrows,ncols]=tablesize(table,dim)
% INPUT
% table   table structure
% dim     optional dimension for which to compute length (like size(a,2))
% OUTPUT
% nrows   number of rows
% ncols   number of columns

if strcmp(table.format,'compact')
   if nargin == 1
      [nrows,ncols]=size(table.columns);
   elseif nargin == 2
      nrows=size(table.columns,dim);
   
   else
      error('One or two input arguments required.')
   end

else
   if nargin == 1
      ncols=size(table.column_info,1);
      nrows=length(table.(table.column_info{1,1}));

   elseif nargin == 2
      if dim == 1
         nrows=length(table.(table.column_info{1,1}));
      elseif dim == 2
         nrows=size(table.column_info,1);
      else
         error('Dimensions must be 1 or 2')
      end

   else
      error('One or two input arguments required')
   
   end

end
