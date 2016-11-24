function string=cell2str(cellvec,sep)
% Function converts cell vector of strings into a string
%
% Written by: E. Rietsch: November 11, 2003
% Last updated: January 27, 2007; handle case of empty "cellvec"
%
%          string=cell2str(cellvec,sep)
% INPUT
% cell    Cell array of strings
% sep     separator between strings output; default is blank
% OUTPUT
% string  All strings in the cell array lined up

% UPDATE HISTORY
%         August 5, 2005; make function "mlint"-compliant 


if nargin == 1
   sep=' ';
end

ncells=numel(cellvec);

if ncells == 0
   string='';

elseif ncells == 1         % Only one cell
   string=cellvec{1};
   return

else
   lengths=cellfun('length',cellvec)-1;
   lengths(1:end-1)=lengths(1:end-1)+length(sep);
   string=blanks(sum(lengths)+ncells);
   ia=1;
   for ii=1:ncells-1
      ie=ia+lengths(ii);
      string(ia:ie)=[cellvec{ii},sep];
      ia=ie+1;
   end
   string(ia:end)=cellvec{end};
end
