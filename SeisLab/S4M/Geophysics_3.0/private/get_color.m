function col=get_color(index,colors)
% Get the color symbol associated with index "index"
% Colors are ['r';'g';'b';'k';'y';'c';'m']
% For index > 7 the colors are repeated, but with dashes, then with dots;
% thereafter the process repeats itself (ge_color(22) is 'r' again
% See also: get_marker, get_linestyle
%
% Written by: E. Rietsch:
% Last updated: October 29, 2007: allow RGB "colors" (second input argument)
%
%         col=get_color(index,colors)
% INPUT
% index   index for which color symbol is requested
% colors  optional sequence of colors; 
%         Default: colors={'r',  'g',  'b',  'k',  'c',  'm',  'y', ...
%                          'r--','g--','b--','k--','c--','m--','y--', ...
%                          'r:', 'g:', 'b:', 'k:', 'c:', 'm:', 'y:'}
% OUTPUT
% col     string with color string or RGB color vector
%
% EXAMPLE
%         mark=get_color(2,{'r','g','blue'})

% UPDATE HISTORY
%         March 2, 2007: allow empty "colors" (second input argument)


if nargin == 1  || isempty(colors)
   colors={'r',  'g',  'b',  'k',  'c',  'm',  'y', ...
           'r--','g--','b--','k--','c--','m--','y--', ...
           'r:', 'g:', 'b:', 'k:', 'c:', 'm:', 'y:'};

elseif ischar(colors)
    colors={colors};    
end

if isnumeric(colors)
   col=colors(mod(index-1,length(colors)-1)+1,:);
else
   col=colors{mod(index-1,length(colors)-1)+1};
end
