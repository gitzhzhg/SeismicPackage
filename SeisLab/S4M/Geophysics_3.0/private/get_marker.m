function mark=get_marker(index,markers)
% Function gets the marker symbol associated with index "index"
% Default markers are {'^';'v';'<';'>';'s';'d';'+';'x';'o';'p';'h';'.'}
% If index "index" is greater than the number of markers specified the markers 
% are repeated.
% See also: get_linestyle, get_color
%
% Written by: E. Rietsch
% Last updated: November 30, 2008: Change how the second input argument is
%                                  handled if it is empty
%
%          mark=get_marker(index)
% INPUT
% index    index for which a marker is requested
% markers  optional; one or more markers to replace the defaults
% OUTPUT
% mark     string with marker symbol
%
% EXAMPLE
%          mark=get_marker(2,{'d','s','o'})

% UPDATE HISTORY
%          April 5, 2007: Add second input argument


if nargin == 1 
   markers={'^';'v';'<';'>';'s';'d';'+';'x';'o';'p';'h';'.'};

elseif ischar(markers)
    markers={markers}; 
    
end

mark=markers{mod(index-1,length(markers))+1};
