function style=get_linestyle(index,styles)
% Get the line style associated with index "index"
% Defaultline styles are {'-';'--';':';'-.'}
% If index "index" is greater than the number of markers specified the markers are
% repeated.
% See also: get_marker, get_color
%
% Written by: E. Rietsch: October 17, 2007
% Last updated:
%
%          style=get_linestyle(index,styles)
% INPUT
% index    index for which a line style r is requested
% markers  optional; one or more markers to replace the defaults
% OUTPUT
% mark     string with line style
%
% EXAMPLE
%          style=get_linestyle(2,{'-',':','--'})

if nargin == 1  || isempty(styles)
   styles= {'-';'--';':';'-.'};

elseif ischar(styles)
    styles={styles}; 
    
end

style=styles{mod(index-1,length(styles))+1};
