function str=remove_consecutive_blanks(str)
% Remove consecutive blanks in the input string (or a cell vector of strings)
% and replace them by a single blank.
% Also remove any leading and trailing blanks.
%
% Written by: E. Rietsch: December 19, 2006
% Last updated:
%
%        str=remove_consecutive_blanks(str)
% INPUT
% str    string or cell vector of strings
% OUTPUT
% str    input string (or cell vector) with consecutive blanks replaced
%        by a single blank
%
% EXAMPLES
%        str=remove_consecutive_blanks(' 1  2 3    5     7 ')
%        cstr=remove_consecutive_blanks({' 1  2 3    5     7 ','a  r ggg h kkk   u '})

if iscell(str)
   for ii=1:length(str)
       str{ii}=rcb(strtrim(str{ii}));
   end

else
   str=rcb(strtrim(str));
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Nested function
function str=rcb(str)
   lstr0=length(str);
   str=strrep(str,'  ',' ');
   lstr=length(str);
   while lstr < lstr0  &&  lstr > 0
      lstr0=lstr;
      str=strrep(str,'  ',' ');
      lstr=length(str);
   end
end     % End of nested function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

end	% End of function
