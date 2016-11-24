function bool=isyes(bool)
% Check if input argument is either 'yes', 'on', or the logical variable "true". 
% If this is the case the output variable is set to "true"; otherwise it 
% is set to "false". The case of the string is irrelevant.
%
% Written by: E. Rietsch: October 6, 2006
% Last updated: February 18, 2008: More error checking
%
%        bool=isyes(bool)
% INPUT
% bool   logical variable or a string
% OUTPUT
% bool   logical variable; true if input argument is the string 'yes' 
%        (case insensitive) or the logical variable "true".
%        otherwise it is false

% UPDATE HISTORY
%        October 26, 2007: The string 'on' is allowed and results in "true"


if islogical(bool)
   return

elseif ischar(bool) 
   bool0=bool;
   bool=ismember(lower(bool),{'yes','on'});
   if bool
      return
   else
      bool=~ismember(lower(bool0),{'no','off'});
      if bool
         error([' illegal argument to "isyes":', bool0])
      end
   end
   
else
   error('The input argument must be a logical variable or a string.')

end
