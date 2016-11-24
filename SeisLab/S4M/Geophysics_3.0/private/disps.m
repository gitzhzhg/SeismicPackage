function disps(string,lb)
% Display a string.
% Checks the dimensions of the command window and wraps the string if it is too
% long to be displayed in one line.
%
% Written by: E. Rietsch: March 29, 2008
% Last updated: January 18, 2009; Add second input argument
%
%          disps(string,lb)
% INPUT
% string   string to display in the command window
% lb       leading blanks (optional); default=0;
%
% EXAMPLE
%          disps(['Checks the dimensions of the command window and wraps ', ...
%                'the string if it is too long to be displayed in ', ...
%                'one line.'])

%     Get the dimensions of the command window
dims=get(0,'CommandWindowSize');

%     Wrap the input string and display it

if nargin == 1  || lb == 0
   cstring=linewrap(string,dims(1));
   for ii=1:length(cstring)
      disp(cstring{ii})
   end

else
   bl=blanks(lb);
   cstring=linewrap(string,dims(1)+lb);
   for ii=1:length(cstring)
      disp([bl,cstring{ii}])
   end
end
