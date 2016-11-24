function alert(message,calling_program)
% Function displays an alert message unless S4M.alert is set to false
%
% Written by: E. Rietsch: August 3, 2001
% Last updated: January 7, 2007: Optional second input argument
%
%          alert(message,calling_program)
% INPUT
% message  message to dispay (the message is preceeded by the string
%          'Alert from "calling_program": ' 
%          where "calling_program" is the name of the program that called "alert".
%          The message can be string of a cell array
% calling_program  optional name of the function that issued the message;
%          This is intended to specify a function name in case the alert 
%          is issued by a sufunction called by the function the user executed.

global S4M

if S4M.alert
   disp(' ')
   if nargin == 1
% 	Find name of the calling program 
      temp=dbstack; 
      if length(temp) > 1       % Alert is called from another program
         program=name_of_calling_function(3);
         if iscell(message)
            disp([' Alert from "',program,'": ',message{1}])
            for ii=2:length(message)
               disp(['     ',message{ii}])
            end
         else
            disp([' Alert from "',program,'": ',message])
         end
      else
         if iscell(message)
            disp([' Alert: ',message{1}])
            for ii=2:length(message)
               disp(['        ',message{ii}])
            end
         else
            disp([' Alert: ',message])
         end 
      end
   else
      disp([' Alert from "',calling_program,'": ',message])
   end
   disp(' ')
end
  