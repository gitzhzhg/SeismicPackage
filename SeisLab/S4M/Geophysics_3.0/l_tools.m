function l_tools(keyword)
% Without argument, function lists all functions that process log data.
% The argument allows restrictions to those functions which have the keyword 
% as part of the description.
%
% Written by: E. Rietsch: May, 6, 2000
% Last updated: April 28, 2006: Use new function "displayTools"
%
%           l_tools(keyword)   or   l_tools keyword
% INPUT
% keyword   Search string to restrict the output of this command to lines that contain
%           this string (needs to be in quotes if used as argument)
%
% EXAMPLES     
%           l_tools             % shows all log-related functions
%           l_tools plot        % shows log-related functions referring to plots
%           l_tools('plot')     % same as above

list=list_of_log_functions;

if nargin == 0
   keyword=[];
end

displayTools(keyword,list,'  No matching tools for well logs found.')
