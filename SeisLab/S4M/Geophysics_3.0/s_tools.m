function s_tools(keyword)
% Without argument, function lists all functions that process seismic data.
% The argument allows restrictions to those functions which have the keyword 
% as part of the description.
%
% Written by: E. Rietsch: May, 6, 2000
% Last updated: April 28, 2006: Use new function "displayTools"
%
%           s_tools(keyword)   or   s_tools keyword
% INPUT
% keyword   Search string to restrict the output of this command to lines that 
%           contain this string (needs to be in quotes if used as argument)
%
% EXAMPLES     
%           s_tools             % shows all seismic-related functions
%           s_tools plot        % shows seismic-related functions referring to plots
%           s_tools('plot')     % same as above

list=list_of_seismic_functions;

if nargin == 0
   keyword=[];
end

displayTools(keyword,list,'  No matching seismic tool found.')
