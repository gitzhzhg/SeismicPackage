function displayTools(keyword,list,msg)
% Display list of tools available for a specific dataset
%
% Written by: E. R.: April 28, 2006
% Last updated:
%
%          displayTools(keyword,list,msg)
% INPUT
% keyword  Search string to restrict the output of this command to lines that 
%          contain this string (needs to be in quotes if used as argument)
% list     four-column cell array with list of tools, one row per tool
%          the first two columns are numeric the second two are strings
% msg      message to display id no tools were found     

global S4M

%	Make sure global variable "S4M" is defined
run_presets_if_needed

if S4M.pd
   list=list(cell2mat(list(:,1))> 0,2:4);
else
   list=list(:,2:4);
end

%	Restrict display to those functions that can be used in the 
%       particular Matlab version.
bool=cell2num(list(:,1)) <= S4M.matlab_version;
list=list(bool,2:3);

nl=size(list,1);

if isempty(keyword)
   disp([char(list{:,1}),blanks(nl)',char(list{:,2})]);
else
   jj=0;
   index=zeros(nl,1);
   for ii=1:nl
      if ~isempty(findstr(lower(list{ii,1}),lower(keyword))) || ...
         ~isempty(findstr(lower(list{ii,2}),lower(keyword)))
         jj=jj+1;
         index(jj)=ii;
      end
   end
   if jj == 0
      disp(msg)
   else
      disp([char(list{index(1:jj),1}),blanks(jj)',blanks(jj)',char(list{index(1:jj),2})]);
   end
end    
