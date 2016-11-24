function cstr=tokens(str,sep)
% Decompose string into individual strings separated by "sep"
%
% Written by: E. Rietsch: November 22, 2006
% Last updated: May 12, 2008: Handle multi-character separator
%
%         cstr=tokens(str,sep)
% INPUT
% str     string
% sep     separator; 
%         Default: sep = ','
% OUTPUT
% cstr    cell vector (row) with individual strings
%
% EXAMPLE
%         cstr=tokens('aa,bbb,ccc')
%         cstr=tokens('aa\&bbb\&ccc','\&')

if isempty(str)
   cstr=cell(1,1);
   return
end


if nargin == 1
   sep=',';
end

lstr=length(str);
lstrh=fix(lstr/(1+length(sep)));
cstr=cell(lstrh+1);

for ii=1:length(str)
   [tok,str]=strtok(str,sep); %#ok Needs to handle multi-character separators
   cstr{ii}=tok;
   if isempty(str)
      cstr(ii+1:end)=[];
      break      
   end
end
