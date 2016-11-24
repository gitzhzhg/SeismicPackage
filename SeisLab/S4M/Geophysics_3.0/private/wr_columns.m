function wr_columns(filename,data,text,format)
% Function writes array to ASCII file
%
% Written by: E. R.:
% Last updated: December 18, 2006: use function "open_file"
% 
%             wr_columns(filename,data,text,format)
%
% INPUT
%   filename  name of file to create (if empty, file will be interactively selected)
%   data      data to store in columnar format
%   text      cell array with ASCII text to be placed in front of data
%             If "text" is empty but global variable S4M is not empty
%             then the string ['Created by ',S4M.script] will be printed on 
%             the first line
%   format    optional string with format for coversion of numeric data
%             Default: '%10.6g'  

global S4M ABORTED

ABORTED=true;

%       Open the file
fid=open_file('w',filename);

if fid < 0
   return
end

%       Write text above data
if nargin > 2
   if isempty(text)
      if ~isempty(S4M)
         fprintf(fid,['Created by ',S4M.script,'\n']);
      end
   else
      if ~iscell(text)
         fprintf(fid,[strrep(text,'\','\\'),'\n']);
      else
         for ii=1:length(text)
            fprintf(fid,[strrep(text{ii},'\','\\'),'\n']);
         end
      end
   end
end

%       Write data
[n,m]=size(data);

if nargin > 3
   format=['%s',format];
else
   format=' %s %10.6g';
end

for ii=1:n
   if mod(ii,10000) == 0
      fprintf('%d of %d lines of data written.\n',ii,n);
   end
   for jj=1:m
      fprintf(fid,format,'   ',data(ii,jj));
   end
   fprintf(fid,'\n'); 
end

fclose(fid);
ABORTED=false;
