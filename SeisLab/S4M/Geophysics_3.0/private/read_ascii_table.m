function [matrix,header,name]=read_ascii_table(filename)
% Function reads data in columnar form from file; Text data at the beginning 
% of the file are collected in a cell array "header".
% The header must not contain a line that is completely numeric.
%
% Written by: E. Rietsch: September 30, 2004
% Last updated: April 17, 2006: add first data row to "header"
%
%            [matrix,header,name]=read_ascii_table(filename)
% INPUT
% filename   file name (optional)
% OUTPUT
% matrix     matrix of data values read (empty if nothing has been read)
% header     cell vector with ASCII header (one cell per line)
%            The last row of "header" is actually the first row of data
%            (apart from formatting equivalent to num2str(matrix(1,:));
%            This row has been added for diagnostic purposes and
%            can be easily dropped if not needed.
% name       file name without extension


global S4M

run_presets_if_needed

%       Try to read file if file name is provided
if nargin > 0
   fid=fopen(filename);
else
   fid=-1;
end


if fid == -1    % File name not provided or wrong
   selected_file=get_filename4r('tbl');
   fid=fopen(selected_file,'rt');
   if fid < 0
      matrix=[];
      header=[];
      name=[];
      return
   end
   [dummy,name]=fileparts(selected_file);   %#ok First ourput argument not required

else
   [filepath,name,ext]=fileparts(filename);
   S4M.filename=[name,ext];
   S4M.pathname=filepath;
end 

hpatience=gui_patience({[' ... reading tabular data from file "',name,'"']});

%       Read the header (variable "number" is empty for the header)
header=cell(500,1);
number=[];
ik=1;
while isempty(number)
   line=fgetl(fid);
   header{ik}=line;
%   number=sscanf(line,'%g');
   number=str2num(line);        %#ok  Possible multiple numbers
   ik=ik+1;
end
if nargout > 1
   header=header(1:ik-1,1);
end

ncol=length(number);

matrix=fscanf(fid,'%g',[ncol,inf])';
fclose(fid);

try2delete(hpatience)
drawnow

try
   matrix=[number;matrix];
catch
   msgdlg('"read_ascii_table" has problem reading data values.')
end
