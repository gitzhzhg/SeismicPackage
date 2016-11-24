function help_info=get_help_file(filename)
% Load ASCII file with help information and store it in a cell array
% The help information in the file has the following form
%    # field_name1 # submenu_label1
%    Select a rectangular area of the plot by clicking in one of its 
%    four corners and dragging the pointer to the opposite corner.
%    # field_name2 # submenu_label2
%    Close window by clicking the X in the window's upper right corner
%    # 
%    . . .
%
% Written by: E. Rietsch: December 16, 2005
% Last updated:
% 
%
% INPUT
% filename   optional; if given it is either the name of the file with the 
%            help info of the file ID, "fid" (obtained via fid=fopen(filename))
%            
% OUTPUT
% help_info  structure with the help info. 
%           "help_info.fieldname1{1}" contains "submenu_label1"
%           "help_info.fieldname2{2}" contains the text following the first 
%            line starting with #
%           "help_info.fieldname2{1}" contains "submenu_label2"
%           "help_info.fieldname2{2}" contains the text following the second 
%            line starting with #

if nargin == 0
   fid=fopen(get_filename4r('txt'));
elseif ischar(filename)
   fid=fopen(filename);
else
   fid=filename;
end

nlines=100;
nlabels=50;
tlines=cell(nlines,1);
first=zeros(1,nlabels);
fields=cell(nlabels,1);
labels=cell(nlabels,1);
ik=0;

for ii=1:nlines
   temp = fgetl(fid);
   if ~ischar(temp)
      first(ik+1)=ii;
      break
   end
   if ~isempty(temp) && strcmp(temp(1:1),'#')
      ik=ik+1;
      first(ik)=ii;
      temp=tokens(temp,'#');
      if isempty(temp{1})
         fields{ik}=strtrim(temp{2});
         labels{ik}='';
      else
         fields{ik}=strtrim(temp{1});
         labels{ik}=strtrim(temp{2});
      end
   else
      tlines{ii}=temp;
   end
end
fclose(fid);

help_info=struct(fields{1},{[labels(1);tlines(first(1)+1:first(2)-1)]});
for ii=2:ik
%   help_info=setfield(help_info,fields{ii},{[labels(ii);tlines(first(ii):first(ii+1)-1)]});
   help_info.(fields{ii})={[labels(ii);tlines(first(ii):first(ii+1)-1)]};
end
