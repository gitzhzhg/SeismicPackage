function header=show_las_header(filename)
% Function displays/outputs the header of a LAS file
% Written by: E. Rietsch: March 18, 2000;
% Last updated: December 13, 2000: use standard form for getting file name
%
% 		header=show_las_header(filename)
% INPUT
% filename	name of the  file
%               if the name is omitted or does not refer to an existing file
%               a file selector box will pop up to allow interactive file 
%               selection
% OUTPUT
% header        LAS header as an ASCII string
%               if argument is omitted the header will be printed


%       Open the file
if nargin == 1
   fid=fopen(filename,'rt');
end

if nargin == 0 || fid==-1 
   selected_file=get_file_name4r(ext);
   fid=fopen(selected_file,'rt');
end 

%       Read LAS header
temp=fgets(fid);
header1=temp;
while ~strcmpi(temp(1:2),'~A')
   temp=fgets(fid);
   header1=[header1,temp]; %#ok
end
if nargout == 0
   disp(header1);
else
   header=header1;
end


