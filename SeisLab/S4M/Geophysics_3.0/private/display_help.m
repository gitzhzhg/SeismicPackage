function display_help(funct,field)
% Displays instructions found in a text file with name ['help4',funct,'.txt'] 
% in subdirectory "HelpFiles" in directory S4M.myseislab
%
% Written by: E. Rietsch: December 20, 2005
% Last updated: February 19, 2007: Adapted to R14
%
%         display_help(funct,field)
% INPUT
% funct   string with name of function that creates the plot for which the 
%         button is intended
% field   field of the help structure which contains the help message
%         (not used if the help structure read from the text file has only one field)

global S4M

%	Check if a help file exists
filename=fullfile(S4M.myseislab,'HelpFiles',['help4',funct,'.txt']);

fid=fopen(filename);

if fid < 0
   alert(['Help file for "',funct,'" has not been found. No help message will be shown.'])
   return
end

%	Read help file and save its content in structure "help_info".
help_info=get_help_file(fid);

%	Determine the number of sub-menus (if any)
fields=fieldnames(help_info);

if length(fields) == 1	% Only one message
%   temp=getfield(help_info,fields{1});
   temp=help_info.(fields{1});
   helpdlg(temp,S4M.name)

else
   if nargin < 2
      error('No message field specified.')
   end
   try
%      temp=getfield(help_info,field);
      temp=help_info.(field);
   catch
      disp(help_info)
      if ischar(field)
         error(['Requested field of "help_info" ("',field,'") not found.'])
      else
         error('Second input argument is not a character string.')
      end
   end

   helpdlg(temp,S4M.name)
end
