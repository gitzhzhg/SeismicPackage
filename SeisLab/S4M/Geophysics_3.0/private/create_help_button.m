function create_help_button(funct)
% Create a menu button that displays instructions found in a text file
% with name ['help4',funct,'.txt'] in subdirectory "HelpFiles" in 
% directory S4M.mymatlab
%
% Written by: E. Rietsch: December 16, 2005
% Last updated: 
%
%         create_help_button(funct)
% INPUT
% funct   string with name of function that creates the plot for which the 
%         button is intended

global S4M

%	Check if a help file exists
filename=fullfile(S4M.helpfiles,['help4',funct,'.txt']);

fid=fopen(filename);

if fid < 0
   alert(['Help file for "',funct,'" has not been found. No help button created.'])
   return
end

label='Need help?';

%	Read help file
help_info=get_help_file(fid);

%	Determine the number of sub-menus (if any)
fields=fieldnames(help_info);

if length(fields) == 1	% No submenus
   temp=help_info.(fields{1});
   uimenu('Label',label,'Tag','help_display', ...
          'ForeGroundColor',[1 0 0],'CallBack',{@myhelpdlg,temp(2:end)});

else			% Submenus
   menu_handle=uimenu('Label',label,'Tag','help_display', ...
          'ForeGroundColor',[1 0 0]);
   for ii=1:length(fields)
%      temp=getfield(help_info,fields{ii});
      temp=help_info.(fields{ii});
      uimenu(menu_handle,'Label',temp{1},'ForeGroundColor',[1 0 0],{@myhelpdlg,temp(2:end)});
   end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function myhelpdlg(varargin)

global S4M

helpdlg(varargin{3},S4M.name)
