function fig2pp(figure_handle,reverse)
% Export figure for use in PowerPoint (.emf format)
%
% Written by: E. Rietsch: January 20, 2003
% Last updated: March 10, 2008: Use "print" function instead of "advexpfig"
%
%          fig2pp(figure_handle,reverse)
% INPUT
% figure_handle   figure number 
%         Default (if not given or empty): figure_handle=gcf
% reverse  Reverse the figure background and axis colors and adjust graphs
%         (see "whitebg")
%         Default: reverse=true;
%         S4M.invert_hardcopy must be set to 'off' to have an effect
%         (See figure property 'InvertHardcopy')

% UPDATE HIOSTORY
%         March 28, 2007: Open file selection box if "S4M.pp_directory" is empty


global S4M
persistent figure_number

directory=S4M.pp_directory;

if nargin == 0
   figure_handle=gcf;
   reverse=false;
elseif nargin == 1
   reverse=false;
else
   if isempty(figure_handle)
      figure_handle=gcf;
   end
end

if isempty(figure_number)
   figure_number=1;
else
   figure_number=figure_number+1;
end

figure(figure_handle)	% Make figure the current figure
% pos=get(figure_handle,'PaperOrientation');

%       Create a file name
if isempty(S4M.script)
   filename=['Figure_',num2str(figure_handle),'_x',num2str(figure_number),'.emf'];
else
   filename=[S4M.script,'_',num2str(figure_handle),'_x',num2str(figure_number),'.emf'];
end

	% Create a path for the figure file
if isempty(directory)
   [filepath,ierr]=get_filename4w('emf',filename);
   if ierr
      return
   end
else  
   filepath=fullfile(directory,filename);
end

if reverse
   whitebg(figure_handle)         % Change background to complementary colors
end

set(figure_handle,'InvertHardcopy',S4M.invert_hardcopy);

try
   %{
   if strcmp(pos,'portrait')
      width=12.7;
      height=16.12;
   else
      width=25.4;
      height=16.12;
   end
   %}
%   exportfig(figure_handle,filepath,'Format','meta','Width',width,'Color','rgb')
%   advexpfig(figure_handle,filepath,'-dmeta','w',width,'h',height)
   print('-dmeta',filepath)

catch %#ok
   [filepath,ierr]=get_filename4w('.emf');
   if ierr && reverse
      whitebg(figure_handle)         % Change background to complementary colors
      return
   end
   [directory,name,ext]=fileparts(filepath);
   filename=[name,ext];
%   exportfig(figure_handle,filepath,'Format','meta','Width',width,'Color','rgb')
%   advexpfig(figure_handle,filepath,'-dmeta','w',25.4,'h',16.12)
   print('-dmeta',filepath)
   
end

if reverse
   whitebg(figure_handle)         % Change background to complementary colors
end

if S4M.deployed
   msgdlg(['Figure saved in file "',filename,'" in directory "',directory,'" as a Windows Enhanced Meta File.'])
end

