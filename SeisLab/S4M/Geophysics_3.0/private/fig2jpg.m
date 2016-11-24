function fig2jpg(figure_handle,reverse)
% Export figure in JPEG format
%
% Written by: E. Rietsch: January 20, 2003
% Last updated: March 10, 2008: Use "print" function instead of "advexpfig"
%
%          fig2pp(figure_handle,reverse)
% INPUT
% figure_handle    figure number 
%          Default (if not given or empty): figure_handle=gcf
% reverse  Reverse the figure background and axis colors and adjust graphs
%          (see "whitebg")
%          Default: reverse=true;
%          S4M.invert_hardcopy must be set to 'off' to have an effect
%          (See figure property 'InvertHardcopy')

% UPDATE HISTORY
%           March 15, 2006: Replace "exportfig" by "advexpfig"
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

if isempty(S4M.script)
   filename=['Figure_',num2str(figure_handle),'_x',num2str(figure_number),'.jpg'];
else
   filename=[S4M.script,'_',num2str(figure_handle),'_x',num2str(figure_number),'.jpg'];
end

	% Create file name for plot file
filepath=fullfile(directory,filename);

if reverse
   whitebg(figure_handle)         % Change background to complementary colors
end

set(figure_handle,'InvertHardcopy',S4M.invert_hardcopy);

%{
if strcmp(pos,'portrait')
   width=6.5;
else
   width=10;
end
%}

if exist(directory,'dir')
%   exportfig(figure_handle,filepath,'Format','jpeg90','Width',width,'Color','rgb')
%   advexpfig(figure_handle,filepath,'-djpeg90','w',width);
   print('-djpeg90','-opengl','-r300',filepath)

else
   [filepath,ierr]=get_filename4w('.jpg');
   if ierr && reverse
      whitebg(figure_handle)         % Change background to complementary colors
      return
   end
   [directory,name,ext]=fileparts(filepath);
   filename=[name,ext];
%   exportfig(figure_handle,filepath,'Format','jpeg90','Width',width,'Color','rgb')
%   advexpfig(figure_handle,filepath,'-djpeg90','w',width);
   print('-djpeg90','-opengl','-r300',filepath)
end

if reverse
   whitebg(figure_handle)         % Change background to complementary colors
end

if S4M.deployed
   msgdlg(['Figure saved in file "',filename,'" in directory "',directory,'" as a JPG file.'])
else
   disp(['Figure saved in file "',filename,'" in directory "',directory,'" as a JPG file.'])
end
