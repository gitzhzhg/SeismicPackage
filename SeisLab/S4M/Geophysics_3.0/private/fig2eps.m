function fig2eps(repfig,figure_handle)
% Export figure for use in reports
%
% Written by: E. Rietsch: March 4, 2003
% Last updated: November 26, 2007: Replace "advexpfig" by "print"
%
%         fig2eps(repfig,figure_handle)
% INPUT
% repfig  unique figure number for report (required)
% figure_handle   figure number 
%         Default: figure_handle=gcf
%
% GLOBAL VARIABLES
% S4M.eps_directory  Directory name in directory with papers
%         e.g.  S4M.eps_directory='Papers\Euclid_wavelet_length'
%         The full path is "fullfile(S4M.report_path,S4M.eps_directory)"

% UPDATE HISTORY
%        April 14, 2004: Use also workflow name, global variable  
%                        WF.name (if it exists)


global S4M WF

if nargin == 0
   alert(' Figure number in report is required for an EPS file to be saved')
   return
end
if nargin == 1
   figure_handle=gcf;
end
figure(figure_handle)
pos=get(figure_handle,'PaperOrientation');

if strcmpi(pos,'portrait')
   set(figure_handle,'PaperPosition',[0.8 0.5 5.5 10]);
else
   set(figure_handle,'PaperPosition',[0.8 0.5 10 5.5]);
   set(figure_handle,'PaperOrientation','portrait')
end

set(gcf, 'InvertHardcopy', S4M.invert_hardcopy)

name1='';
if isfield(S4M,'eps_directory') && ~isempty(S4M.eps_directory)
   if isfield(WF,'name')  &&  ~isempty(WF.name)      % Prepend workflow name followed by "."
      name1=[WF.name,'.'];
   end
   try
      filepath=fullfile(S4M.eps_directory,[name1,S4M.script,'_',num2str(repfig),'.eps']);
      print('-depsc2',filepath)
%      advexpfig(figure_handle,filepath,'-depsc2','w',20);
%      disp('here')
   catch
      keyboard
      try
         filepath=fullfile(S4M.report_path,S4M.eps_directory,[name1,S4M.script,'_',num2str(repfig),'.eps']);
%         advexpfig(figure_handle,filepath,'-depsc2','w',20);
         print('-depsc2',filepath)
      catch
         alert(['File "',filepath,'" could not be created. EPS file has not been saved.'])
         return
      end
   end
   

else
   alert(' Field "eps_directory" of global variables "S4M" is empty.');
   if S4M.deployed
      S4M.eps_directory=uigetdir('C:\','Directory for EPS files');
      if isempty(S4M.eps_directory)
         return
      end
      filepath=fullfile(S4M.eps_directory,[name1,S4M.script,'_',num2str(repfig),'.eps']);
%      advexpfig(figure_handle,filepath,'-depsc2','w',20);
      print('-depsc2',filepath)
   else
      disp(' Figure not saved.')
      return
   end
end


[directory,filename,ext]=fileparts(filepath);
if S4M.deployed
   msgdlg(['Figure saved in file "',filename,ext,'" in directory "',directory,'" as an Encapsulated PostScript file'])
end
disp(['Figure saved in file "',filename,ext,'" in directory "',directory,'" as an Encapsulated PostScript file'])

set(figure_handle,'PaperOrientation',pos)
