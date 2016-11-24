function warnings=run_help_examples(folder)
% Run the examples in the help sections of all functions in a directory
%
% Written by: E. Rietsch: January 27, 2008
% Last updated:
%
%           warnings=run_help_examples(folder)
% INPUT
% folder    directory with functions (optional)
% OUTPUT
% warnings  cell vector with all the warnings issued
%
% EXAMPLE
%           run_help_examples('D:\MyMatlab\Current\Seislab\StableCode')


run_presets_if_needed

global S4M
S4M.batch=true;   

%%     Skip files that request user input or are obsolete
files2skip={ ...
'add_parameter';'create_seismic4different_nets';'default_modelpar';
't_matrix2table';
'l_plot1.m';
'open_file.m';
'read_las_file.m';'read_las2_file.m';'read_segy_file.m';
's_iplot.m';'s_ispectrum.m';
's_pick_horizon_2d.m';'s_volume_browser.m';'s_wavelet4landmark.m';
's_wavelet_comparison.m';'s_wavelet_from_spectrum.m';
't_plot_3d.m';
'write_las_file.m';'write_pwells2pavo.m';'write_segy_file.m'
};


%%	Get folder
if nargin == 0 || isempty(folder)
   folder=uigetfolder('Folder for m-files',S4M.myseislab);
end

warnings=cell(100,1);
files=what(folder);
ik=0;

%  Get files
allfiles=files.m;

files2test=setdiff(allfiles,files2skip);

%%    Run examples of selected files
for ii=1:length(files2test)
   warn=helpx(files2test{ii});
   if ~isempty(warn)
      ik=ik+1;
      warnings{ik}=warn;
   end
   close all   % Close all figures
   systemDefaults4Seislab
   S4M.batch=true;   
end
warnings(ik+1:end)=[];
% show(warnings)
