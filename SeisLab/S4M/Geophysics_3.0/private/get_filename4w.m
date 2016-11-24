function [selected_file,ierr]=get_filename4w(ext,filename)
% Function interactively gets a file name with extension ext for writing a data set
%
% Written by: E. Rietsch, December 13, 2000
% Last updated: March 28, 2007: Add a default figure directory
%
%              [selected_file,ierr]=get_filename4w(ext,filename)
% INPUT
% ext          file extension (including .) or filename. Extensions for which 
%              paths are defined are: 
%              {'sgy','segy'},'log',{'tbl','asc','txt','dat'},'mat',{'eps','jpg','jpeg','emf'}
%              Default: '.mat'
%              This extension is used to choose the initial directory path
%              and to show only files with this extension. Both can be changed 
%              in the file selector box; the dot (".") in front of the extension
%              is not required (extension "txt" and ".txt" are equivalent).
% filename     optional file name (without path)
% OUTPUT
% selected_file  filename including path
% ierr         error code. ierr = false if a filename was selected and 
%              ierr = true if not.
%              global variables: S4M.filename   name of the file selected
%                                S4M.pathname   name of the path selected
%                                ABORTED (same as ierr)
%              this means that   selected_file = fullfile(S4M.pathname,S4M.filename)

global S4M ABORTED

run_presets_if_needed

if nargin < 2 
   filename='';
   if nargin == 0
      ext='.mat';
   end
else
   filename=strrep(filename,';',',');
   filename=strrep(filename,'\','_');
   filename=strrep(filename,'/','_');
   filename=strrep(filename,'*','_');
   filename=strrep(filename,':','_');
   filename=strrep(filename,'"','_');
   filename=strrep(filename,'?','_');
   filename=strrep(filename,'|','_');
   filename=strrep(filename,'>','_');
   filename=strrep(filename,'<','_');
end

ierr=false;

oldDir=pwd;

%        Open file selector window
if ismember(ext,{'sgy','.sgy'})
   filter_spec={'*.sgy;*.segy', 'Seismic files (*.segy, *.sgy)'; ...
                '*.*',          'All files (*.*)'};
   dialogue='Write SEG-Y file';
   try
      cd(S4M.seismic_path)
   catch
   end

elseif ismember(ext,{'mat','.mat'})
   filter_spec={'*.mat', 'Matlab mat files (*.mat)'; ...
                '*.*',          'All files (*.*)'};
   dialogue='Write Matlab MAT file';
   try
      cd(S4M.mat_path)
   catch
   end

elseif ismember(ext,{'las','.las'})
   filter_spec={'*.las', 'Well log files (*.las)'; ...
                '*.*',          'All files (*.*)'};
   dialogue='Write LAS file';
   try
      cd(S4M.log_path)
   catch
   end

elseif ismember(ext,{'tbl','asc','txt','dat','.tbl','.asc','.txt','.dat'})
   filter_spec={'*.asc;*.tbl;*.txt;*.dat', 'Table files (*.tbl, *.txt, *.dat, *.asc)'; ...
                '*.*',          'All files (*.*)'};
   dialogue='Write TABLE to file';
   try
      cd(S4M.table_path)
   catch
   end

elseif ismember(ext,{'emf','jpg','eps'})
   filter_spec={'*.emf;*.jpg;*.jpeg,*.eps;',' Figure files (*.emf, *.jpg, *.jpeg, *.eps)'; ...
                '*.*',          'All files (*.*)'};
   dialogue='Save Figure to file';
   try
      cd(S4M.figure_path)
   catch
   end

else
   filter_spec=ext;
   dialogue='Select file for output';
end 

try
   [filename,pathname]=uiputfile(filter_spec,dialogue,filename);
catch
   alert('Failure to get a file name (uiputfile aborted)')
   filename=0;
end

drawnow
cd(oldDir)

if isnumeric(filename) && filename == 0 
   uiwait(errordlg(' No file selected. Task terminated',S4M.name,'modal'))
   selected_file='';
   ierr=true;
   ABORTED=true;
   return
end

selected_file=[pathname,filename];
if ~S4M.deployed
   disp(['File    ',selected_file,'    interactively selected']);
end
  
%   	Set path name to the path just used
if ismember(ext,{'sgy','.sgy','segy','.segy'})
   S4M.seismic_path=pathname;
elseif ismember(ext,{'mat','.mat'})
   S4M.mat_path=pathname;
elseif ismember(ext,{'las','.las'})
   S4M.log_path=pathname;
elseif ismember(ext,{'tbl','txt','asc','dat','.tbl','.txt','.asc','.dat'})
   S4M.table_path=pathname;
elseif ismember(ext,{'emf','jpg','jpeg','eps','.emf','.jpg','.jpeg','.eps'})
   S4M.figure_path=pathname;
end 

S4M.filename=filename;
S4M.pathname=pathname;
ABORTED=false;
