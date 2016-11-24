function [selected_file,ierr]=get_filename4r(ext)
% Function interactively gets a file name with extension ext for reading a data set
%
% Written by: E. Rietsch, December 13, 2000
% Last updated: April 19, 2006: more extensions for which to save the last 
%                               directory used
%
%              [selected_file,ierr]=get_filename4r(ext)
% INPUT
% ext          file extension. Extensions for which paths are defined are: 
%              {'sgy','segy'},'log',{'tbl','asc','txt','dat'},'mat'.
%              Default: 'mat'
%              This extension is used to choose the initial directory path
%              and to show only files with this extension. Both can be changed 
%              in the file selector box; the dot (".") in front of the extension
%              is not required (extension "txt" and ".txt" are equivalent).
% OUTPUT
% selected_file  filename including path
%              global variables: S4M.filename   name of the file selected
%                                S4M.pathname   name of the path selected
%                                ABORTED (same as ierr)
%              this means that   selected_file = [S4M.pathname,S4M.pathname]
% ierr         error indicator: true if error
% GLOBAL VARIABLE
%              The filename is stored in  S4M.filename
%              the path name in S4M.pathname;


global ABORTED S4M

run_presets_if_needed

if nargin == 0
   ext='mat';
end

ierr=false;

oldDir=pwd;

%        Open file selector window
if ismember(ext,{'sgy','.sgy'})
   filter_spec={'*.sgy;*.segy', 'Seismic files (*.segy, *.sgy)'; ...
                '*.*',          'All files (*.*)'};
   dialogue='Read SEG-Y file';
   try
      cd(S4M.seismic_path)
   catch
   end

elseif ismember(ext,{'mat','.mat'})
   filter_spec={'*.mat', 'Matlab mat files (*.mat)'; ...
                '*.*',          'All files (*.*)'};
   dialogue='Read Matlab MAT file';
   try
      cd(S4M.mat_path)
   catch
   end

elseif ismember(ext,{'las','.las'})
   filter_spec={'*.las', 'Well log files (*.las)'; ...
                '*.*',          'All files (*.*)'};
   dialogue='Read LAS file';
   try
      cd(S4M.log_path)
   catch
   end
elseif ismember(ext,{'tbl','asc','txt','dat','.tbl','.asc','.txt','.dat'})
   filter_spec={'*.tbl;*.txt;*.dat;*.asc', 'Table files (*.tbl, *.txt, *.dat,*.asc)'; ...
                '*.*',          'All files (*.*)'};
   dialogue='Read TABLE from file';
   try
      cd(S4M.table_path)
   catch
   end
else
   filter_spec=ext;
   dialogue='Select file for input';
end 

try
   [filename,pathname]=uigetfile(filter_spec,dialogue);
catch
   alert('Failure to get a file name (uigetfile aborted)')
   filename=0;
end
drawnow
cd(oldDir)

if filename == 0;
   uiwait(errordlg(' No file selected. Task terminated',S4M.name,'modal'))
   selected_file='';
   ierr=true;
   ABORTED=true;
   return;
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
end 

S4M.filename=filename;
S4M.pathname=pathname;
ABORTED=false;
