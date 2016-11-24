function [fid,filename]=open_file(permission,varargin)
% Open an file for reading or writing 
%
% Written by: E. Rietsch: December 15, 2006
% Last updated: November 19, 2007: Additional diagnostic output
%
%           [fid,filename]=open_file(permission,varargin)
% INPUT
% permission  permission as defined in help for function "fopen"
%           to read a text file permission is 'rt'
%           to write a text file permission is 'wt'
% varargin  cell arrays --- possibly preceded by a string with the file name; 
%           If write permission is requested, the file name, if given, must 
%           be the fully-qualified name of the file to open for writing;
%           if no filename is given or if the filename is empty a 
%           file selection box will be opened.
%           If read permission is requested the file name can be a file 
%           extension (see example below). In this case the file-selection box
%           shows only files with that extension (see example below).
%           If no filename is given, if the filename is empty, or if a file
%           with the name specified is not found a file selection box will 
%           be opened.
%           The first element of each cell array is a keyword,
%           the other elements are parameters. Presently, keywords are:
%     'mformat'  machine format; see function "fopen" for possible values.
%           Default: {'mformat','native'}
% OUTPUT
% fid       file identifier
%           if file is not found "fid" is set to -1 and an error message is printed
% filename  full name of the file found
%           File name and path are also stored in S4M.filename and
%           S4M.pathname, respectively, and the path is store in S4M.table_path
%
% EXAMPLES
%           fid=open_file('r','.sgy',{'mformat','ieee-be'})  % Open binary file for reading
%           fid=open_file('wt')                              % Open text file for writing

% UPDATE HISTORY
%           December 22, 2006: Use dialog box if function is deployed
%           November 6, 2007:  Additional input parameter via keyword

global S4M

if ~isempty(varargin)
   if ischar(varargin{1})
      filename=varargin{1};
      varargin=varargin(2:end);
   end
else
   filename='';
end

%       Defaults of input arguments
param.mformat='native';

%       Replace defaults by actual input arguments
param=assign_input(param,varargin);


%	Open file for reading or writing
if nargin > 1  &&  ~isempty(filename)
   [filepath,name,extension]=fileparts(filename);
   if ~isempty(name)
      fid=fopen(filename,permission,param.mformat);
      if fid < 0
         disp([' File "',filename,'" not found. Select file interactively.'])
      end
      if ~isempty(extension)
         if ismember(extension,{'tbl','txt','asc','dat','.tbl','.txt','.asc','.dat'})
            S4M.table_path=filepath;
         elseif ismember(extension,{'sgy','segy','.sgy','.segy'})
            S4M.seismic_path=filepath;
         elseif ismember(extension,{'las','txt','.las','.txt'})
            S4M.log_path=filepath;
         elseif ismember(extension,{'mat','.mat'})
            S4M.mat_path=filepath;
         elseif ismember(extension,{'bin','.bin'})
            S4M.binary_path=filepath;
         end 
      end
      S4M.filename=[name,extension];
      S4M.pathname=filepath;
   else
      fid=-1;
   end

else
   fid=-1;
   extension='txt';
end

if fid == -1
   if ~isempty(strfind(permission,'w'))
      [filename,ierr]=get_filename4writing(extension);
   elseif ~isempty(strfind(permission,'r'))
      [filename,ierr]=get_filename4r(extension);
   else
      warning(warnid,['Unidentified permission for file opening: ',permission])
      ierr=true;
   end
   if ierr
      return
   end
   fid=fopen(filename,permission,param.mformat);
   if fid < 0
      if isdeployed
         dispdlg(['... unable to open interactively requested file "', filename,'"']);
      else
         disp(['... unable to open interactively requested file "', filename,'"']);
      end
      return
   end
end
