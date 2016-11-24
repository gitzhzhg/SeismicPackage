classdef File < hgsetget
%
%File Class
%  Open a file for file operations such as reading or writing
%  Kevin Hall, 2009
%
% NOTE: This SOFTWARE may be used by any individual or corporation for any purpose
% with the exception of re-selling or re-distributing the SOFTWARE.
% By using this software, you are agreeing to the terms detailed in this software's
% Matlab source file.

% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by
% its 'AUTHOR' (identified above) and the CREWES Project.  The CREWES
% project may be contacted via email at:  crewesinfo@crewes.org
%
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) This SOFTWARE may be used by any individual or corporation for any purpose
%    with the exception of re-selling or re-distributing the SOFTWARE.
%
% 2) The AUTHOR and CREWES must be acknowledged in any resulting publications or
%    presentations
%
% 3) This SOFTWARE is provided "as is" with no warranty of any kind
%    either expressed or implied. CREWES makes no warranties or representation
%    as to its accuracy, completeness, or fitness for any purpose. CREWES
%    is under no obligation to provide support of any kind for this SOFTWARE.
%
% 4) CREWES periodically adds, changes, improves or updates this SOFTWARE without
%    notice. New versions will be made available at www.crewes.org .
%
% 5) Use this SOFTWARE at your own risk.
%
% END TERMS OF USE LICENSE

    %

    %properties (Access = private)
     properties
         fid=-1;                 % file id, output from fopen()
     %end
     %properties
         filename;               % string containing filename
         machineformat='native'; % see 'help fopen'
         permission='r';         % see 'help fopen'
     end % end properties

     methods
         %Constructor
         function obj = File(file, varargin)
             % Constructor for File object
             %
             % Examples:
             %   File('file.sgy','permission','w','machineformat','ieee-be')
             %   File(fileid)
             %
             % Where:
             %   file          = string containing full file name or a valid
             %                   fileid from fopen()
             %   permission    = permissions for fopen(); default is 'r'
             %   machineformat = 'native', 'ieee-be', 'ieee-le'
             %

         if (nargin ==0)
                 obj.filename = [];
             else
                 % parse varargin
                 for i = 1:2:length(varargin)
                     name = varargin{i};
                     value = varargin{i+1};
                     if(~ischar(name) || ~ischar(value))
                         error('varargin must contain paired character strings');
                     end
                     switch name
                         case 'permission'
                             obj.permission = value;
                         case 'machineformat'
                             obj.machineformat = value;
                         otherwise
                     end
                 end
                 if(ischar(file))
                     obj.filename = file;
                     obj.openFile;
                 elseif(isnumeric(file))
                     obj.fid = file;
                 end

             end
         end

         %Destructor
         function delete(obj)
             if(obj.fid>0)
                 fclose(obj.fid);
             end
         end

         %Set functions
 %         function obj = set.permission(obj, p)
 %             obj.permission=p;
 %         end
         function obj = set.machineformat(obj, mf)
             switch(mf)
                case('native')     %native byte order
                    obj.machineformat=mf;
                case('ieee-be')    %big-endian byte order
                    obj.machineformat=mf;
                case('ieee-le')     %open or create file for writing; append data to end of file
                    obj.machineformat=mf;
                 otherwise
                    obj.machineformat='native';
                    warning('machineformat must be one of ''native,'' ''ieee-be,'' or ''ieee-le''');
             end
         end
         function obj = set.filename(obj, fn)
             if (ischar(fn))
                obj.filename = fn;
             else
                obj.filename = [];
                warning('filename must be a character string');
             end
         end

         %Open and close file
         function obj = openFile(obj)
             %skip if no filename
             if(ischar(obj.filename))
                 % Open file for file operations
                 obj.fid = fopen(obj.filename, obj.permission, obj.machineformat);
                 if (obj.fid == -1)
                     error(['Unable to open ''' obj.filename ''' for file operations']);
                 end
             else
                 warning('No filename, can''t open file for file operations');
             end
         end

         function obj = closeFile(obj)
             if ~isempty(obj.fid)
             if(obj.fid>0)
                 fclose(obj.fid);
                 obj.fid = -1;
             end
             end
         end

         
     end % end methods
     
     methods (Static)
     mryflag=memoryallowancecheck(szneeded,type,allowance);
     end
     
 end % end classdef
 