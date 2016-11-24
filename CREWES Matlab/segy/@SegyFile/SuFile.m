	classdef SuFile < File

%
%SegyFile Class
% Open a SEG-Y file for reading or writing and get basic
% information about the file, such as the format of the
% text header and the byte order of the disk file
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
  
    properties
        textheader=NaN;           %
        binaryheader=NaN;         % 
        traceheader;          %
        tracedata;            %
                              % 
%         computerEndian;       % output from computer()
%                               %   string containing 'B' or 'L'
    end % end properties
    
    methods
        %Constructor/Destructor
        function obj = SuFile(filename,varargin)
            %function obj = SuFile(filname,varargin)
            %
            % Example:
            %   SuFile('file.su','machineformat','ieee-be')
            %
            % Where:
            %   filename      = string containing full segy file name
            %   permission    = permissions for fopen(); default is 'r'
            %   machineformat = string containing 'ieee-be' or 'ieee-le'
            %                   for big and little endian (optional)
            %   textfmt       = string containing 'ebcdic' or 'ascii'
            %
            
            % process varargin
            
            %Superclass constructors
            if(nargin==0)
                filename = [];
            end
            obj = obj@File(filename,varargin{:});
            
            if nargin > 0
                %   read contents of input arguments
%                 for i = 1:2:length(varargin)
%                     name = varargin{i};
%                     value = varargin{i+1};
%                     if(~ischar(name) || ~ischar(value))
%                         error('Error: input to SegyFile() must be character strings');
%                     end
%                     switch name
%                         case 'machineformat'
%                             obj.machineformat = value;
%                         case 'textfmt'
%                             obj.textformat = value;
%                         case 'mode'
%                             obj.permission = value;
%                         otherwise
%                     end
%                 end
                
                %   check byte order values
                if (~(strcmp(obj.machineformat,'ieee-be') || strcmp(obj.machineformat,'ieee-le')))
                    %warning('crewes:segy:SegyFile','endian argument not supplied or invalid. Checking file...');
                    %obj.machineformat = guessByteOrder(obj);
                end
                
                % get computer endianness
                %[c m obj.computerEndian] = computer();
                
                % read file headers                
                %obj.textheader = TextHeader(obj.fid);
                %obj.binaryheader = BinaryHeader(obj.fid);
            end
        end
        function delete(obj)
            if(obj.fid)
                fclose(obj.fid);
            end
        end                    
    end % end methods
    
    methods (Static)
        ibm = ieee2ibm(ieee);
        ieee = ibm2ieee(ibm,fmt);
    end % end static methods    
    
end % end classdef