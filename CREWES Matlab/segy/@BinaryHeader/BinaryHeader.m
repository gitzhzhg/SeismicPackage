classdef BinaryHeader < Header
% Class for SEGY Binary Header
%   BinaryHeader contains the binary header for the SEG-Y file.  This
%     header is stored as uint8 type values.  The definition file
%     directs matlab how to interpret these values therefor it is
%     essential that getheadervalue and setheadervalue are used.
%
%  When BinaryHeader is called filename must be entered to construct a
%    new BinaryHeader object, other arguments can be entered as listed
%    below.
%
% Arguments that can be entered into traceheader are:
%   'bindefinition', this will allow the binaryheader to be decoded using
%      a non-standard definitions file.  'bindefinition' must be
%      accompanied by a *.csv filename
%   'new', this is a flag that will create a traceheader object without
%      reading in any of the file.  This is especially usefull for when 
%      a new file is created. 'new' is accompanied by '1'.
%
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
    
    end

    methods
        function obj = BinaryHeader(file, varargin)
            obj=obj@Header(file,varargin{:});
            
            obj.hdrsize=400; %bytes
            obj.hdroffset=3200; %bytes from beginning of file.
            guessByteOrder(obj);
            new=0;
            definitions='segyrev1_file.csv';
            for i = 1:2:length(varargin)
                name = varargin{i};
                value = varargin{i+1};
                if strcmpi(name,'machineformat');
                    obj.machineformat=value;
                elseif strcmpi(name,'new')
                    new=1; 
                    elseif strcmpi(name,'bindefinitions')
                    definitions=value;  
                end
            end
            if (nargin>0)
                obj.definitions = HeaderDefinitions(definitions);
                info=dir(obj.filename);
                if info.bytes<=obj.hdroffset || new
                    obj.nontypecasthdr=uint8(zeros(obj.hdrsize,1));
                    [ab, ac, e] = computer;
                    obj.filefmt=e;
                else
                obj.filefmt=obj.getfileendiantype4sgy(obj,3224);
                readHeader(obj);
                %convertHeader2(obj)
                
                end
            end
        end
        function obj=convertHeader(obj)
            obj=convertHeader2(obj);
        end
        function writeheader(obj)
            obj.writeHeader();
        end
        
        function delete(obj)
             obj.closeFile();            
         end
%         function bhead = readBinaryHeader(fileID);
%         writeBinaryHeader(obj);
%         getBinaryHeaderValue(obj);
%         setBinaryHeaderValue(obj);
%         interpretBinaryHeader(obj);
    end
end