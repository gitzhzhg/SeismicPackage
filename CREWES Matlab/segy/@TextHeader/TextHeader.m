classdef TextHeader < File
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

    %Class to deal with SEGY textual file headers
    
    %properties (Access = private)
    properties
        header;
        format;
        hdrsize=3200;      %bytes
        hdroffset=0;       %bytes from beginning of file.
        getas='ascii';     %'ascii' or 'ebcdic'
        getdimension='2D'; %'1D' or '2D'
    end
    
    methods
        %Constructor
        function obj = TextHeader(file,varargin)
            nnargin=nargin;
            if nnargin==0
                path=which('TextHeader');
                path=path(1:end-12);
                file=[path,'SGY_test.sgy'];
                varargin={'permission','r'};
            end
            obj=obj@File(file,varargin{:});
            if (nnargin>0)
                
                %obj.fid = fid; obj=obj@File(file,varargin{:});
                % set the hdroffset for occasions when there is extended
                % text headers
                for i = 1:2:length(varargin)
                    name = varargin{i};
                    value = varargin{i+1};
                    if strcmp(name,'txthoffset');
                        obj.hdroffset=str2double(value);
                    end
                end
                %
                info=dir(obj.filename);
                if info.bytes<=obj.hdroffset
                    [obj.header obj.format] = obj.newHeader();
                else
                    [obj.header obj.format] = readHeader(obj);
                end
            else
                [obj.header obj.format] = obj.newHeader();
            end
        end
        
        %Destructor
        %         function delete(obj)
        %             if(fid.o)
        %                 fclose(fid)
        %             end
        %         end
        
        function writeheader(obj)
            obj.writeHeader();
        end
        %Set/Get Header
        
        function obj = set.header ( obj, header )
            obj.header = obj.reshapeHeader(header,'1D');
        end % set.header
        
        function h = get.header(obj)
            %h = obj.reshapeHeader(obj.header,'matrix');
            h = obj.header;
            % convert text
            if (strcmp(obj.format,obj.getas))
                h = obj.header;
            elseif (strcmp(obj.getas,'ascii'))
                h = obj.ebcdic2ascii(obj.header);
            elseif (strcmp(obj.getas,'ebcdic'))
                h = obj.ascii2ebcdic(obj.header);
            end
            
            % reshape 1D vector to 2D matrix
            if (strcmp(obj.getdimension,'2D'))
                h = obj.reshapeHeader(h,obj.getdimension);
            end
        end
    
        function delete(obj)
             obj = obj.closeFile();            
         end
        
    end % methods
    
    methods (Static)
        header = reshapeHeader(header,shape);
        [header format] = newHeader();
        format = guessTextFormat(header)
        ascii  = ebcdic2ascii(ebcdic)
        ebcdic = ascii2ebcdic(ascii)
        
    end % end static methods
    
end