classdef Headers
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

    %Class for SEGD header definitions
    properties
        headerType;   %One of:
                      % generalHeader1
                      % generalHeader2
                      % generalHeaderN
                      % scanTypeHeader
                      % demuxTraceHeader
                      % traceHeaderExtension
                      % generalTrailer
        fid;          % Open file id
        filePosition; % bytes from start of file (for debugging)
        header;       % SEG-D header from disk as 8-bit unsigned integers
        hexHeader;    % SEG-D header converted to hex string
        headerSize;   % 32 bytes, except for trace headers
        headerDefCols;% header word definition column names
        headerDefRows;% header word definition row names
        headerDefs;   % header word definitions
    end

    methods
        function obj = Headers(fid, headerType, varargin)
            obj.fid = fid;
            obj.headerType = headerType;
            obj.headerSize = 32;
            obj.filePosition = ftell(obj.fid);
            if(strcmp(headerType,'demuxTraceHeader'))
                obj.headerSize = 20;
            end

            obj = obj.readHeaderDefinitions();
            obj = obj.readHeader();
        end
    end

end