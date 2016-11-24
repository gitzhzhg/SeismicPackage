classdef Trace 
% Class for SEGY Trace Headers
% Summary of this class goes here
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

    %   Detailed explanation goes here
    
    properties
       traceheader;
       tracedata;
    end
    
    methods 
         function obj = Trace(obj_file, varargin)
             if ischar(obj_file)
             obj.traceheader=TraceHeader(obj_file,varargin{:});
            obj.tracedata=TraceData(obj_file,varargin{:});
             elseif isa(obj_file,'TraceHeader')
                 obj.traceheader=obj_file;
                 obj.tracedata=TraceData(obj.traceheader.filename,varargin{:});
             end
         end
         function obj=getTraces(obj,varargin)
             if isempty(obj.traceheader.traceoffsets) && isempty(obj.tracedata.data)
                 obj=obj.getTracesfromfile(varargin{:});
             else
         obj=obj.getTraceswithtracehead(varargin{:});
             end
         end
         function writetrace(obj)
            obj.writeTrace();
         end
         function delete(obj)
             obj = obj.closeFile();            
         end
    end
    
    methods (Static)
        [type bytes endian]=uigettracetype(obj,numoftraces);
    end
end