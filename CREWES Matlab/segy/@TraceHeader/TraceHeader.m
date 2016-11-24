classdef TraceHeader < Header
% Class for SEGY Trace Headers
%   Traceheaders contains the trace headers for the SEG-Y file.  These
%     headers are stored as uint8 type values.  The definition file
%     directs matlab how to interpret these values therefor it is
%     essential that getheadervalue and setheadervalue are used.
%
%  When TraceHeader is called filename must be entered to construct a
%    new TraceHeader object, other arguments can be entered as listed
%    below.
%
% Arguments that can be entered into traceheader are:
%   'trcdefinition', this will allow the traceheaders to be decoded using
%      a non-standard definitions file.  'trcdefinition' must be
%      accompanied by a *.csv filename
%   'hdroffset', this is the pointer to where the first traceheader is
%      located in the file. If extended text headers are present or the
%      file is a *.su this will need to be changed.  'hdroffset' must
%      be accompanied by the starting byte as a string for example:
%      '3600'
%   'extendedheaderflag', this is a flag that will test for extended
%      headers if the flag is accompanied by 'yes' then it will test 
%      for extended headers if accompanied by 'no', then it will not 
%      test for them
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
        traceoffsets=[];
        tracetype={4,'float'};
    end
    
    methods
        function obj = TraceHeader(file, varargin)
            obj=obj@Header(file,varargin{:});
            obj.hdrsize=240;
            obj.hdroffset=3600;
            extflag=1;
            limits=0;
            definition='segyrev1_trc.csv';
            new=0;
            obj.filefmt=obj.getfileendiantype4sgy(obj,3224);
            for i = 1:2:length(varargin)
                name = varargin{i};
                value = varargin{i+1};
                if strcmpi(name,'hdroffset');
                    obj.hdroffset=str2double(value);
                elseif strcmp(name,'extendedheaderflag');
                    if strcmp(value,'yes')
                        extflag=1;
                    else
                        extflag=0;
                    end
                elseif strcmpi(name,'limits')
                    if isnan(str2num(value))
                        me=MException('TraceHeader:InvalidInput',...
                            ['limits must be accompanied buy either ''[min:max]'' ,',...
                            'where min is the minimum trace number and max is the ',...
                            'maximum trace number. Or ''[n1 n2 n3 ...]'', where',...
                            'n1, n2 ... are trace numbers in the order you which',...
                            'they are displayed']);
                        throw(me)
                    end
                    limits=str2num(value);
                elseif strcmpi(name,'trcdefinitions')
                    definition=value;
                elseif strcmpi(name,'fileformat')
                    if strcmpi(value,'L');
                        obj.filefmt='L';
                    elseif strcmpi(value,'B');
                        obj.filefmt='B';
                    end
                elseif strcmpi(name,'new')
                    new=1;
                end
            end
            if (nargin>0)
                
                obj.definitions = HeaderDefinitions(definition);
                guessByteOrder(obj);
                
                if obj.hdroffset~=0
                    obj.tracetype=obj.gettracetype(obj);
                end
                if ~new
                    if obj.hdroffset~=0 && obj.hdroffset<=3600 && extflag
                        obj.hdroffset=obj.uitestforextendedheaders(obj);
                    end
                    
                    if any(limits)
                        if limits==1
                            obj=readonetrheader(obj);
                        else
                        obj=gettracepointers(obj);
                        obj=readlimitedHeader(obj,limits);
                        end
                    else
                        readHeader(obj);
                    end
                end
            end
        end
        
        function delete(obj)
            obj = obj.closeFile();
        end
        %[values colnames] = readHeaderDefinitions(filename);
        %this needs much work
    function objout=copy(obj)
    objout=copy2(obj);
    end
    end
    
    methods (Static)
        tracetype=gettracetype(obj,offset);
        
        
    end
    
end
