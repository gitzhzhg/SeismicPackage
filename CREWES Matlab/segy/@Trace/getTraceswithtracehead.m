function obj=getTraceswithtracehead(obj,varargin)
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

tracehead=obj.traceheader;
traces=[];
traceh=[];
null=NaN;
tracebuff=Trace(copy(obj.traceheader));
traceheadflag=1;
if isempty(tracehead.nontypecasthdr);
    traceheadflag=0;
end
try
    % test if tracehead is a TracappnaneHeader Object
    if ~isa(tracehead,'TraceHeader');
        me=MException('getTraces:InvalidInputType',...
            ['tracehead MUST be a TraceHeader Object',...
            'Please Run getTraceHeader First']);
        throw(me)
    end
    % if varargin is a 1x1 cell expand
    if iscell(varargin) && all(size(varargin)==1);
        varargin=varargin{:};
    end
        
    % get trace offsets
    troffset=tracehead.traceoffsets;
    
    % find out what type of floating point 
    if strcmp(tracehead.tracetype{2},'unknown')
            tracehead.tracetype{2}=tracebuff.uigettracetype(tracebuff,1);
    end
    if strcmp(tracehead.tracetype{2},'ieee')
            tracehead.tracetype{2}='float32';
    end
    % test what fields the user has inputed and get the values
    
    if nargin>1
        if isodd(length(varargin))
            me=MException('getTraces:insufficentinput',...
                'Input must be entered in pairs.  ');
            throw(me);
        end
        name=cell(length(varargin)/2,1);
        value=cell(length(varargin)/2,1);
        m=1;
        for i = 1:2:length(varargin)
            name{m,1} = lower(varargin{i});
            if ~ischar(name{m,1})
                if isa(name,'Trace')
                    break
                end
               me=MException('getTraces:InputIsWrongDataType',...
                'Searching parameters must be strings.');
            throw(me); 
            end
            value{m,1}=varargin{i+1};
            if ~isnumeric(value{m,1})
               me=MException('getTraces:InputIsWrongDataType',...
                'Searching parameters must be accompanied by numeric values.');
            throw(me); 
            end
            
            m=m+1;
        end
    end
    namepos=strcmpi(tracehead.definitions.keys(),'Name');
    fldnames=tracehead.definitions.values(:,namepos);
    %find traces that satisfy name, values
    if nargin>1
        numofth=length(getheadervalue(tracehead,fldnames{2,1}));
        masterind= true(length(name),numofth);
        for i=1:length(name)
            if strcmp(name{i,1},'tracenumber')
                masterind(i,:)= false(1,numofth);
                masterind(i,value{i,1})=1;
            else
                defpos=find(strcmp(fldnames,name{i,1}));
                if defpos
                    vals=value{i,1};
                    trval=getheadervalue(tracehead,fldnames{defpos,1});
                    ind= false(length(vals),numofth);
                    for n=1:length(vals)
                        ind(n,:)=trval==vals(n);
                    end
                    if n>1
                        ind=logical(sum(ind));ind(ind)=1;
                    end
                    masterind(i,:)=ind;
                end
            end
        end
        if isempty(varargin)
        tracenum=(1:length(troffset)-1);
        else
        tracenum=ones(1,length(masterind));
        szmast=size(masterind);
        for n=1:szmast(1)
            tracenum=tracenum.*masterind(n,:);
        end
        tracenum=find(tracenum==1);
        end
    else
        tracenum=(1:length(troffset)-1);
    end
    
    % Preallocate traces and traceh matrix
    tracelength=(troffset(end)-troffset(end-1)-tracehead.hdrsize)/tracehead.tracetype{1};
    traces=null*ones(tracelength,length(tracenum));
    traceh=uint8(ones(tracehead.hdrsize,length(tracenum)));
    hwait=waitbar(0,'Please Wait as Traces are Loaded');
    
    
    for j=1:length(tracenum)
        
        k=tracenum(j);
           % Extract Trace Header Data from TraceHeader Object if the
           % traceheader object only contains pointers then read in the
           % header information
        if traceheadflag
        fseek(tracehead.fid,troffset(k)+tracehead.hdrsize,'bof');
        traceh(:,j)=tracehead.nontypecasthdr(:,k);
        else
        fseek(tracehead.fid,troffset(k),'bof');
        traceh(:,j)=fread(tracehead.fid,tracehead.hdrsize,'*uint8');
        end
        % Read in the traces
        
        tracelength=double((troffset(k+1)-troffset(k)-tracehead.hdrsize)/tracehead.tracetype{1});
        if ~fpequal(tracelength,length(traces(:,j)),100)
            appnan=null*ones((tracelength-length(traces(:,j))),length(tracenum));
            traces=[traces;appnan];
        end
        if strcmp(tracehead.tracetype{2},'ibm')
            trace1=fread(tracehead.fid,tracelength,'*uint32');
            trace1=checkforrightbyteorder(trace1,tracehead.filefmt);
             trace1 = ibm2ieee (trace1);
             traces(1:length(trace1),j)=trace1;
        else
        trace1=fread(tracehead.fid,tracelength,tracehead.tracetype{2},0,tracehead.machineformat);
        %trace1=checkforrightbyteorder(trace1,tracehead.filefmt);
        traces(1:length(trace1),j)=trace1;
        end
     
        waitbar(j/length(tracenum),hwait,...
            ['Please Wait as Traces are Loaded ',num2str(j),' of ',num2str(length(tracenum))]);
        
    end
 obj.tracedata.data=traces;
 obj.traceheader.nontypecasthdr=traceh;  
   
    
catch me
    error(me.message)
end

delete(hwait);
end