function obj=getTracesfromfile(obj,varargin)
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

traces=[];
null=NaN;
traceheadbuff=obj.traceheader;
j=0;
tracebuff=Trace(obj.traceheader,'new','1');
try
    % test if tracehead is a TraceHeader Object
    if ~isa(obj,'Trace');
        me=MException('getTraces:InvalidInputType',...
            'obj MUST be a Trace Object');
        throw(me)
    end
    
    % organize inputs so they can be tested easily
    if nargin>1
        if iscell(varargin{1})
            varargin=varargin{:};
        end
        if isodd(length(varargin))
            me=MException('getTraces:insufficentinput',...
                'Input must be entered in pairs.  ');
            throw(me);
        end
        name=cell(1,length(varargin)/2);
        value=cell(1,length(varargin)/2);
        m=1;
        for i = 1:2:length(varargin)
            name{1,m} = lower(varargin{i});
            if ~ischar(name{1,m})
                me=MException('getTraces:InputIsWrongDataType',...
                    'Searching parameters must be strings.');
                throw(me);
            end
            value{1,m}=varargin{i+1};
            if ~isnumeric(value{1,m})
                me=MException('getTraces:InputIsWrongDataType',...
                    'Searching parameters must be accompanied by numeric values.');
                throw(me);
            end
            
            m=m+1;
        end
    end
    
    
    %start reading file
    fseek(obj.traceheader.fid,obj.traceheader.hdroffset,'bof');
    %get size of traceheader definitions
    sz=size(obj.traceheader.definitions.values);
    %create variables to hold assorted things
    nsforall=[];
    val=1;
    % test to find where ns (number of samples is located);
    nsind=0;
    nspos=find(strcmp(obj.traceheader.definitions.values(:,strcmp(obj.traceheader.definitions.keys,'Name')),'ns'));
    if nspos
        nsind=nspos;
    end
    troffset=obj.traceheader.hdroffset;
    %create waitbar and associated variables.
    nontypecastheader=uint8([]);
    count=0;
    fileinfo=dir(obj.traceheader.filename);
    totcount=fileinfo.bytes/2000;
    hwait=waitbar(count/totcount,'Please Wait as Trace Headers Load','CreateCancelBtn',@stopearly);
    %read trace header values
    m=1;  % is the number of traces that have been read in
    while(~isempty(val))
        % read in traceheader as nusigned byte intergers
        fseek(obj.traceheader.fid,troffset(end),'bof');
        val=fread(obj.traceheader.fid,[obj.traceheader.hdrsize,1] ,'*uint8');
        if isempty(val)
            break
        end
        traceheadbuff.nontypecasthdr=val;
        
        % get ns from header using definitions
        if nsind
            ns=traceheadbuff.getheadervalue('ns');
        end
        
        
        % ask user to imput number of samples if ns does not exist
        if ~nsind || ns==0 && isempty(nsforall)
            ns=str2double(inputdlg({['The number of samples in the trace was not found.',...
                'Please enter the number of samples in the trace.']},...
                'Number of Samples NOT FOUND',1,{'500'}));
            if isempty(ns)
                warndlg('Invalid Number Of Samples Entered, Exiting Script')
                delete(hwait);
                return
            end
            nsforall=questdlg(['You have entered ',num2str(ns),' as the number of samples in this trace.  ',...
                'Would you like to use this value for all traces in the file?'],...
                'Constant Number of Samples','Yes','No','Yes');
            if strcmp(nsforall,'No'),nsforall=[];else nsforall=ns;end;
        end
        
        if ~isempty(nsforall), ns=nsforall; end
        
        % make sure we are not past the end of file
        if (ftell(obj.traceheader.fid)+(ns*obj.traceheader.tracetype{1}))>fileinfo.bytes
            fseek(obj.traceheader.fid,0,'eof');
        else
            
            % test to see if trace meets search parameters
            if nargin>1
            readind=false(size(name));
            for k=1:length(name)
                if strcmpi(name,'tracenumber')
                    readind(1,k)=any(m==value{1,k});
                else
                vari=traceheadbuff.getheadervalue(name{1,k});
                readind(1,k)=any(vari==value{1,k});
                end
            end
            else
                readind=1;
            end
            % if we meet all parameters we need to see what type of trace it is
            if all(readind)
                j=j+1;
                nontypecastheader(:,j)=traceheadbuff.nontypecasthdr;
                if strcmp(obj.traceheader.tracetype{2},'unknown')
                    traceheadbuff.traceoffsets=[troffset(end)+traceheadbuff.hdrsize,troffset(end)+traceheadbuff.hdrsize+ns*traceheadbuff.tracetype{1}];
                    tracebuff.traceheader=traceheadbuff;
                    [obj.traceheader.tracetype{2},obj.traceheader.tracetype{1},obj.traceheader.filefmt]=tracebuff.uigettracetype(tracebuff,1);
                    if strcmpi(obj.traceheader.filefmt,'b');
                        obj.traceheader.machineformat='ieee-be';
                    else
                        obj.traceheader.machineformat='ieee-le';
                    end
                    if j==1
                    fseek(obj.traceheader.fid,troffset(end)+obj.traceheader.hdrsize,'bof');
                    end
                end
                
                if strcmp(obj.traceheader.tracetype{2},'ieee')
                    obj.traceheader.tracetype{2}='float32';
                end
                
                tracelength=double(ns);
                % extend the matrix if file is variable trace length
                if isempty(traces)
                    traces=zeros(tracelength,1);
                end
                trcsz=size(traces);
                if ~fpequal(tracelength,trcsz(1),100)
                    appnan=null*ones((tracelength-trcsz(1)),trcsz(2));
                    traces=[traces;appnan];
                end
                % if ibm decode accordingly else read it in normally
                if strcmp(obj.traceheader.tracetype{2},'ibm')
                    trace1=fread(obj.traceheader.fid,tracelength,'*uint32');
                    trace1=checkforrightbyteorder(trace1,obj.traceheader.filefmt);
                    trace1 = ibm2ieee (trace1);
                    traces(1:length(trace1),j)=trace1;
                else
                    trace1=fread(obj.traceheader.fid,tracelength,obj.traceheader.tracetype{2},0,obj.traceheader.machineformat);
                    %trace1=checkforrightbyteorder(trace1,tracehead.filefmt);
                    traces(1:length(trace1),j)=trace1;
                end
                
            else
                % if it doesnt satisfy all parameters skip over it.
                fseek(obj.traceheader.fid,ns*obj.traceheader.tracetype{1},'cof');
            end
            troffset(1,m+1)=troffset(m)+double(240+double(ns)*obj.traceheader.tracetype{1});
            
            %Adjust waitbar
            count=count+1;
            if any(count==(1:floor(0.01*totcount)*10:totcount));
                totcount=ceil(double((fileinfo.bytes-obj.traceheader.hdroffset))/double((obj.traceheader.hdrsize+ns*obj.traceheader.tracetype{1})));
                waitbar(count/totcount,hwait,...
            ['Please Wait as Traces are Loaded ',num2str(count),' of ',num2str(totcount)]);
            end
            
            % See if additional space needs to be added to preallocate for
            % speed
%             matsz=size(nontypecastheader);
%             if matsz(2)<totcount;
%                 mryflag=obj.traceheader.memoryallowancecheck([matsz(1) totcount],'uint8');
%                 if mryflag
%                     adnan=NaN(matsz(1),(totcount-matsz(2)));
%                     nontypecastheader=[nontypecastheader,adnan];
%                     troffset=[troffset,NaN(1,(totcount-matsz(2)))];
%                 else
%                     warndlg('Not Enough Memory to Read Trace Headers')
%                     delete(hwait);
%                     return
%                 end
%             end
            
            % add one to m for the times it has gone arround the while loop
            m=m+1;
        end
        
        
    end
    
    % remove additional entries
        nontypecastheader= nontypecastheader(:,1:j);
        troffset=troffset(:,1:m);
        
        
        % set the object header property
        obj.traceheader.traceoffsets=troffset;
        obj.traceheader.nontypecasthdr=nontypecastheader;
        obj.tracedata.data=traces;
        
catch me
    delete(hwait);
    error (me.message);
end
delete(hwait);
    function stopearly(hobject,eventdata)
         % set last offset to the end of file
        fseek(obj.traceheader.fid,0,'eof');
        troffset(end)=ftell(obj.traceheader.fid);
        return
    end
end