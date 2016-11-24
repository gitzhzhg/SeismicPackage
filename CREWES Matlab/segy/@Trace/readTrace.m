function obj=readTrace(obj,varargin)
%
%function obj = readHeader ( obj )
%
% Read the trace headers from a SEG-Y or SU file
% Returns:
%   header = a structure array containing the trace header information.
%     Each structure heading is a variable defined in headerdefininitions
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

try
    %position pointer at start of header
    fseek(obj.fid,obj.traceoffset,'bof');
    %get size of traceheader definitions
    sz=size(obj.definitions.values);
    %create structure with traceheader definintion names
    nsind=0;
    tps=cell(sz(1),1);
    for k=1:sz(1)
        ts=obj.definitions.values{k,4};
        if strcmp(ts(1,1),'u')
            tps{k,1}=ts;
        elseif strcmp(ts(1,1),'c')
            tps{k,1}=ts;
        else
            tps{k,1}=['u',ts];
        end
        if strcmp(obj.definitions.values{k,1},'ns')
            nsind=k;
        end
    end
    %create variables to hold assorted things
    nsforall=[];
    val=1;
    trace=[];
    traces=[];
    header=uint32(ones(sz(1),1));
    %create waitbar and associated variables.
    count=0;
    fileinfo=dir(obj.filename);
    totcount=fileinfo.bytes/2000;
    hwait=waitbar(count/totcount,'Please Wait as Traces Loaded');
    %read trace header values
    m=1;  % is the number of traces that have been read in
    while(~isempty(val))
       for k=1:sz(1)
            ln=str2double(obj.definitions.values{k,3})-str2double(obj.definitions.values{k,2})+1;
            typ=obj.definitions.values{k,4};
            if findstr(typ,'32')
                ln=ln/4;
            elseif findstr(typ,'16')
                ln=ln/2;
            elseif findstr(typ,'8')
                ln=ln/1;
            end
            val=fread(obj.fid, ln ,['*',typ]);
            if isempty(val)
                break
            end
            header(k,m)=val;
            
        end
        if isempty(val)
            break
        end
        if nsind
            ns=header(nsind,m);
        end
        
        % ask user to imput number of samples if trace header does not exist
        if ~nsind || ns==0 && isempty(nsforall)
            ns=str2double(inputdlg({['The number of samples in the trace was not found.',...
                'Please enter the number of samples in the trace.']},...
                'Number of Samples NOT FOUND',1,{'500'}));
            nsforall=questdlg(['You have entered ',num2str(ns),' as the number of samples in this trace.  ',...
                'Would you like to use this value for all traces in the file?'],...
                'Constant Number of Samples','Yes','No','Yes');
            if strcmp(nsforall,'No'),nsforall=[];else nsforall=ns;end;
        end
        
        if ~isempty(nsforall), ns=nsforall; end
        
        
        trace=fread(obj.fid,ns,'float');
        matszt=size(traces);
        if matszt(1)<length(trace);
            traces=[traces;NaN((length(trace)-matszt(1)),matszt(2))];
        end
        traces(:,m)=trace;
        
        %Adjust waitbar
        totcount=ceil(double((fileinfo.bytes-obj.traceoffset)/(obj.hdrlength+ns*4)));
        count=count+1;
        waitbar(count/totcount);
        
        % See if additional space needs to be added to preallocate for
        % speed
        matszh=size(header);
        if matszh(2)<totcount;
            header=[header,NaN(matszh(1),(totcount-matszh(2)))];
        end
        matszt=size(traces);
        if matszt(2)<totcount;
            traces=[traces,NaN(matszt(1),(totcount-matszt(2)))];
        end
        
        % add one to m for the times it has gone arround the while loop
         m=m+1;
    end
    
    % remove additional entries
    header=header(:,1:m-1);
    traces=traces(:,1:m-1);
    
    
    % set the object header property
    obj.header=header;
    obj.trace=traces;
catch me
    error (me.message);
end
delete(hwait);
end