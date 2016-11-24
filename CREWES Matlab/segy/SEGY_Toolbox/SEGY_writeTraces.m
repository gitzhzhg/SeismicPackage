function SEGY_writeTraces(fileout,traces,numcurtraces,numcurshots)
% SEGY_writeTraces(fileout,traces,numcurtraces,numcurshots)
% SEGY_writeTraces(fileout,{tracehead, tracedata},numcurtraces,numcurshots)
%
% SEGY_writeTraces will append the traceheaders and traces to the end of 
%    the file according to segy revision 1 standards.  To write the text 
%    header, binary header, and any extended headers to a SEG-Y file use
%    SEGY_writeHeaders first.  To write the entire file at once use
%    SEGY_write.
%
% Inputs:
%    fileout= the name of the new sgy file should end in .sgy
%    traces= can be a Trace object or a cell array where: 
%      {1,1}= is a matrix of traceheader information.  This is a 
%          (240 x number of traces) of type uint8. % this can be made using
%          AFD_makeSEGYtraceheaders
%      {1,2}= is a matrix of traces.  This is a (tracelength x number of traces)
%           of 4-byte floats.
%    numcurtraces= this is a numeric value that depicts the number of
%      traces that are currently in the file.  This is required for SEG-Y
%      Revision 1 Standards.
%    numcurshots= this is a numeric value that depicts the number of shots
%      that are currently in the file.  This is optional but can be helpful
%      when retrieving data.
%
%
% Heather Lloyd 2010, Kevin Hall 2009, Chad Hogan 2004
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
    if nargin<3
        me=MException('SEGY_write:InsufficientInput',...
            'You must enter at least fileout, traces, and numcurtraces.');
        throw(me)
    end
% create a blank Trace object
obj=Trace(fileout,'machineformat','ieee-be','permission','r+','New','1');
% load data to Trace Object
obj=loadtraces(obj,traces);
% set file flags
fseek(obj.traceheader.fid,0,'eof');
obj.traceheader.hdroffset=ftell(obj.traceheader.fid);

% Update traceheaders
tracr=1:length(obj.tracedata.data(1,:));
SEGY_setHeader(obj,'tracr',tracr);
tracl=tracr+numcurtraces;
SEGY_setHeader(obj,'tracl',tracl);
if nargin>3
    ep=numcurshots+ones(size(tracr));
    SEGY_setHeader(obj,'ep',ep);
end

% write the traces
obj.writetrace();
    disp(['Traces have been written to File: ',fileout]);
catch me
    error(me.message,me.identifier);
end


    function trchead=loadtraces(trchead,traces)
        if isa(traces,'Trace')
            szh=size(traces.traceheader.nontypecasthdr);
            szt=size(traces.tracedata.data);
            if szh(2)~=szt(2)
                me=MException('SEGY_writeTraces:InvalidSize',...
                    'There must be one column of TraceHeaders for every trace.');
                throw(me)
            end
            trchead.traceheader.nontypecasthdr=traces.traceheader.nontypecasthdr;
            trchead.tracedata.data=traces.tracedata.data;
            trchead.traceheader.filefmt=traces.traceheader.filefmt;
        elseif iscell(traces)
            trcsz=length(traces{1,1}(:,1));
            names=trchead.traceheader.definitions.values(:,strcmpi(trchead.traceheader.definitions.keys(),'Name'));
            stndsz=length(names);
            if trcsz~=stndsz;
                me=MException('SEGY_write:TraceheaderSize',...
                    ['traces must be a cell array where {1,1} is a ',num2str(stndsz),...
                    'x n numerical array','and {1,2} is a tracelength x n matrix of tracedata']);
                throw(me)
            else
                szh=size(traces{1,1});
            szt=size(traces{1,2});
            if szh(2)~=szt(2)
                me=MException('SEGY_writeTraces:InvalidSize',...
                    'There must be one column of TraceHeaders for every trace.');
                throw(me)
            end
            trchead.traceheader.nontypecasthdr=uint8(zeros(trchead.traceheader.hdrsize,szh(2)));
                th=traces{1,1};
                for k=1:stndsz
                trchead.traceheader.setheadervalue(names{k,1},th(k,:),0);
                end
                trchead.tracedata.data=traces{1,2};
                [u1,u2,trchead.traceheader.filefmt]=computer();
            end
        else
            me=MException('SEGY_write:TraceFormat',...
                ['traces must be either a cell array containing {traceheaders, tracedata}',...
                ' or a Trace Object']);
            throw(me)
        end
    end


end