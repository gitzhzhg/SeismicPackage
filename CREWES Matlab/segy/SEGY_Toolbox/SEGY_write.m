function SEGY_write(fileout,texthead,binaryhead,traces,extendedhead)
% SEGY_write(fileout,texthead,binaryhead,traces,extendedhead)
% SEGY_write(fileout,texthead,binaryhead,traces)
%
% SEGY_write will write a new segy file according to segy revision 1
% standards.  
%
% Inputs:
%    fileout= the name of the new sgy file should end in .sgy
%    texthead= can be a 40 x 80 char array or a TextHeader object.
%    binaryhead= can be a numerical array that satisfies SEG-Y Revision 1 
%      standards or a BinaryHeader object.
%    traces= can be a Trace object or a cell array where: 
%      {1,1}= is a matrix of traceheader information.  This is a 
%          (240 x number of traces) of type uint8.
%      {1,2}= is a matrix of traces.  This is a (tracelength x number of traces)
%           of 4-byte floats.
%    extendedhead= is optional. This should be either a 40 x 80 char array
%      or a TextHeader object.  If multiple extended headers are required
%      extendedhead can be a cell array containing multiple char arrays or
%      multiple TextHeader object.
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
    if nargin<4
        me=MException('SEGY_write:InsufficientInput',...
            'You must enter at least fileout, texthead, binaryhead, and traces.');
        throw(me)
    end
    % create a blank segy file
    segyobj=SegyFile(fileout,'machineformat','ieee-be','permission','w+','New','1');
    
    % load approriate data to segyobj
    segyobj.textheader=loadtexthead(segyobj.textheader,texthead);
    segyobj.binaryheader=loadbinhead(segyobj.binaryheader,binaryhead);
    segyobj.trace=loadtraces(segyobj.trace,traces);
    
    if nargin > 4
        offset=400;
        for m=1:length(extendedhead)
            offset=(3600+((m-1)*3200));
            extin=TextHeader(fileout,'machineformat','ieee-be','permission','w+','New','1','txthoffset',num2str(offset));
            segyobj.extendedheader{1,m}=loadtexthead(extin,extendedhead{1,m});
        end
        segyobj.trace.traceheader.hdroffset=offset+3200;
    end
    segyobj.segywriteall();
    disp(['File: ',fileout,' has been written']);
catch me
    error(me.message,me.identifier);
end

    function txthead=loadtexthead(txthead,texthead)
        if isa(texthead,'TextHeader')
            txthead.header=texthead.header;
            txthead.format=texthead.format;
        elseif ischar(texthead)
            txtsz=size(texthead);
            if (txtsz(1)*txtsz(2)~=3200)
                me=MException('SEGY_write:TextheaderSize',...
                    'texthead must be 3200 characters');
                throw(me)
            else
                txthead.header=texthead;
            end
        else
            me=MException('SEGY_write:TextheaderFormat',...
                'texthead must be either a 40x80 character array or a TextHeader Object');
            throw(me)
        end
    end

    function binhead=loadbinhead(binhead,binaryhead)
        names=binhead.definitions.values(:,strcmpi(binhead.definitions.keys(),'Name'));
            stndsz=length(names);
        if isa(binaryhead,'BinaryHeader')
            binhead.nontypecasthdr=binaryhead.nontypecasthdr;
            binhead.filefmt=binaryhead.filefmt;
        elseif isnumeric(binaryhead)
            binsz=length(binaryhead);
            if binsz~=stndsz;
                me=MException('SEGY_write:BinaryheaderSize',...
                    ['binaryhead must be ',num2str(stndsz),'x1 numerical vector']);
                throw(me)
            else
                
                for k=1:stndsz
                binhead.setheadervalue(names{k,1},binaryhead(k));
                end
                [useless,useless,binhead.filefmt]=computer();
            end
        else
            me=MException('SEGY_write:BinaryheaderFormat',...
                ['binaryhead must be either a ',num2str(stndsz),...
                'x1 numerical vector or a BinaryHeader Object']);
            throw(me)
        end
    end

    function trchead=loadtraces(trchead,traces)
        if isa(traces,'Trace')
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
                th=traces{1,1};
                datasz=size(traces{1,2});
                trchead.traceheader.nontypecasthdr=uint8(ones(trchead.traceheader.hdrsize,datasz(2)));
                for k=1:stndsz
                trchead.traceheader.setheadervalue(names{k,1},th(k,:));
                end
                trchead.tracedata.data=traces{1,2};
                [useless,useless,trchead.traceheader.filefmt]=computer();
            end
        else
            me=MException('SEGY_write:TraceFormat',...
                ['traces must be either a cell array containing {traceheaders, tracedata}',...
                ' or a Trace Object']);
            throw(me)
        end
    end


end