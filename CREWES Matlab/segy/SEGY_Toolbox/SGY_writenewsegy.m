function SGY_writenewsegy(fileout,texthead,binaryhead,traces,extendedhead)
% SGY_writenewsegy(fileout,texthead,binaryhead,traces,extendedhead)
% SGY_writenewsegy(fileout,texthead,binaryhead,traces)
%
% SGY_writenewsegy will write a new segy file according to segy revision 1
% standards.  
%
% Inputs:
%    fileout= the name of the new sgy file should end in .sgy
%    texthead= can be a 40 x 80 char array or a TextHeader object.
%    binaryhead= must be a BinaryHeader object.
%    traces= must be a cell array where: 
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
        me=MException('SGY_writesegy:InsufficientInput',...
            'You must enter at least fileout, texthead, binaryhead, and traces.');
        throw(me)
    end
    
    % create the objects using the new fileout name
    txthead=TextHeader(fileout,'permission','w+','machineformat','ieee-be');
    
    % add the header information to txthead and write to file
    txthead=loadtexthead(txthead,texthead);
    writeHeader(txthead);
    delete(txthead);
    
    % add the header information to binhead and write to file
    binhead=BinaryHeader(fileout,'permission','r+','machineformat','ieee-be');
    binhead=loadbinhead(binhead,binaryhead);
    % add number of extended header data
    if nargin>4
        if iscell(extendedhead)
            szext=length(extendedhead);
            setheadervalue(binhead,'netfh',szext);
        else
            setheadervalue(binhead,'netfh',1);
        end
    end
    % set format to 5 indicating that the traces are in the ieee format
    binhead=setheadervalue(binhead,'format',5);
    
    writeHeader(binhead);
    delete(binhead);
    trcheadoffset=3600;
    %test to see if there is an extended header
    if nargin>4
        % test to see if there is more than one extended header and
        if iscell(extendedhead)
            szext=length(extendedhead);
            for n=1:szext
                extinput=extendedhead{n};
                exthead=TextHeader(fileout,'permission','r+','txthoffset','3600','machineformat','ieee-be');
                exthead=loadtexthead(exthead,extinput);
                writeHeader(exthead);
                delete(exthead);
            end
            trcheadoffset=3600+n*3200;
        else
            exthead=TextHeader(fileout,'permission','r+','txthoffset','3600','machineformat','ieee-be');
            exthead=loadtexthead(exthead,extendedhead);
            writeHeader(exthead);
            delete(exthead);
            trcheadoffset=3600+3200;
        end
    end
    
%     trchead=Trace(fileout,'permission','r+','machineformat','ieee-be');
%     trchead=loadtraces(trchead,traces);
    writetraces(fileout,traces{1,1},traces{1,2},'ieee-be',num2str(trcheadoffset));
    %delete(trchead);
    
disp(['File: ',fileout,' has been written']);    
catch me
    error(me.message,me.identifier);
end

    function txthead=loadtexthead(txthead,texthead)
        if isa(texthead,'TextHeader')
            txthead.header=texthead.header;
        elseif ischar(texthead)
            txtsz=size(texthead);
            if (txtsz(1)*txtsz(2)~=3200)
                me=MException('SGY_writesegy:TextheaderSize',...
                    'texthead must be 3200 characters');
                throw(me)
            else
                txthead=set(txthead,'header',texthead);
            end
        else
            me=MException('SGY_writesegy:TextheaderFormat',...
                'texthead must be either a 40x80 character array or a TextHeader Object');
            throw(me)
        end
    end

    function binhead=loadbinhead(binhead,binaryhead)
        stndsz=binhead.hdrsize;
        if isa(binaryhead,'BinaryHeader')
            binhead.nontypecasthdr=binaryhead.nontypecasthdr;
        elseif isnumerical(binaryhead)
            binsz=length(binaryhead);
            if binsz~=stndsz;
                me=MException('SGY_writesegy:BinaryheaderSize',...
                    ['binaryhead must be ',num2str(stndsz),'x1 numerical vector']);
                throw(me)
            else
                binhead.nontypecasthdr=binaryhead;
            end
        else
            me=MException('SGY_writesegy:BinaryheaderFormat',...
                ['binaryhead must be either a ',num2str(stndsz),...
                'x1 numerical vector or a BinaryHeader Object']);
            throw(me)
        end
    end

    function trchead=loadtraces(trchead,traces)
        stndsz=trchead.hdrlength;
        if isa(traces,'Trace')
            trchead.nontypecasthdr=traces.nontypecasthdr;
            trchead.trace=traces.trace;
        elseif iscell(traces)
            trcsz=length(traces{1,1}(:,1));
            if trcsz~=stndsz;
                me=MException('SGY_writesegy:TraceheaderSize',...
                    ['traces must be a cell array where {1,1} is a ',num2str(stndsz),'x n numerical array']);
                throw(me)
            else
                trchead.nontypecasthdr=traces{1,1};
                trchead.trace=traces{1,2};
            end
        else
            me=MException('SGY_writesegy:TraceFormat',...
                ['traces must be either a cell array containing {traceheaders, tracedata}',...
                ' or a Traces Object']);
            throw(me)
        end
    end


end