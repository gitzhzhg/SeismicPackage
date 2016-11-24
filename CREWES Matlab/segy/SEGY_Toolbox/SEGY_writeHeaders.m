function SEGY_writeHeaders(fileout,texthead,binaryhead,extendedhead,permission)
% SEGY_writeHeaders(fileout,texthead,binaryhead,extendedhead)
% SEGY_writeHeaders(fileout,texthead,binaryhead)
%
% SEGY_writeHeaders will write the text header, binary header, and any 
%    extended headers to a SEG-Y file according to SEG-Y revision 1 
%    standards. To write traces to a file use SEGY_writeTraces.  To write
%    the entire file at once use SEGY_write.
%
% Inputs:
%    fileout= the name of the new sgy file should end in .sgy
%    texthead= can be a 40 x 80 char array or a TextHeader object.
%    binaryhead= can be a numerical array that satisfies SEG-Y Revision 1 
%      standards or a BinaryHeader object.
%    extendedhead= is optional. This should be either a 40 x 80 char array
%      or a TextHeader object.  If multiple extended headers are required
%      extendedhead can be a cell array containing multiple char arrays or
%      multiple TextHeader object.
%    permission= if you want to just rewrite the headers then
%      permission='r+' else if you want to write over an old file or create
%      a new one use permission='w+'.  'w+' is the default.
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

% try
    
    if nargin<5
        prms='w+';
    else
        prms=permission;
    end
    if nargin<3
        me=MException('SEGY_writeHeaders:InsufficientInput',...
            'You must enter at least fileout, texthead, and binaryhead.');
        throw(me)
    end
% create a blank segy file
segyobj=SegyFile(fileout,'machineformat','ieee-be','permission',prms,'New','1');

% load approriate data to segyobj 
segyobj.textheader=loadtexthead(segyobj.textheader,texthead);
segyobj.binaryheader=loadbinhead(segyobj.binaryheader,binaryhead);
    if nargin > 4
        for m=1:length(extendedhead)
            offset=(3600+((m-1)*3200));
            extin=TextHeader(fileout,'machineformat','ieee-be','permission',prms,'New','1','txthoffset',num2str(offset));
            segyobj.extendedheader{1,m}=loadtexthead(extin,extendedhead{1,m});
        end     
    end
segyobj.segywriteall();
    disp(['SEGY file headers have been written to File: ',fileout]);
% catch me
%     error(me.message,me.identifier);
% end

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
                [~,~,binhead.filefmt]=computer();
            end
        else
            me=MException('SEGY_write:BinaryheaderFormat',...
                ['binaryhead must be either a ',num2str(stndsz),...
                'x1 numerical vector or a BinaryHeader Object']);
            throw(me)
        end
    end
end