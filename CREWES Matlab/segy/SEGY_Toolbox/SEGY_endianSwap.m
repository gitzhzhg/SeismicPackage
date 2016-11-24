function obj=SEGY_endianSwap(obj)
% tracehead=SEGY_endianSwap(tracehead);
% binhead=SEGY_endianSwap(binhead);
% traces=SEGY_endianSwap(traces);
%
% SEGY_endianSwap will switch the endian ordering in the event that the 
%  data was read incorrectly.
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

if isa(obj,'Trace')
    words=obj.traceheader.definitions.values(:,strcmpi(obj.traceheader.definitions.keys(),'Name'));
    for k=1:length(words)
        vari=obj.traceheader.getheadervalue(words{k});
        vari=swapbytes(vari);
        obj.traceheader=obj.traceheader.setheadervalue(words{k},vari);
    end
    if strcmpi(obj.traceheader.filefmt,'L')
        obj.traceheader.filefmt='B';
        obj.traceheader.machineformat='ieee-be';
    else
        obj.traceheader.filefmt='L';
        obj.traceheader.machineformat='ieee-le';
    end
    obj.tracedata.data=swapbytes(single(obj.tracedata.data));
end

if isa(obj,'Header')
    words=obj.definitions.values(:,strcmpi(obj.definitions.keys(),'Name'));
    for k=1:length(words)
        vari=obj.getheadervalue(words{k});
        vari=swapbytes(vari);
        obj=obj.setheadervalue(words{k},vari);
    end
    if strcmpi(obj.traceheader.filefmt,'L')
        obj.filefmt='B';
        obj.machineformat='ieee-be';
    else
        obj.filefmt='L';
        obj.machineformat='ieee-le';
    end
end   


end