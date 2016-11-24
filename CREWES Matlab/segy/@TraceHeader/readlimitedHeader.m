function obj = readlimitedHeader ( obj, limits)
%
%function obj = readHeader ( obj )
%
% Read selected trace headers from a SEG-Y or SU file
% Input:
%   obj=TraceHeader object
%   limits= a vector containing either [min:max] where min is the minimum
%               trace number and max is the maximum trace number.
%           Or [n1 n2 n3 ...], where n1, n2 ... are trace numbers in the
%               order you which they are displayed
%
% Returns:
%   nontypecasthdr = a unsigned int8 array contining the header information
%           which can be converted usingconvertHeader or getHeader
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
    troffset=obj.traceoffsets;
    val=1;
    sz=size(limits);
    % check for enough memory
    [user, sys] = memory;
            MemMaxBytes = sys.PhysicalMemory.Available;
            mryflag=((obj.hdrsize*sz(2))<(0.1*MemMaxBytes));
    if mryflag
        nontypecastheader=uint8(zeros(obj.hdrsize, sz(2)));
    else
        warndlg('Not Enough Memory to Read Trace Headers')
        return
    end
    hwait=waitbar(0/length(limits),'Please Wait as Trace Headers Load');
    
    
    %read trace header values
    for m=1:sz(2)
        % read in traceheader as nusigned byte intergers
        fseek(obj.fid,troffset(limits(m)),'bof');
        val=fread(obj.fid,[obj.hdrsize,1] ,'*uint8');
        if isempty(val)
            break
        end
        nontypecastheader(:,m)=val;
        
    end
    
    % set the object header property
    obj.nontypecasthdr=nontypecastheader;
catch me
    error (me.message);
end
delete(hwait);
end