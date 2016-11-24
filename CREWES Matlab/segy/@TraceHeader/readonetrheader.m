function obj = readonetrheader ( obj)
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
    fseek(obj.fid,obj.hdroffset,'bof');
    val=fread(obj.fid,[obj.hdrsize,1] ,'*uint8');
    obj.nontypecasthdr=val;
    ns=obj.getheadervalue('ns');
    tracebytes=ns*obj.tracetype{1}+obj.hdrsize+obj.hdroffset;
    obj.traceoffsets=[obj.hdroffset,tracebytes];
    
    trace=fread(obj.fid,[ns*obj.tracetype{1},1] ,'*uint8');
    obj2=copy(obj);
    while ~any(trace)
        tb=obj2.traceoffsets(1);
        val=fread(obj.fid,[obj.hdrsize,1] ,'*uint8');
        obj2.nontypecasthdr=val;
        ns=obj2.getheadervalue('ns');
        obj2.traceoffsets=[tb,ns*obj.tracetype{1}+obj.hdrsize+tb];
        trace=fread(obj.fid,[ns*obj2.tracetype{1},1] ,'*uint8');
    end
    tracebuff=Trace(obj2);
    obj.tracetype{2}=tracebuff.uigettracetype(tracebuff,1);
    
    
catch me
    error (me.message);
end
end