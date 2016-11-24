function writetraces(fileout,traceheaders,traces,machineformat,traceoffset)
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

null=NaN;

try
if nargin <5
    traceoffset='3600';
end
if nargin <4
    machineformat='ieee-be';
end
szhead=size(traceheaders);
sztrac=size(traces);

if szhead(2)~=sztrac(2)
    me=MException('writetraces:SizeMismatch',...
        'There must be a traceheader entry for each trace.');
    throw(me);
end

trchnew=TraceHeader(fileout,'hdroffset',traceoffset,'machineformat',machineformat,...
    'permission','r+');

trchnew.nontypecasthdr=traceheaders;
fseek(trchnew.fid,trchnew.hdroffset,'bof');

for m=1:sztrac(2)
    
    %remove any NaNs from the trace and make sure ns is set to tracelength
    ntrace=traces(:,m);
    ntrace=ntrace(ntrace~=null);
    ns=getheadervalue(trchnew,'ns');
    ns(1,m)=length(ntrace);
    trchnew=setheadervalue(trchnew,'ns',ns);
    %write the trace header
    trchead=trchnew.nontypecasthdr(:,m);
    fwrite(trchnew.fid,trchead,'uint8');
    
    % write the trace data
    fwrite(trchnew.fid,ntrace,'float',0,trchnew.machineformat);
end

catch me
    errordlg(me.message, me.identifier);
end


end