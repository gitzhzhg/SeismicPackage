function writeTrace( obj )
%
%function writeTrace ( obj )
% writes the traceheaders and tracedata in a trace object
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
null=NaN;
try
    %make sure we're at the start of the header in the file
    fseek(obj.traceheader.fid,obj.traceheader.hdroffset,'bof');
sz=size(obj.traceheader.nontypecasthdr);
% test to see if any nulls are in the data
if any(any(obj.tracedata.data==null))
hwait=waitbar(0,'Please Wait as Traces are Prepared for Writing');
for m=1:sz(2)
    
    %remove any null from the trace and make sure ns is set to tracelength
    ntrace=obj.tracedata.data(:,m);
    if isnan(null)
        ntrace=ntrace(~isnan(ntrace));
    else
    ntrace=ntrace(~(ntrace==null));
    end
    ns=obj.traceheader.getheadervalue('ns');
    ns(1,m)=length(ntrace);
    obj.traceheader=obj.traceheader.setheadervalue('ns',ns);
     if any(m==1:50:sz(2))
        waitbar(m/sz(2),hwait,[num2str(m),' of ',...
            num2str(sz(2)),' Traces have been Prepared for Writing. ']);
    end
end
delete(hwait)
else
    ns=size(obj.tracedata.data);
    ns=ns(1)*ones(ns(2),1);
    obj.traceheader=obj.traceheader.setheadervalue('ns',ns);
end
% swap bytes in header if nessasary
if isempty(strfind(lower(obj.traceheader.filefmt),lower(obj.traceheader.machineformat)));
    words=obj.traceheader.definitions.values(:,strcmpi(obj.traceheader.definitions.keys(),'Name'));
    for k=1:length(words)
        vari=obj.traceheader.getheadervalue(words{k},0);
        vari=swapbytes(vari);
        obj.traceheader=obj.traceheader.setheadervalue(words{k},vari,0);
    end
end

hwait=waitbar(0,'Please Wait as Traces are Written to the File');
for m=1:sz(2)
    
    %remove any null from the trace and make sure ns is set to tracelength
    ntrace=obj.tracedata.data(:,m);
    if isnan(null)
        ntrace=ntrace(~isnan(ntrace));
    else
    ntrace=ntrace(~(ntrace==null));
    end
    
    %write the trace header
    trchead=obj.traceheader.nontypecasthdr(:,m);
    fwrite(obj.traceheader.fid,trchead,'uint8',0,obj.traceheader.machineformat);
    
    % write the trace data
    fwrite(obj.traceheader.fid,ntrace,'float',0,obj.traceheader.machineformat);
    
    if any(m==1:50:sz(2))
        waitbar(m/sz(2),hwait,[num2str(m),' of ',...
            num2str(sz(2)),' Traces have been Written to the File. ']);
    end
end

catch me
    delete(hwait);
    error (me.message);
end
delete(hwait);

end