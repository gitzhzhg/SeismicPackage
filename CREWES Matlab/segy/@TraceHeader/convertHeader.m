function obj = convertHeader ( obj )
%
%function obj = convertHeader ( obj )
%
% Refomats the header information as defined by HeaderDefinitions
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
    %size of definitions
    sz=size(obj.definitions.values);
    numoftr=size(obj.nontypecasthdr);
    hwait=waitbar(0,'Please Wait as Trace Headers are Converted');
    % loop to typecast unformatted traceheaders
    
    for k=1:sz(1)
        st=str2double(obj.definitions.values(k,strcmp(obj.definitions.keys,'startByte')));
        ed=str2double(obj.definitions.values(k,strcmp(obj.definitions.keys,'endByte')));
        typ=obj.definitions.values{k,strcmp(obj.definitions.keys,'Type')};
        ibmflag=false;
        if strfind(typ,'ieee')
            typ='float32';
        elseif strfind(typ,'ibm')
            typ='uint32';
            ibmflag=true;
        end
        val=eval([typ,'(zeros(1,','numoftr(2)))']);    
        for m=1:numoftr(2)
            val(1,m)=typecast(obj.nontypecasthdr(st:ed,m)',typ);
        end
        val=checkforrightbyteorder(val,obj.filefmt);
        if ibmflag
            val=ibm2ieee(val);
        end
        header.(obj.definitions.values{k,1})=val;
        %Adjust waitbar
        waitbar(k/sz(1));
        
    end

% set the object header property
obj.header=header;

catch me
    error (me.message);
end
delete(hwait);
end