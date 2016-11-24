function newobj=standardizeheader(obj)
% headerobj=standardizeheader(headerobj)
%
% standardizeheader is a function that will convert a non standard header
% to the standard segy header as defined in revision 1.  This function uses
% a column named SEGY_Rev1_EQ, in the non standard header *.csv file that
% has the name of the standard variables used in revision 1, to map where
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

% the values are located in the standard header.
try
if isa(obj,'TraceHeader') || isa(obj,'BinaryHeader') || isa(obj,'Trace')
else
    me=MException('standardizeheader:InvalidInputType',...
        'obj must be a TraceHeader or BinaryHeader object');
    throw(me)
end

newobj=obj;


% set the newobj definitions to the standard segy definitions
    if isa(obj,'TraceHeader')
        newobj.definitions=HeaderDefinitions('segyrev1_trc.csv');  
    end
    if isa(obj,'Trace')
        newobj=obj.traceheader;
        newobj.definitions=HeaderDefinitions('segyrev1_trc.csv');  
    end
    if isa(obj,'BinaryHeader')
        newobj.definitions=HeaderDefinitions('segyrev1_file.csv');
    end
    
    
% create a new header filled with zeros
szint8hdr=size(newobj.nontypecasthdr);
newobj.nontypecasthdr=uint8(zeros(szint8hdr));
newobj.header=[];

% get name of variables and the name for the segy equivalent
    indname=strcmpi(obj.definitions.keys,'Name');
    indeq=strcmpi(obj.definitions.keys,'SEGY_Rev1_EQ');
    
    if all(~indeq)
        if strcmp(obj.definitions.filename,newobj.definitions.filename);
            return
        else
        me=MException('standardizeheader:UndefinedEquivalents',...
            ['The *.csv definitions file must contain a column named ''SEGY_Rev1_EQ''',...
            'Either add this using a spreadsheet program or using HeaderConverter']);
        throw(me)
        end
    end
    
    names=obj.definitions.values{:,indname};
    stdnames=obj.definitions.values{:,indeq};
    
    
    % loop through the segy equivalents and assign the new object header
    % these values
    for k=1:length(stdnames)
        values=getheadervalue(obj,names{k,1});
        newobj=setheadervalue(newobj,stdnames{k,1},values);
    end

    if isa(obj,'Trace')
        obj.traceheader=newobj;
        newobj=obj;
    end
    
catch me
    errordlg(me.message,me.identifier);
end
end
    
    