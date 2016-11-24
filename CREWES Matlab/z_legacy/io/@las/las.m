classdef las
%Purpose: read LAS file and return a cell array of all information
%contained in the LAS file.
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

properties
    fileName;
    fullFileName;
    version;
    delimiter;
    wrapped;
    lognull;
    sections;
    sectionNames;
end %end properties

methods
    
%Constructor
function obj       = las(varargin)
    obj = obj.checkInputArgs(obj, varargin);
    
    if isempty(obj.fileName)
        return
    end
    
    switch obj.fileName
        case('new')
            obj = obj.createNewLas();             
        otherwise
            if obj.isLas()
                obj = obj.read();       % read text from las file
                obj = obj.splitLas();   % split file contents into 2D cell array
                obj = obj.arrangeLas(); % arrange 2D cell array structured cell array
                obj = obj.setLasInformation(); %get basic information about the LAS file
                obj = obj.splitData();  % split data lines based on obj.delimiter
                obj = obj.fixTops();    % re-arrange tops sections for consistency
            else
                obj.fileName='';
                obj.fullFileName='';
            end
    end
end

%Other methods
% % obj  = createNewLas(obj)
% % d    = cell2double(obj, c)
% % s    = cell2las(obj,sn)
% % obj  = clearComments(obj)
% % obj  = las2cell(obj)
% % 

% % file = checkOutputArgs(obj, varargin)
 
%Get section information from LAS object
%sn = partial section name as char or as index number into obj.sections, ca = cell array

%% commented and re-tested
%get information from obj.sectionNames
ca   = getSectionNames(obj,sn)
ca   = getSectionAssociation(obj, sn)

% % ca   = getAssociatedSectionNames(obj,sn)
% % idx  = getAssociatedSectionIndices(obj,sn)
t    = getSectionType(obj,sn)

%get information from obj.sections
ca   = getSection(obj,sn)
% % ca   = getSectionComments(obj,sn)
ca   = getSectionData(obj,sn)
ca   = getSectionDataAsDouble(obj,sn)

% Get Information from Parameter section
f    = getParameterSectionRows(obj,sn,row)
f    = getParameterSectionMnemonics(obj, sn)
f    = getParameterSectionFormats(obj, sn)
f    = getParameterSectionValues(obj, sn)
f    = getParameterSectionValuesAsDouble(obj, sn)
f    = getParameterSectionUnits(obj, sn)
f    = getParameterSectionDescriptions(obj, sn)
f    = getParameterSectionAssociations(obj, sn)

% % cs   = cell2cellstr(obj,sn)
n    = numLogs(obj, sn)

% Get Information from a paramter section
v    = getMnemonicUnit(obj,sn,mn)
v    = getMnemonicValue(obj,sn,mn)
v    = getMnemonicDescription(obj,sn,mn)
v    = getMnemonicFormat(obj,sn,mn)
v    = getMnemonicAssociation(obj,sn,mn)
i    = getMnemonicIndex(obj,sn,mn)

%modify sections in LAS objext
 c    = getSectionAsChar(obj,sn,lasver)
% % % c    = getTopsAsChar(obj,lasver)            %v = output version (1,2,3)

obj        = obj.deleteLogs(obj,lm,sn)
obj        = obj.uiDeleteLogs(obj,sn,msg)
varargout  = obj.uiSelectDataSection(obj,sn,msg)
varargout  = obj.uiSelectParameterSection(obj,sn,msg)

c    = getVersionSectionAsChar(obj,sn,outputversion)
c    = getWellSectionAsChar(obj,sn,outputversion)
c    = getTopsSectionsAsChar(obj,tp,tdef,tdat,outputversion)
[tn,td] = getTops(obj,tparm,tdef,tdat)
c    = getLogParameterSectionAsChar(obj,sn,outputversion)
c    = getLogDefinitionSectionAsChar(obj,sn,outputversion)
% % c    = getLogDataSectionsAsChar(obj,sn,outputversion)
% % 
% % 
ca   = joinSectionName(obj,sn)
% ca   = joinSectionComments(obj,sn)
% ca   = joinSectionData(obj,sn)
ca   = joinParameterSectionData(obj,sn)
ca   = joinDataSectionData(obj,sn,delimiter)
t    = isDataSection(obj,sn)


varargout = readlas(obj)
% % write(obj,varargin)


function obj       = set.delimiter(obj,d)
% LAS versions <= 2 used spaces as delimiters in data sections.
% LAS version 3 allows the use of SPACE, TAB or COMMA
    switch(upper(d))
        case('SPACE')
            obj.delimiter = ' ';
        case('TAB')
            obj.delimiter = char(9);
        case('COMMA')
            obj.delimiter = ',';
        otherwise % SPACE is the default
            obj.delimiter = ' ';
    end
end %end set.delimiter

function obj = set.sections(obj,v)
    %disp('in set.sections')
    if isempty(v)
        obj.sections = [];
        return
    end

    switch v{1}
        case 'all'
            obj.sections = v{2};
        case 'section'
            disp('made it to case section')
            sidx = obj.getSectionIndices(v{2});
            obj.sections{sidx} = v{3};
        case 'sectionname'
            warning('crewes:las','not implemented yet');
        case 'sectioncomments'
            warning('crewes:las','not implemented yet');
        case 'sectiondata'
            sidx = obj.getSectionIndices(v{2});
            
            if isempty(sidx)
                warning('crewes:las',['Section: ' sn ' was not found']);
            else
                if isequal(length(sidx),1)
                    if iscellstr(v{3})
                        obj.sections{sidx}{3} = v{3};
                    else
                        %check to see if format string matches?
                        %nah...
                        obj.sections{sidx}{3} = v{3};
                    end
                else
                    warning('crewes:setDataSectionData', ...
                        ['Section name: ' v{2} ' is not unique']);
                end
            end
        case 'sectiontype'
            sidx = obj.getSectionIndices(v{2});
            if isempty(sidx)
                warning('crewes:las',['Section: ' sn ' was not found']);
            else
                if islogical(v{3})
                    obj.sections{sidx}{4} = v{3};
                else
                    warning('crewes:las','datasection flag must be set to true/false');
                end
            end

        case 'mnemonicunit'
            warning('crewes:las','not implemented yet');
        case 'mnemonicvalue'
            sidx = obj.getSectionIndices(v{2});
            
            %get the section data
            ca = obj.getSectionData(sidx);
            
            %get index for the desired mnemonic
            midx = strcmpi(ca(1,:),v{3});
            
            switch sum(midx)
                case 0 %mnemonic not found in first row of cellarray
                    % do nothing
                case 1 %values are in the 3rd row
                    ca{3,midx}=v{4};
                    obj.sections{sidx}{3}=ca;
                otherwise 
                    % do nothing
            end
        case 'mnemonicdescription'
            warning('crewes:las','not implemented yet');
        case 'menmonicformat'
            warning('crewes:las','not implemented yet');
        case 'mnemonicassociation'
            warning('crewes:las','not implemented yet');
        case 'mnemonicunits'
            warning('crewes:las','not implemented yet');
        case 'mnemonicvalues'            
            warning('crewes:las','not implemented yet');
        case 'mnemonicdescriptions'
            warning('crewes:las','not implemented yet');
        case 'mnemonicformats'
            warning('crewes:las','not implemented yet');
        case 'mnemonicassociations'
            warning('crewes:las','not implemented yet');
        otherwise
            warning('crewes:las:setsections',['Option: ' v{1} ' not found; skipping...']);
    end %end switch v{1}
end %end set.sections

% end %end public methods
% 
% methods (Hidden = true, Access = private)
    %Methods call only by the constructor
    obj = read(obj)
    obj = splitLas(obj)
    obj = arrangeLas(obj)    
    obj = setLasInformation( obj )
    obj = splitData(obj)
    obj = fixTops(obj)
    
    %Methods called by constructor methods    
    obj  = checkInputArgs(obj, varargin)
    t    = isLas(obj)    
    sn   = setAssociatedSections(obj, sn)
    idx  = getSectionIndices(obj,sn)    
 end %end hidden, private methods

methods (Static = true)
    %method is in separate file
% %     sn = setAssociatedSections(sn)
% %     [df, fw] = newJavaDecimalFormat(v,f,d);    
% %     v = updateLasVersion(v)
    v  = padcell(v,maxlength)
    cs = padcellstr(cs)
% %     cs = convertFormatString(cs)
    fmt = modifyExponentialFormat (fmt)    

    cs = addDoubleQuotes(cs,delimiter)
% %     
% %     v = newVersionSection()
% %     v = newWellSection()
% %     v = newParameterSection(sn)
% %     v = newDefinitionSection(n)
% %     v = newDataSection(n)
% %     [a, b]= fieldwidth(d)
end %end static methods

end %end classdef