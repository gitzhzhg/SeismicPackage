function varargout = readlas(obj)
%
% function varargout = readlas(obj)
%   Wrapper method for las object to emulate behaviour of original
%   readlas() function:
%
% function [logmat,logmnem,logdesc,wellname,wellid,loc,nullval,dpthunits,...
%		kb,tops,ztops,lasheader]=readlas(filein,errflag)
%
% filein ... string with the file name (and full path if desired)
% errflag ... flag indicating error behavior. if errflag == 0, then if an
% error occurs during reading, this function will abort calling MATLABS
% ERROR function. If errflag==1 and an error occurs, the function will
% return normally with logmat set to -1 and all other values [].
% *************** default = 0 *********************
%   logmat ... matrix containing the logs in the file. Each log is in a column 
%   and the first column contains the depths. Thus, if logmat has n columns,
%   then there are n-1 logs
%   mnem ...matrix of standard 4 letter mnemonics of the n columns of logmat
%   desc ... matrix of string descriptors of the n columns of the logmat
%   name ... string with the well name
%   id ... the unique well id
%   loc ... string containg the well location
%   null ... the null value used in the logs
%   units ... string indicating the depth units of the log
%   kb ... elevation of the kelly bushing
%   tops ... names of tops found in the file
%   ztops ... depths of tops
%   lash ... matlab string matrix containing the entire las header
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

%prep output to match expected output from readlas upon failure
varargout{13} = obj.fileName;
varargout{1}  = -1;

inputversion = str2double(obj.version);

if ~isempty(obj.sections) %make sure we're playing with a full deck   
    if inputversion < 3.0
        vers_parameter  = '~v';
        well_parameter  = '~w';
        [tops, tops_idx]  = obj.uiSelectParameterSection('~t'); %more than one ~t?
        tops_parameter = obj.getSectionNames(tops_idx);
        tops_definition = '';
        tops_data       = '';        
        log_parameter   = '~p';
        log_definition  = '~c';
        log_data        = '~a';        
    else
        vers_parameter  = '~version';
        well_parameter  = '~well';
        tops_parameter  = '~tops_parameter';
        tops_definition = '~tops_definition';
        tops_data       = '~tops_data';
        log_parameter   = '~parameter';
        log_definition  = '~curve';
        log_data        = '~ascii';        

        if isempty(obj.getSectionNames(log_data))
            log_parameter  = '~log_parameter';
            log_definition = '~log_definition';
            log_data       = '~log_data'; 
        end
    end

    %get number of logs in LAS in all well log data sections (LAS3)
    nlogs = obj.numLogs(log_data);
    if numel(nlogs) > 1 %LAS3
       [log_data, log_definition] = obj.uiSelectDataSection(log_data, ...
           {'';...
            'Logedit';...
            'does not handle more than one log data section. ';...
            'Please select section you wish to edit before continuing';...
           });
        nlogs = obj.numLogs(log_data);    
    end

    if nlogs > 10
        obj = obj.uiDeleteLogs(log_data, ...
            {'';...
            'Logedit';...
            'does not handle more than ten logs at a time very well. ';...
            'Please consider deleting some logs before continuing';...
            });        
    end
    
    d = obj.getSectionDataAsDouble(log_data);     
    varargout{1} = d{1};
    
    %mnem
    varargout{2}  = char(obj.getParameterSectionMnemonics(log_definition));

    %truncate mnemonics to 4 characters
    %     if size(mnem,2) > 4
    %        varargout{2} = mnem(:,1:4);
    %     end
    
    %desc
    varargout{3}  = char(obj.getParameterSectionDescriptions(log_definition));   
    
    %name
    varargout{4}  = obj.getMnemonicValue(well_parameter,'WELL');
    
    %id
    varargout{5}  = obj.getMnemonicValue(well_parameter,'UWI');
    
    %loc
    varargout{6}  = obj.getMnemonicValue(well_parameter,'LOC');
        
    %null
    varargout{7}  = str2double(obj.lognull);
   
    %units
    varargout{8}  = obj.getMnemonicUnit(log_definition,'DEPT');
    if isempty(varargout{8}) %time?
        varargout{8} = obj.getMnemonicUnit(log_definition,'ETIM');
    end
     
    %kb
    varargout{9}  = obj.getMnemonicValue(log_parameter,'EKB');
    if isempty(varargout{9}) %try EREF
        varargout{9}  = obj.getMnemonicValue(log_parameter,'EREF');
    end
    if isempty(varargout{9}) %try looking under well information
        varargout{9} = obj.getMnemonicValue(well_parameter,'EREF');      
    end
    if isempty(varargout{9}) %give up?
        varargout{9} = [];
    else
        varargout{9} = str2double(varargout{9}); %convert to double
    end
    
    [varargout{10}, varargout{11}] = obj.getTops(tops_parameter,tops_definition,tops_data);
    
    % outputversion for logedit (hardwired)
    outputversion = 2.0;
    obj.delimiter  = 'SPACE';
    
    lashead = { ...
        obj.getVersionSectionAsChar(vers_parameter,outputversion);      ...    
        obj.getWellSectionAsChar(well_parameter,outputversion);         ...
        obj.getTopsSectionsAsChar(      ...
                        tops_parameter, ...
                        tops_definition,...
                        tops_data,      ...
                        outputversion);                                 ...
        obj.getLogParameterSectionAsChar(log_parameter,outputversion);  ... 
        obj.getLogDefinitionSectionAsChar(log_definition,outputversion);...
        '~A'...     
        };
    
    %Remove blank lines (if any) from header
    lashead(cellfun(@isempty,lashead))=[];
    varargout{12} = char(lashead);

end %end

end %end function