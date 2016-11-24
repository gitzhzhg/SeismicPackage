function c = getLogDefinitionSectionAsChar(obj,sn,outputversion)
%
% function c = getLogParameterSectionAsChar(obj,sn,outputversion)
%
%   Return formated char from cell array appropriate for a given LAS
%   version
%
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

    inputversion = str2double(obj.version);
    
    %assume failure
    c=[];
      
    ca = obj.getSection(sn);
%     celldisp(ca)
    ca = updateLogDefinitionSection(ca,inputversion,outputversion);
    c  = obj.getSectionAsChar(ca);
    
%     % if object version and output version are the same:
%     if isequal(inputversion,outputversion) || ...
%             (inputversion < 3.0 && outputversion < 3.0)
%         
%         disp('No parameters conversion')
%            c=obj.getSectionAsChar(log_definition);
%         
%     elseif inputversion < 3.0 && outputversion >= 3.0
%         disp ('STUB: convert from v2 to v3')
%                 
%     elseif inputversion >= 3.0 && outputversion < 3.0
%         disp ('STUB: convert from v3 to v2')      
%       
%     end

end %end function

function ca = updateLogDefinitionSection(ca,inputversion,outputversion)

%reset section name
ca{1} = {'~CURVE'};

%set comments (if any) to null
%ca{2} = cell(1,0);  

%clear log data formats
ca{3}(5,:) = {''};

%clear log mnemonic associations 
ca{3}(6,:) = {''};

%strip everything but required mnemonics
%ca{3}(:,~idx)=[];

if outputversion >= 3.0 % Add required mnemonics not present in LAS < 3.0
%     ca{3} = horzcat(ca{3}, ...
%     {...
%         'CTRY','PROV','LIC','LATI','LONG','GDAT'; ...
%         ''    ,''    ,''   ,''    ,''    ,''    ; ...
%         ''    ,''    ,''   ,''    ,''    ,''    ; ...
%         'COUNTRY','PROVINCE','LICENSE NUMBER',    ...
%             'X LOCATION','Y LOCATION','Geodetic Datum'; ...
%         ''    ,''    ,''   ,''    ,''    ,''    ; ...
%         ''    ,''    ,''   ,''    ,''    ,''    ;} ...        
%     )               
end

end