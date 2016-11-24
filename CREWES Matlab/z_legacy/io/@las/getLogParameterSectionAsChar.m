function c = getLogParameterSectionAsChar(obj,sn,outputversion)
%
% function c = getLogParameterSectionAsChar(obj,sn,outputversion)
%
%   Return formated char from cell array appropriate for a given LAS
%   version
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
    if isempty(ca)
        %c=cell(1,0);
        return
    else
        q = numel(ca{1}); %1 for one las section, 4 for multiple sections
        %celldisp(ca)
        if isequal(q, 4)
            %Problem; las has more than one ~p or ~log_parameter section          
            [~, d] = obj.uiSelectParameterSection(ca,...
                ['LAS has more than one log parameter section, '...
                 'please select the one you would like to keep'])
            ca = ca{d}; 
        end
        
        ca = updateLogParameterSection(ca,inputversion,outputversion);
        c  = obj.getSectionAsChar(ca);
    end


end %end function

function ca = updateLogParameterSection (ca,inputversion,outputversion)


% if object version and output version are the same:
% if isequal(inputversion,outputversion) || ...
%         (inputversion < 3.0 && outputversion < 3.0)
%     % do nothing ?
% elseif inputversion < 3.0 && outputversion >= 3.0
%     % do nothing ?   
% elseif inputversion >= 3.0 && outputversion < 3.0
%     % change section name
    ca{1} = {'~PARAMETER'};    
% end

%set comments (if any) to null
ca{2} = cell(1,0);  

% %find required mnemonics
% idx = strcmpi(ca{3}(1,:),'PDAT') | ...
%       strcmpi(ca{3}(1,:),'APD')  | ...
%       strcmpi(ca{3}(1,:),'DREF') | ...
%       strcmpi(ca{3}(1,:),'EREF') | ...
%       strcmpi(ca{3}(1,:),'ETIM');
% 
% %strip everything but required mnemonics
% ca{3}(:,~idx)=[];
% 
% if outputversion >= 3.0 % Add required mnemonics not present in LAS < 3.0
% %     ca{3} = horzcat(ca{3}, ...
% %     {...
% %         'CTRY','PROV','LIC','LATI','LONG','GDAT'; ...
% %         ''    ,''    ,''   ,''    ,''    ,''    ; ...
% %         ''    ,''    ,''   ,''    ,''    ,''    ; ...
% %         'COUNTRY','PROVINCE','LICENSE NUMBER',    ...
% %             'X LOCATION','Y LOCATION','Geodetic Datum'; ...
% %         ''    ,''    ,''   ,''    ,''    ,''    ; ...
% %         ''    ,''    ,''   ,''    ,''    ,''    ;} ...        
% %     )               
% end

end %end function updateVersion