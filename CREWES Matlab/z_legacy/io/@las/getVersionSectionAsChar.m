function c = getVersionSectionAsChar(obj,sn,outputversion)
%
% function c = getVersionSectionAsChar(obj,outputversion)
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

    %assume failure
    c=[];

    inputversion = str2double(obj.version);
%     outputversion;

    if outputversion < 2.0 %eg LAS 1.2
        version     = '1.2';
        description = 'CWLS LOG ASCII STANDARD - VERSION 1.2';
    elseif outputversion >= 3.0 %eg LAS 3.0
        version     = '3.0';
        description = 'CWLS LOG ASCII STANDARD - VERSION 3.0';
    else %LAS 2
        version     = '2.0';
        description = 'CWLS LOG ASCII STANDARD - VERSION 2.0';
    end
    
    if isequal(inputversion,outputversion)
        c = obj.getSectionAsChar(sn);
    else
        ca = obj.getSection(sn);
        ca = updateVersionSection(ca,version,description);
        %obj.sections={'section',sn,ca};
        c  = obj.getSectionAsChar(ca,outputversion);       
    end
    
end % end function
    
function ca = updateVersionSection (ca,version,description)
% reset version section name
% ca{1} = {'~Version'};

%set comments (if any) to null
% ca{2} = cell(1,0);  

%update version number in VERS mnemonic
ca{3}(1,:);
vers_idx = strcmpi(ca{3}(1,:),'VERS');
ca{3}(3,vers_idx) = {version};
ca{3}(4,vers_idx) = {description};

%strip everything but mnemonics VERS and WRAP
idx = strcmpi(ca{3}(1,:),'VERS') | strcmpi(ca{3}(1,:),'WRAP');
ca{3}(:,~idx)=[];

if str2double(version) >= 3.0 % Add DLM
    idx = strcmpi(ca{3}(1,:),'VERS') | strcmpi(ca{3}(1,:),'WRAP');
    ca{3}(:,~idx)=[];
    ca{3}(:,3) = { ...
        'DLM',     ...
        '',        ...
        'SPACE',   ...
        'DELIMITING CHARACTER (SPACE TAB OR COMMA)',...
        '',        ...
        ''};
end

end %end function updateVersion
        