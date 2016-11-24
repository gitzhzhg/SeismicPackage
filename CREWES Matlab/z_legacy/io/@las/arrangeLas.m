function obj = arrangeLas(obj)
%
% function obj = arrangeLas(obj)
% Arrange regexp results from obj.splitlas into a cell array of sections
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

% grab all section names from obj
s = obj.sections(1,:);

% find indices of non-empty section names in cell array
idx = find(cellfun(@(x) ~isempty(x), s)==1);

% separate any associated section names based on '|' (LAS 3)
sn = regexp(s(idx),'\s\|\s','split');

% find indices for start and end of each section in cell array
startidx = idx+1;  %section starts one row after section name
endidx = idx-1;    %section ends one row before section name
endidx(end+1) = length(s); %except for last section, which ends at EOF
endidx = endidx(2:end); %and starts at index 1, not 0

%pre-allocate arrays
fc{length(idx)}=[];

np(idx)=0;
nd(idx)=0;

for i = 1:length(idx) % i = sequential section number within las file
    %%set section name
    fc{i}{1} = sn{i};
    
    %get section contents
    sc = obj.sections(:,startidx(i):endidx(i)); %section contents

    % How many parameters or data rows? mnemonics should be in row 3, data
    % in row 4
    np(i) = sum(cellfun(@(X) ~isempty(X),sc(3,:)));
    nd(i) = sum(cellfun(@(X) ~isempty(X),sc(4,:)));
    
    if isempty(sc)
        disp(['q)     Section: ',sn{i}{1}])
        %section has no information in it
        fc{i}{2}={[]};
        fc{i}{3}={[]};
        fc{i}{4}=false;        
    else
        % get comment lines
        comments = sc(2,:);
        c=cellfun(@(X) isempty(X),comments);
        comments(c) = [];
        fc{i}{2}=comments;
        %nuke empty cols corresponding to comment lines in sc
        sc(:,~c) = [];
        
        if ~isequal(np(i),0) %no data lines in this section
            disp(['a)     Section: ',sn{i}{1}])
                %'parameter section', remove section name and data rows;
                sc(1:2,:)=[]; %remove section name and comment lines
                sc(2,:)=[]; %remove comment lines
                
                %find exponential formats
                idx = cellfun(@(X) ~isempty(X),regexp(sc(5,:),'E'));
                
                %fix exponential formats (eg. E00.00E+00 => E0.000E+00)
                sc(5,idx) = cellfun(@(X) obj.modifyExponentialFormat(X),...
                    sc(5,idx),'UniformOutput',false);
                
                fc{i}{3} = sc;
                fc{i}{4} = false; % not data section

        else
            if isempty(sc)
                %section has no data
                disp(['b)     Section: ',sn{i}{1}])
                fc{i}{3} = sc(4,:);
                fc{i}{4} = false; %not data section                
            elseif strncmpi(sn{i}{1},'~o',2)
                % ~other section
                disp(['c)     Section: ',sn{i}{1}])
                fc{i}{3} = sc(4,:);
                fc{i}{4} = false; %not data section
            else
                % all other sections
                disp(['d)     Section: ',sn{i}{1}])
                fc{i}{3} = sc(4,:);
                fc{i}{4} = true; %data section
            end
        end
    end
end
obj.sections = {'all',fc};
obj.sectionNames = obj.setAssociatedSections(sn);

end