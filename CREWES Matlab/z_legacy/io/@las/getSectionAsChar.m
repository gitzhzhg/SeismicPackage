function s = getSectionAsChar(obj,sn,delimiter)
%
%function s = getSectionAsChar(obj,sn,delimiter)
%  returns a formatted character array 
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

if isequal(nargin,2)
    delimiter = obj.delimiter;
end

if iscell(sn) && isequal(numel(sn),4)
    ca = sn;
else
    sidx = obj.getSectionIndices(sn);
    
    if isempty(sidx)
        s = cell(1,0);
        return
    end
    
    ca = obj.getSection(sidx);
end
% section name
s1 = obj.joinSectionName(ca{1});

% section comments
s2 = horzcat(s1,ca{2});

% data section
if obj.isDataSection(ca{1})
    s3 = obj.joinDataSectionData(ca{3},delimiter);
    s = char(vertcat(s2',s3));
% parameter section    
else
    s3 = obj.joinParameterSectionData(ca{3});
    s = char(horzcat(s2{1},s3));
end

end %end function