function ca = getSectionType( obj, sn )
%
%function ca = getSectionType( obj, sn )
% Returns cell array(s) selected from obj.sections based on indices
% contained in sn (if numeric) or on case-invariant matches between 
% section names in obj.sections and the contents of sn (char or cellstr).
%
% Note that this function can return the contents of more than one section:
% eg. sn='~log' could match '~log_parameter','~log_definition','~log_data',
% etc.
%
% If no matches to sn are found in the object, getSectionType returns an empty
% cell array
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

c = obj.getSection(sn);
if isempty(c)
    ca=c;
    return
end

if iscellstr(c{1})
   ca = c(4);
else
    for idx = 1:numel(c)
        ca{idx} = c{idx}{4};
    end
end

end