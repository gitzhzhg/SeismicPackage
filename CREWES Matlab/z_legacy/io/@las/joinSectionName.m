function c = joinSectionName( obj, sn )
%
%function c = joinSectionName( obj, sn )
%   returns char, eg1. '~Ascii', eg2. '~Log_data[1] | Log_definition[1]'
%   sn = cellstr array such as obj.sections{1}{1}
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

if ischar(sn)
    sn = {sn};
end

if isempty(sn) || numel(sn) >2
    warning('crewes:las:joinsectionname',...
        'Input may not be a section name, returning empty string');
    c = '';
else
   if isequal(numel(sn), 1)
       c = char(sn);
   elseif strncmpi(sn{1},'~ascii',6) || strncmpi(sn{1},'~a ',3);
       c = sn{1};       
   else
       c = sprintf('%s | %s',sn{:});
   end    
end

end %end joinSectionName()