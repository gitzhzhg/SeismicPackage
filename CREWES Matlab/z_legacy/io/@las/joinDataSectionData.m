function dat = joinDataSectionData( obj, dat, delimiter )
%
%function cs = joinDataSectionData( obj, dat, delimiter )
%
% Assumption: section data is already cellstr; needs to be more general.
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

if nargin < 3
    delimiter = obj.delimiter;
end

dat = obj.addDoubleQuotes(dat,delimiter);
if strcmp(delimiter,' ')
    dat = obj.padcellstr(dat);
end
% interleave columns
dat=dat';
[m, n] = size(dat);

% create 2D matrix of delimiters the same size as cs
delimiter = repmat({delimiter},m,n);

% interleave 'ca' and 'delimiter' columns
dat = reshape([dat(:) delimiter(:)]', 2*m, [])';

% drop rightmost column (extraneous delimiters)
dat(:,2*m)=[];

% concatenate cellstr's so we have one cellstr per row.
dat = num2cell(dat,1);
dat = strcat(dat{:});

end