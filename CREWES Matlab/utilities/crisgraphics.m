function tf = crisgraphics( h, gtype )

% crisgraphics: return logical true if h is a graphics handle or logical
% false if it is not
%
% tf = crisgraphics( h, gtype )
%
% tf    = logical true or false for every h
% h     = object handle or array of object handles (function returns false 
%         for cell arrays of handles
% gtype = graphics type as character string (eg. 'figure' or 'axes')
% 
% by K.W. Hall, 2015
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

%Check input arguments
if nargin < 1 || nargin > 2
    error('crewes:utilities:crisgraphics','Incorrect number of input arguments')
elseif nargin == 1
    gtype = [];
end

% If Matlab version has a builtin isgraphics function, use it
if(exist('isgraphics','builtin'))
    tf = isgraphics(h, gtype);
    return
end

% If gtype is empty (nargin == 1), set to getListOfGtypes(). 
% Otherwise, give up if gtype is not a character string
if isempty(gtype)
    gtype = getListOfGtypes;
elseif ~ischar(gtype)
    error('crewes:utilities:crisgraphics','Input ''gtype'' must be char')
end

% Assume none of the handles in h are graphics objects, Pre-allocate by
% setting isg to false
[m, n] = size(h);
tf = false(m,n);

% Get type for each handle and compare to gtype
for ii = 1:m*n
    try
        tf(ii) = sum(strcmpi(get(h(ii),'type'),gtype));
    catch ex
%         tf(ii) = false;
    end
end


end %end function crisgraphics

function gt = getListOfGtypes()
%list of all type of graphics objects (list from R2010b and R2014b)
gt = { ...
    'figure', ...
    'axes', ...
    'line', ...
    'text', ...
    'patch', ...
    'rectangle', ...
    'surface', ...
    'image', ...
    'light', ...
    'uibuttongroup', ...
    'uicontextmenu', ...
    'uicontrol', ...
    'uimenu', ...
    'uipushtool', ...
    'uitable', ...
    'uitoggletool', ...
    'uitoolbar', ...
    'hggroup' ...
    };

end %end function getListOfGtypes