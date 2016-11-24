function PI_SetColorMap (varargin)
%
% function PI_SetColorMap ()
%
% Sets the colormap for the current plotimage figure
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

global NUMBER_OF_COLORS GRAY_PCT NOBRIGHTEN

switch nargin
    case 0 %no args
        thisfig = PI_GetFigureHandle();
    case 1 %thisfig
        thisfig = varargin{1};
    otherwise
        error('crewes:displaytools:plotimage','Incorrect number of input arguments.');
end

hcolormap = findobj(thisfig,'tag','COLORMAP');

if ~isempty(hcolormap)
   str = get(hcolormap,'string');
   val = get(hcolormap,'value');
   color_map = str{val};
end

% set the figures color map
if strcmp(color_map,'alpine') %CREWES color map
    cmap = alpine(NUMBER_OF_COLORS);
elseif strcmp(color_map,'seisclrs') %CREWES color map
    cmap = seisclrs(NUMBER_OF_COLORS,GRAY_PCT);
else
    cmap = eval([color_map '(' num2str(NUMBER_OF_COLORS) ')']);
end

%NOBRIGHTEN==0 => brighten the colormap before use
if ~NOBRIGHTEN %brighten cmap before use
    cmap = brighten(cmap,0.25);
end

%Set brightness of colormap based on current slider value
br_end  = get(findobj(thisfig,'tag','brightslider'),'value');
br_sign = sign(br_end);
br_strt = br_sign; %start at +/- 1, _not_ at zero

for ii = br_strt:br_sign:br_end
    cmap = brighten(cmap,br_sign*0.25);
end

%Set thisfig color map
set(thisfig,'colormap',cmap)