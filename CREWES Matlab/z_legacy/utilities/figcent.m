function b=figcent(width,height,units,masterfig)
%
% figout=figcent(width,height,units,masterfig)
% figout=figcent(width,height,units)
% figout=igcent(width,height)
% figout=figcent
%
% FIGCENT will open a figure in the centre of the screen
% to a user defined or default size.  If the user wishes to have a new
% figure centered on an existing figure, masterfig must be supplied
%
% width  = screen width in units (pixels or normalized)
% ********** Default = .5 (normalized units) *************
%
% height = screen height in units 
% ********** Default = .5 (normalized units) *************
%
% units	 = unit type for scaling figure
% 	 = 'pixels' or 'normalized'
% **************** Default = 'normalized' ****************
%
% masterfig  =  handle to the figure user wants new figure centered on
% *********** Default = 'absolutly nothing!' *************
% 
% By Jeff Larsen, July 1997
% - Completely reworked by Chris Harrison, July 2003
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
screenunits=get(0,'units');
% Just in case the original units of the screensize are not in pixels,
% returned to original screen units after done acquiring size
set(0,'units','pixels');
ssize=get(0,'screensize');
set(0,'units',screenunits);
mastersize=[];
if(nargin<1)
    height = .5;
    width = .5;
    units='normalized';
elseif(nargin==1)
    stringinfo={'Please include both a width and a height for desired figure'};
    helpdlg(stringinfo,'Check FIGCENT help file');
    return
elseif(nargin==2)
    % user wants figure centered on Screen with defaulted units
    units = 'normalized';
elseif(nargin<=3)
    % user is specifying units, and centered on screen
elseif(nargin==4)
    % user desires new figure centered on masterfigure
    if(~ishandle(masterfig))
        stringinfo={'Masterfig argument is not a figure'};
        helpdlg(stringinfo,'Check FIGCENT help file');
        return
    elseif(~strcmp(get(masterfig,'type'),'figure'))
        stringinfo={'Masterfig argument is not a figure'};
        helpdlg(stringinfo,'Check FIGCENT help file');
        return
    end
    % masterfigure units are changed to normalized and then back to
    % original units
    masterunits=get(masterfig,'units');
    set(masterfig,'units','normalized');
    mastersize=get(masterfig,'position');
    set(masterfig,'units',masterunits);
end
% checking width, height, and units
if(~isnumeric(width)|~isnumeric(height))
    stringinfo={'Please set your width and height as numeric values'};
    helpdlg(stringinfo,'Check FIGCENT help file');
    return
elseif(~ischar(units))
    stringinfo={'Please set units properly'};
    helpdlg(stringinfo,'Check FIGCENT help file');
    return
end
% mispelling can occure, just as long as the first lett is "p" or "n",
% eventually this section should be able to handle any units, as long as at
% the end of this section, the width and height are normalized
if(strcmp(units(1,1),'p') )
    % pixles, but changed to be normalized wrt the screen size
    width=width/ssize(3);
	height=height/ssize(4);
elseif(strcmp(units(1,1),'n'))
    % normalized already
else
    % user has not supplied a proper unit
    stringinfo={'Please set units properly'};
    helpdlg(stringinfo,'Check FIGCENT help file');
    return
end
% checking width and height
if((width > 1 | height > 1))
    stringinfo={'Width or Height greater then screen size'};
    helpdlg(stringinfo,'Check FIGCENT help file');
    return
end
if(isempty(mastersize))
    % new figure centered on screen
    pos1=.5-width/2;
    pos2=.5-height/2;
else
    % new figure centered on masterfigure, will adjust in case masterfigure
    % intially slightly off screen
    po1=sort([width (mastersize(1)+mastersize(3)/2+width/2) 1]);
    pos1=po1(2)-width;
    po2=sort([height (mastersize(2)+mastersize(4)/2+width/2) 1]);
    pos2=po2(2)-height;
end
if( nargout==1 )
    b=figure('units','normalized','position',[pos1 ...
            pos2 width height],'menubar','none');
else
    figure('units','normalized','position',[pos1 ...
            pos2 width height],'menubar','none');
end
set(gcf,'units','pixels');