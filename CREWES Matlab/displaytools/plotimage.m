function plotimage(smat,t,x)
% PLOTIMAGE ... Image display utility for seismic matrices
%
% plotimage
% plotimage(smat)
% plotimage(smat,t)
% plotimage(smat,t,x)
%
% PLOTIMAGE can be opened in several ways.  Simply typing 'plotimage' will
% initiate the PLOTIMAGE figure.  Initializing PLOTIMAGE using the other 
% methods does a quick plot of a seismic matrix in a figure window
% (made by plotimage). By default, it plots the seismic matrix in gray levels
% using the seisclrs colormap.  
%
%	smat ... the seismic matrix to be plotted. Traces are assumed stored in
%       the columns smat.
%	t ... time coordinates of traces.
%       ****** default 1:nrows where nrows = number of rows in smat ****
%	x ... x coordinates of the traces
%       ****** default 1:ncols where ncols=number of columns *****
%
% As of 2003, PLOTIMAGE has a new look interface and more features.
% Descriptions of new features follow.
% 
% Image Controls
%----------------
% There are three modes for which PLOTIMAGE can be set.  Zoom mode allows
% for the user to zoom in on the image using a box and zoom out with a
% single click.  There are two picking modes, Pick(O) and Pick(N).  The 'O'
% and 'N' stand for old and new.  This signifies that invoking Picks(O)
% allows user to augment an already existing pickset while
% invoking Picks(N) begins a new pickset by clearing the existing PICKS
% matrix and deleting any picks drawn on top of the plot.
%
% Each PLOTIMAGE window can also be set to the status of "independent",
% "master", or "slave".  This refers to the method by which the maximum
% absolute value and standard deviations of the data are obtained.  For
% both "independent" and "master" these numbers are measured from the input
% data while for the "slave" case the numbers are the same as for the most
% recent PLOTIMAGE window to be declared "master".  This allows multiple
% PLOTIMAGE windows to be displayed in true relative amplitude with respect
% to one another by setting one to be "master" and the other(s) to be
% "slave". NOTE: If you change the identity of the master window, then any
% slave windows are not automatically refreshed. To refresh then, simply
% toggle the amplitude control to independent and then back to slave.
% 
% The two basic plot modes, mean and maximum scaling, are described below. Mean scaling is
% best for real data (with a large dynamic range) while maximum scaling is preferred for
% synthetic data when you want an accurate display of amplitudes.
%
% Mean scaling (SCALE_OPT=1)... The mean and standard deviation of all samples are computed.
%		samples>= mean+CLIP*stddev are clipped (set equal to mean+CLIP*stddev).
%		(The same is done for negative samples which are set to mean -CLIP*stddev).
%		These two extremes are mapped to the ends of the color map and all
%		intermediate values are displayed linearly.
% Maximum scaling (SCALE_OPT=2) ... The maximum absolute value of the data is computed (say mxs).
%		The extremes of the colormap are then assigned to +/- mxs and all
%		intermediate values are displayed linearly. There is no clipping but
%       the display may be dominated by large values.
%
% MAIN AXIS 
% ---------
% Contains the image that the user has loaded.  Various uicontext menu
% controls (accessed using MB2 on the axis) help control the axis labels,
% sample rate, Zoom Options, and Picks options (only when other PLOTIMAGE
% figures are open).  
%
% Limit Box
%-----------
% Can be activated on the main axis uicontext menu, or Options menu.
% Activiating this function creates a box which finds the mean value inside
% the box and limits the rest of the image.  This can be used in both Mean
% amd Maximum scaling modes.  
%
% Zoom options
%--------------
% When zooming mode is initiated in the Image Controls, MB2 clicks and drags
% in the main axis will initiate zooming limited to the image.  When
% multiple PLOTIMAGES are open, each figure can be zoom locked to any
% other.  Publishing and Matching zoom limits are also available options
% when multiple PLOTIMAGE windows have been opened.
%
% Picking
%---------------
% There are two distinct types of picking: time-dips or amplitude.
% Picking of time-dips is done without reference to the data. The accuracy
% of the time-dip is entirely controlled by the user mouse motions. 
% In contrast, picking of amplitudes does refer to the data. The user
% clicks at distinct points to define a trajectory along which picks are to
% be
% made. This trajectory is then sent to PICKER to make the picks. It is also
% possible to copy PICKS from one PLOTIMAGE window to another. See the
% "options" menu. For more description on the mechanics of amplitude
% picking, type "help ipick". For more discussion on the picking algorithms
% type "help picker".
% 
% Global Pick Buffers
% Time-dip picks are stored in a global buffer called PICKS. Amplitude
% picks are stored in a global buffer called AMP_PICKS. Because these are
% global buffers, they are used by all open PLOTIMAGE windows. Things can
% get quite confusing so it is a good idea to pick in a single window at a
% time and harvest the picks from the buffers before moving on. NOTE: In
% Matlab, global variables are only visible (or accesible) if you declare
% them. That is done with commands like:
% >> global PICKS
% or 
% >> global AMP_PICKS
% 
% Structure of the PICKS global:
% This is an Nx3 cell matrix with one row for each PLOTIMAGE window. The
% columns of the kth row have the following contents:
% PICKS{k,1} ... the figure number of the PLOTIMAGE window that made the
%           picks
% PICKS{k,2} ... a matrix of real numbers defining the picks. There is one
%          row per pick. Each row has four entries defining the (x,t)
%          coordinates of the two points defining the time dip.
% PICKS{k,3} ... vector of the graphic handles of the picks.
%
% Structure of the AMP_PICKS global:
% This is a cell array of structures, one structure per picked event. Thus
% the structure for the kth picked event is accessed by:
% >> pickstruc=AMP_PICKS{k}
% The structures have the following fields:
% pickstruc.eventname ... the name of the event as specfied by the user
% pickstruc.handle ... the graphic handle of the line displaying the picks
% pickstruc.picktype ... string defining the type of pick (see PICKER for more)
% pickstruc.trajhandle ... graphic handle of the points clicked by the user to
%          define the picking trajectory.
% pickstruc.delt ... width of the picking fairway
% pickstruc.amppick ... amplitude of the pick
% pickstruc.ampevent ... amplitude of the event
% pickstruc.tpick ... time of the pick
% pickstruc.xpick ... x coordinate of the pick
% 
%
% Position Axis
% -------------
% This axis is a smaller version of the main axis.  When a zoom occures on
% the main axes, a red patch appears on the position axis that corresponds
% to the location of the zoom that just occured.  Moving the patch with MB1
% will also move the zoom parameters on the main axis.
%
% NOTE: PLOTIMAGE has a number of parameters that can be set via global
% variables.  SCALE_OPT, NUMBER_OF_COLORS, GRAY_PCT, CLIP, COLOR_MAP, AMPFLAG
% NOBRIGHTEN, PICKCOLOR, XAXISSTOP, CLOSEREQUEST, IMCONTROLS are all 
% parameters that the User is allowed to set in the command window while 
% plotimage is running.  These variables are best changed using the 
% figure context menu, but can also be changed by other programs while 
% plotimage is running.  Changing the corresponding globals sets the default 
% behavior, see plotimage_setglobal for more details.
% Meaning of Plotimage globals
%    CHECK_GLOBAL ... set to 1 to turn on the behavior of the other globals
%    SCALE_OPT ... controls the scaling option as described above
%    CLIP ... clip level (default 4)
%    AMPFLAG ... controls the relative scaling (independent, master, or slave)
%    NUMBER_OF_COLORS ... number of colors in the colormap (default 64)
%    GRAY_PCT ... controls the nonlinearity of the default gray scale
%    COLOR_MAP ... name of the colormap to use (default 'seisclrs')
%    NOBRIGHTEN ... determines brightness of the colormap
%    PICKCOLOR ... color of picks
%    XAXISTOP ... location of x axis (default 'bottom')
%    CLOSEREQUEST ... control whether plotimage closes with a warning or not
%    IMCONTROLS ... allows plotimage to begin with image controls hidden
% Again, for a better description of these, see plotimage_setglobal
% Note: You can set your favorite values for these easily in your startup.m
% file. For example, suppose you want plotimage to always begin with mean
% scaling and clip level of 6. Then the lines:
% global SCALE_OPT CLIP CHECK_GLOBAL
% SCALE_OPT=1;CLIP=6;CHECK_GLOBAL=1;
% could be placed in your startup.m file.
%
% G.F. Margrave, CREWES Project, U of Calgary, 1996, 1999, 2000, and 2003,
% 2013
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

%plotimage has become maddeningly complex. The next 4 lines are to ensure that SCALE_OPT
%defaults to 1 if undefined. Somewhere deep in the code is a default to 2. Cannnot find that to
%change it so am overridding it here (GFM).
global SCALE_OPT
if(isempty(SCALE_OPT))
    SCALE_OPT=1;
end

if(nargin==0)
    action='PI_init';
    smat=[];
    t=[];
elseif(nargin==1)
    % testing smat to see if it is numericall
    if(isnumeric(smat))
        action='PI_init';
        %smat=smat;
    else
        action=smat;
        smat=[];
    end
    t=[];
    x=[];
elseif(nargin==2);
    action='PI_init';
%     smat=smat;
%     t=t;
    x=[];
elseif(nargin==3)
    action='PI_init';
%     smat=smat;
%     t=t;
%     x=x;
end
if(strcmp(action,'PI_init'));
    PI_CheckGlobals;
    cfig = PI_init_image;
    if(~isempty(smat))
        PI_OpenFile(cfig,smat,t,x);
    end
    return
end
if(strcmp(action,'SpawnPlotImage'))
    PI_SpawnPlotImage;
    return
end
if(strcmp(action,'OpenFile'))
    PI_OpenFile;
    return
end
if(strcmp(action,'SaveFile'))
    PI_SaveFile;
    return
end
% the following is going to turn off position axes if it is supposed to be
% off... I just can't quickly find a better place for it
hbak=findobj(gcf,'tag','BACKING');
if(~isempty(hbak))
    checkon=get(hbak,'visible');
    if(strcmp(checkon,'off'))
        set(findobj(gcf,'type','axes','tag','POSITIONAXES'),'visible','off');
        set(get(findobj(gcf,'type','axes','tag','POSITIONAXES'),'children'),'visible','off','hittest','off');
    end
end
% Alternate Button Head-off-at-the-Pass- Function
%----------------------------------------
%
% This function will head off callbacks at the pass (so to speak)
if(strcmp(action,'zoom')||strcmp(action,'pick')||strcmp(action,'PickMoveClose'))
    StopOrGo=PI_HeadOff(action);
    if(strcmp(StopOrGo,'STOP'))
        drawnow
        return
    end
end

if(strcmp(action,'zoom'))
    PI_zoom;
    return;
end
% Zoom Options (zoomoptions)
%--------------
%
% Allows user to set locks for zoom controls
if(strcmp(action,'zoomoptions'))
    PI_zoomoptions;
    return
end

% Zoom Srolling (zoomscroll)
%---------------
%
% This function allows user to move around a zoomed plot
if(strcmp(action,'zoomscroll'))
    PI_zoomscroll;
    return
end
if(strcmp(action,'zoominout'))
    PI_zoominout;
end
if(strcmp(action,'zoomscrollmotion')||strcmp(action,'zoominoutmotion'))
    PI_zoomscrollmotion(action);
    return
end
if(strcmp(action,'zoomfcnend'))
    PI_zoomfcnend;
    return
end

if(strcmp(action,'flip'))
    PI_flip;
    return;
end
if(strcmp(action,'zoompick'))
    PI_zoompick;
    return;
end
if(strcmp(action,'pick'))
    PI_pick;
    return
end
if(strcmp(action,'fromipick'))
    PI_zoompick('fromipick');
    return
end
if(strcmp(action,'showamppicks'))
    PI_showamppicks;
    return
end
% Pick Line Menu and options (picklinemenu)
%----------------------------
%
% This will allow bring up a uicontext menu where user can move or delete lines
if(strcmp(action,'picklinemenu'))
    PI_picklinemenu;
    return
end
if(strcmp(action,'LmLnActivation'))
    PI_LmLnActivation;
    return
end
if(strcmp(action,'ImportPicks'))
    PI_ImportPicks;
    return
end
if(strcmp(action,'MovePickLineStop'))
    PI_MovePickLineStop;
    return
end
if(strcmp(action,'DeletePickLine'))
    PI_DeletePickLine;
    return
end
if(strcmp(action,'MovePickLine'))
    PI_MovePickLine;
    return
end
if(strcmp(action,'MovePickLineStart'));
    PI_MovePickLineStart;
    return
end
if(strcmp(action,'MovePickLineMotion'))
    PI_MovePickLineMotion;
end
if(strcmp(action,'MovePickLineEnd'))
    PI_MovePickLineEnd;
    return
end

if(strcmp(action,'setcolormap'))
    PI_SetColorMap;
    return
end

if(strcmp(action,'colormap'))
    PI_PlotImageColorMap;
    return
end

if(strcmp(action,'brighten'))
    PI_PlotImageBrighten;
    return;
end

if(strcmp(action,'rescale')||strcmp(action,'limboxrescale'))
    PI_rescale(action);
    return;
end

% MOVEMENT OF POINTS and LINES (limptmove) & (limlnmv)
%------------------------------
%
% This will begin the movement of the four points and corresponding on the axes
% this will also open a menu for both lines and markers that will
% allow user to change properties of the lines and markers.
if(strcmp(action,'limptmove')||strcmp(action,'limlnmove')||strcmp(action,'limcentmove'))
    PI_limptmove(action);
    return
end
if(strcmp(action,'limptmove2'))
    PI_limptmove2;
    return
end
if(strcmp(action,'limlnmove2'))
    PI_limlnmove2;
    return
end

if(strcmp(action,'limmoveend'))
    PI_limmoveend;
    return
end

% User settings for limit lines and points (limlnoptions)
%------------------------------------------
%
% This callback is where the user can specify what the look of the lines
% and markers is going to be
if(strcmp(action,'limlnoptions'))
    PI_limlnoptions;
    return
end

% Limit Box Master Control (limboxmaster)
%--------------------------
%
% This call back occures when limit box hasbeen moved allowing for new
% properties to be applied to the present figure as well as slaved figures
if(strcmp(action,'limboxmaster'))
    PI_limboxmaster;
    return
end

%---------------------------------------------
%---------------------------------------------
%  Call backs for MVLINESMEASUREMENTS figure
%---------------------------------------------
%---------------------------------------------
%
% These call backs control the small data figure that shows the positions
% of the reference lines.  Measuremen figure can only be removed my deleteing
% it due to its closefcn being set to the following call back

% Reset line measurements (limptreset)
%-------------------------
%
%  Resets measurements to zero
if(strcmp(action,'lmptreset')||strcmp(action,'lmptresetmenu'))
    PI_lmptreset(action);
    return
end

% Manual Enter Measurements (lmptenter)
%---------------------------
%
% Allows user to input own numbers for measurments
if(strcmp(action,'lmptenter'))
    PI_lmptenter;
    return
end
if(strcmp(action,'ChangePropertiesEnd'))
    % untill askthings init changes, this is the only way to make this work
    PI_ChangeProperties(2);
    return
end
if(strcmp(action,'ChangePropertiesMenu'))
    PI_ChangeProperties(3);
    return
end
if(strcmp(action,'PicksOpen'))
    PI_PicksOpen;
    return
end
if(strcmp(action,'PicksSave'))
    PI_PicksSave;
    return
end
if(strcmp(action,'copyamppicks'))
    PI_copyamppicks;
end
if(strcmp(action,'copyamppicks2'))
    PI_copyamppicks('pick');
end
if(strcmp(action,'copyamppicks3'))
    PI_copyamppicks('pick2');
end
if(strcmp(action,'figuresizechange'))
    PI_FigureSizeChange;
    return
end
if(strcmp(action,'launchpicktool'))
    hmaster=gcf;
    hmsg=findobj(hmaster,'tag','messages');
    set(hmsg,'string','Load saved pick file (or cancel if none)');
    PI_LaunchPicktool;
    set(hmsg,'string','PickTool is ready to use')
    hpicktool=findobj(hmaster,'tag','picktool');
    set(hpicktool,'enable','off')
    %adjust the main axis size parameters
    hcolormap=findobj(hmaster,'tag','COLORMAP');
    mainaxparms=get(hcolormap,'userdata');
    mainaxpos=get(gca,'position');%this is the position determined by picktool
    if(mainaxpos(1)<.19) %this indicates that image controls are hidden
        mainaxparms(6)= mainaxpos(3);
        mainaxparms(3) = mainaxpos(1)+mainaxpos(3)-mainaxparms(1);
    else
        %image controls are visible
        mainaxparms(3) = mainaxpos(3);
        mainaxparms(6)= mainaxpos(1)+mainaxpos(3)-mainaxparms(5);
    end
    set(hcolormap,'userdata',mainaxparms)
    return;
end