function PI_CheckGlobals()
%
% function PI_CheckGlobals()
%
% Checks global variables
%    CHECK_GLOBAL SCALE_OPT CLIP AMPFLAG NUMBER_OF_COLORS GRAY_PCT
%    NOBRIGHTEN COLOR_MAP PICKCOLOR XAXISTOP CLOSEREQUEST IMCONTROLS
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

% Declare globals
global CHECK_GLOBAL SCALE_OPT CLIP AMPFLAG NUMBER_OF_COLORS GRAY_PCT ...
    NOBRIGHTEN COLOR_MAP PICKCOLOR XAXISTOP CLOSEREQUEST IMCONTROLS

%    1. CHECK_GLOBAL ... Controls behaviour of globals
%           CHECK_GLOBAL = [] or does not exist ... load global
%               variables from plotimageproperties.mat and check them, but do
%               not overwrite plotimageproperties.mat
%           CHECK_GLOBAL = anything ... set default global variables and
%               save them tp plotimageproperties.mat
%
sv = true; % Assume we are going to save the global variables
if isempty(CHECK_GLOBAL) && PI_LoadGlobals
    sv = false; % Don't overwrite plotimageproperites.mat
end

%    2. SCALE_OPT (default = 2) ... controls the scaling option
%           SCALE_OPT=1 ... The mean and standard deviation of all samples are computed.
%           SCALE_OPT=2 ... The maximum absolute value of the data is computed.
SCALE_OPT = checkNumericGlobal(SCALE_OPT, 2, 1:2);

%    3. CLIP ... clip level (default = 4)
CLIP = checkNumericGlobal(CLIP, 4, PI_GetClipLevels);

%    4. AMPFLAG (default 'I') ... controls the relative scaling
%           AMPFLAG = I ... Independant (default)
%           AMPFLAG = M ... Master
%           AMPFLAG = S ... Slave
AMPFLAG = checkStringGlobal(AMPFLAG,'I',{'I','M','S'});

%    5. NUMBER_OF_COLORS ... number of colors in the colormap (default 64)
NUMBER_OF_COLORS = checkNumericGlobal(NUMBER_OF_COLORS, 64, 8:1024);

%    6. GRAY_PCT ... controls the nonlinearity of the default gray scale
%         (defaults to 50, meaning 50% gray transition)
GRAY_PCT = checkNumericGlobal(GRAY_PCT, 50, 1:100);

%    7. NOBRIGHTEN ... determines brightness of the colormap
%         (defaults to 0, meaning the colormap will be brightened)
NOBRIGHTEN = checkNumericGlobal(NOBRIGHTEN, 0, 0:1);

%    8. COLOR_MAP ... name of the colormap to use
%        Name of color maps to use: see 'help graph3d' or 'listcrcolormaps'
%           COLOR_MAP = 'seisclrs' ... default CREWES seismic color map
COLOR_MAP = checkStringGlobal(COLOR_MAP, 'seisclrs', listcrcolormaps);

%    9. PICKCOLOR ... color of picks (default 'red')
%           PICKCOLOR = 'r' ... Red
%           PICKCOLOR = 'g' ... Green
%           PICKCOLOR = 'b' ... Blue
PICKCOLOR = checkStringGlobal(PICKCOLOR, 'r', {'r','g','b'});
        
%   10. XAXISTOP ... location of x axis (default 'bottom')
%           XAXISTOP = 'top'
%           XAXISTOP = 'bottom'
XAXISTOP = checkStringGlobal(XAXISTOP, 'bottom', {'top','bottom'});

%   11. CLOSEREQUEST ... control whether plotimage closes with a warning or not
%           CLOSEREQUEST = 'Slow Close' ... Check with user
%           CLOSEREQUEST = 'Fast Close' ... Just close (default)
CLOSEREQUEST = checkStringGlobal(CLOSEREQUEST, 'fast close', {'slow close','fast close'});

%   12. IMCONTROLS ... allows plotimage to begin with image controls hidden
%           IMCONTROLS = 'On'  ... Display controls (default)
%           IMCONTROLS = 'Off' ... Do not display controls
IMCONTROLS = checkStringGlobal(IMCONTROLS, 'on', {'on','off'});

% Save global variables in current directory
if sv
    PI_SaveGlobals();
end

end %end PI_CheckGlobals()

%% function check numeric global
function v = checkNumericGlobal(v, vdefault, vrange)
%
% function v = checkNumericGlobal(v, vdefault, vrange)
%   v        = numeric value to check
%   vdefault = default value to return on failure
%   vrange   = range of values to compare v to
%

eta = 0.001; %avoid having near return two indices if v is exactly halfway 
             %between two values in vrange
if isempty(v) || ~isnumeric(v)
    v = vdefault;
else
    v = vrange(near(vrange,v+eta));
end

end %end checkNumericGlobal

%% function check numeric global
function v = checkStringGlobal(v, vdefault, vrange)
%
% function v = checkStringGlobal(v, vdefault, vrange)
%   v        = value to check
%   vdefault = default value to return on failure
%   vrange   = range of values to compare v to
%
if isempty(v) || ~ischar(v)
    v = lower(vdefault);
elseif ~sum(strcmpi(v,vrange)) %can't find v in vrange
    v = lower(vdefault);
else
    v = lower(v);
end

end %end checkStringGlobal