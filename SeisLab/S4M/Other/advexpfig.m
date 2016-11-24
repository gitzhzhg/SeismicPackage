function [result,msg] = advexpfig(fh,fName,format,varargin)
% ADVEXPFIG Exports a figure into a file (all internal formats are
%   supported as well as some formats generated indirectly using an
%   intermediary eps format.
%
%   [result,msg] = ADVAXPFIG(fh,fName,format,varargin)
%   Creates an output from the figure fg.
%
%   - fh:           figure handle to export
%   - fName:        name of the file to create (if the extension is not
%                   given it will be appended automatically according to
%                   the format used)
%   - format:       output format: all formats that are available for the
%                   print command (e.g.:'-dbmpmono', '-dbmp16m',... ; see
%                   the help of the print function) plus some additional
%                   "indirect" formats which uses eps format as the
%                   intermediary format:
%                   'jpg<-eps', 'pdf<-eps', 'png<-eps', 'tiff<-eps'
%                   The cons of these intermediary formats are that figures
%                   have nicer look while the resulting figures have sizes
%                   dictated by the bounding box from the intermediary eps
%                   figure and not by the paper size.
%                   This is especialy suited for LaTeX users for example to
%                   use these figure when creating documents,... But, to
%                   use these intermediary formats, GhostScript must be
%                   installed (se the 'gs' parameter in varargin) as well
%                   as the EPS2XXX function must be available.
%   - varargin:    (optional) property-value pairs of additional
%                   switches/settings (note that they must come in PAIRS):
%                   'ui':       can be 'on' (default) if ui objects are to be
%                               exported as well or 'off' if not
%                   'w':        changed width of the figure in cm (e.g. 15)
%                   'h':        changed height of the figure in cm (e.g. 9)
%                   'res':      resolution in dpi; given as a number;
%                               defaults to whatever is set to be default
%                               in the Matlab's print function
%                   'colored':  1 or 0; applicable to indirect formats only
%                   'renderer': '-painters', '-opengl, '-zbuffer'; if not
%                               used, the current renderer is used
%                   'gs':       path to the GhostScript executable; used
%                               for the formats generated indirectly;
%                               defaults to 'gswin32c.exe' in win32 and to
%                               'gs' in other platforms; if the executable
%                               is not in the system's path, the full path
%                               should be given
%
%   - result:       (optional); -1: errors, no file created; 0: file(s) created but
%                   there were warnings; 1: OK (no errors or warnings)
%   - msg:          (optional); resulting status on file being processed (confirmation string , error
%                   string or warning string)
%
%   Examples:
%       advexpfig(gcf,'test.eps','-deps2c','w',15,'h',9)
%
%   Notes: for formats generated indirectly this function uses the function
%   EPS2XXX. If result and msg are given, no error is raised as this
%   variables holds the status.
%
%   See also: EPS2XXX
%
%   Primoz Cermelj, 07.10.2004
%   (c) Primoz Cermelj, primoz.cermelj@email.si
%   Last revision: 24.11.2005
%--------------------------------------------------------------------------

if nargin < 3
    error('Not enough parametrs given (at least 3 required)');
end
if ~ischar(format)
    error('Format must be given as a string');
end
[w,h,gs,colored,optStr] = setoptions(varargin);

units = get(fh,'Units');
paperMode = get(fh,'PaperPositionMode');
set(fh,'PaperPositionMode','auto');
set(fh,'Units','centimeters');
pos = get(fh,'Position');
oldPos = pos;
newPos = pos;

% Change the size of the figure
if ~isempty(w)
    newPos(3) = w;
end
if ~isempty(h)
    newPos(4) = h;
end
set(fh,'Position',newPos);

% Set the intermediary file name and format if required
if format(1) ~= '-'
    % intermediary format
    if colored
        gformat = '-deps2c';    %#ok Used in "eval"
    else
        gformat = '-deps2';     %#ok Used in "eval"
    end
    [pathstr,sourceName,ext] = fileparts(fName);
    if isempty(ext)
        ext = '.eps~';
    else
        ext = [ext '~'];
    end
    if isempty(pathstr)
        pathstr = cd;
    end
    fName = fullfile(pathstr,[sourceName ext]);
    intermediary = 1;
    switch lower(format)
        case 'jpg<-eps'
            iformat = 'jpeg';
        case 'pdf<-eps'
            iformat = 'pdf';
        case 'png<-eps'
            iformat = 'png';
        case 'tiff<-eps'
            iformat = 'tiff';
        otherwise
            error(['Unknown intermediary format: ' format]);
    end
else
    gformat = format;        %#ok Used in "eval"
    intermediary = 0;
end

% Print the figure to a file
eval(['print(fh,fName,gformat', optStr, ')']);

% Restore the original figure
set(fh,'Position',oldPos);
set(fh,'Units',units);
set(fh,'PaperPositionMode',paperMode);

result = 1;
msg = '';

% If intermediary format is used, convert the figure appropriately
if intermediary
    [res,resMsg] = eps2xxx(fName,cellstr(iformat),gs);
    % Delete the intermediary-format file
    if exist(fName,'file')
       delete(fName);
    end
    result = res;
    msg = resMsg;
    if ~nargout
        if res < 0   % error
            error(['File not created :' resMsg]);
        end
        if res == 0  % warning
           warning(['Warning :' resMsg]);  %#ok
        end
    end
end



%-----------------------------------------------------------------
function [w,h,gs,colored,optStr] = setoptions(varargin)
% Gets the w, h, gs string and the options string from the varargin
% parameters.

%Defaults
w = [];
h = [];
gs = [];
optStr = '';
colored = 0;
if isempty(varargin{:})
    return
end

% Get the options from the varargin
n = length(varargin{:});
if mod(n,2)~= 0
    error('Additional parameters must be given in property-value pairs');
end
for ii=1:2:n-1
    prop = varargin{:}{ii};
    value = varargin{:}{ii+1};
    if ~ischar(prop)
        error('Property name must be given as a string');
    end
    switch lower(prop)
        case 'ui'
            if strcmpi(value,'off')
               optStr = [optStr ',''-noui''']; %#ok
            end
        case 'w'
            w = value;
        case 'h'
            h = value;
        case 'res'
            optStr = [optStr ',''' '-r' num2str(value) '''']; %#ok
        case 'renderer'
            if ~isempty(value)
                optStr = [optStr ',''' value '''']; %#ok
            end
        case 'gs'
            gs = value;
        case 'colored'
            colored = value > 0;
        otherwise
            error('Wrong property given')
    end
end