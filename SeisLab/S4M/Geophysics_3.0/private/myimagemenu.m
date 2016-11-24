function myimagemenu(figure_handle)
%IMAGEMENU adds a context menu to change image properties
%   IMAGEMENU(handle) creates a context menu for all images with the parent
%   handle that allows image properties to be easily changed.
%   IMAGEMENU, without any input arguments, will create this context menu
%   for all images that are found in the current figure.
%   This allows users to easily change image properties, and is especially
%   useful for compiled programs, as users do not have access to MATLAB's
%   property editor.
%
% Last updated: January 23, 2008: Add colormap for PDFs
%
% EXAMPLE
%   figure
%   imagesc(peaks)
%   axis image
%   myimagemenu

if nargin == 0
    % Use all images in current figure as default
    figure_handle = gcf;
end

handle=findobj(figure_handle,'Tag',['image_menu',num2str(figure_handle)]);
if ~isempty(handle)     % Menu button exists already
   return
end

bh=uimenu('Label','Modify display','Separator','on','Tag',['image_menu',num2str(gcf)], ...
   'ForeGroundColor',[0 0 1]);

%% handle = findobj(handle, 'type', 'image');

% Define the context menu
cmenu=bh;

% Define the context menu items
colormapmenu = uimenu(cmenu, 'Label', 'Colormap');
uimenu(cmenu, 'Label', 'Reverse current colormap', 'Callback', 'colormap(flipud(colormap))');
uimenu(cmenu, 'Label', 'Toggle colorbar on/off', 'Callback', @togglecolorbar);
if exist('pixval.m','file')
    % Only show this to those who have it installed...help exist
    uimenu(cmenu, 'Label', 'Toggle pixel values', 'Callback', 'pixval');
end
uimenu(cmenu, 'Label', 'Colormap length...', 'Callback', @colormaplength);
uimenu(cmenu, 'Label', '3D plot...', 'Callback', @call3d);
uimenu(cmenu, 'Label', 'Image limits...', 'Callback', @imagelimits);
uimenu(cmenu, 'Label', 'Title...', 'Callback', @titlecallback);
uimenu(cmenu, 'Label', 'X-axis label...', 'Callback', @xaxiscallback);
uimenu(cmenu, 'Label', 'Y-axis label...', 'Callback', @yaxiscallback);

% Define colormap choices
uimenu(colormapmenu, 'Label', 'Seismic', 'Callback', 'colormap(mycolormap(''seismic''))');
uimenu(colormapmenu, 'Label', 'Dark seismic', 'Callback', 'colormap(mycolormap(''dark_seismic''))');
uimenu(colormapmenu, 'Label', 'PDF', 'Callback', 'colormap(mycolormap(''pdf''))');
uimenu(colormapmenu, 'Label', 'Jet',     'Callback', 'colormap(jet)');
uimenu(colormapmenu, 'Label', 'Gray',    'Callback', 'colormap(gray)');
uimenu(colormapmenu, 'Label', 'Hot',     'Callback', 'colormap(hot)');
uimenu(colormapmenu, 'Label', 'Bone',    'Callback', 'colormap(bone)');
uimenu(colormapmenu, 'Label', 'Cool',    'Callback', 'colormap(cool)');
uimenu(colormapmenu, 'Label', 'Color cube', 'Callback', 'colormap(colorcube)');
uimenu(colormapmenu, 'Label', 'HSV',     'Callback', 'colormap(hsv)');
uimenu(colormapmenu, 'Label', 'Pink',    'Callback', 'colormap(pink)');
uimenu(colormapmenu, 'Label', 'Prism',   'Callback', 'colormap(prism)');
uimenu(colormapmenu, 'Label', 'VGA',     'Callback', 'colormap(vga),colorbar,colorbar'); % make sure that the colorbar, if it exists, is correct
uimenu(colormapmenu, 'Label', 'Spring',  'Callback', 'colormap(spring)');
uimenu(colormapmenu, 'Label', 'Summer',  'Callback', 'colormap(summer)');
uimenu(colormapmenu, 'Label', 'Autumn',  'Callback', 'colormap(autumn)');
uimenu(colormapmenu, 'Label', 'Winter',  'Callback', 'colormap(winter)');

% And apply menu to handle(s)
% set(handle, 'uicontextmenu', cmenu);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Menu callback
function togglecolorbar(varargin)

%	Do we have a colorbar now?
phch = get(findall(gcf,'type','image','tag','TMW_COLORBAR'),{'parent'});


if isempty(phch)
   colorbar
else
   delete(phch{1})
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Menu callback
function colormaplength(varargin)
cmap = colormap;
oldlength = length(cmap);
clength = cellstr(num2str(oldlength));
new = inputdlg({'Enter new colormap length:'},'New colormap length', 1, clength);
if isempty(new) || ~isnumeric_str(new{1})
   return
end
newlength = str2double(new{1});
oldsteps = linspace(0, 1, oldlength)';
newsteps = linspace(0, 1, newlength)';
newmap = zeros(newlength, 3);

for ii=1:3
    % Interpolate over RGB spaces of colormap
   newmap(:,ii) = min(max(interp1(oldsteps, cmap(:,ii), newsteps), 0), 1);
end
colormap(newmap);
% And update the colorbar, if one exists
phch = get(findall(gcf,'type','image','tag','TMW_COLORBAR'),{'parent'});
for ii=1:length(phch)
   phud = get(phch{ii},'userdata');
   if isfield(phud,'PlotHandle')
      if isequal(gca, phud.PlotHandle)
         colorbar
      end
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Menu callback
function imagelimits(varargin)
lims = get(gca, 'CLim');
oldlower = num2str(lims(1));
oldupper = num2str(lims(2));
new = inputdlg({'Enter new lower limit:', 'Enter new upper limit:'}, ...
     'New image limits', 1, {oldlower, oldupper});
if isempty(new)
   return
end
if ~isnan(str2double(new{1})) && ~isnan(str2double(new{2}))
   set(gca, 'CLim', [str2double(new{1}) str2double(new{2})]);
end

%       And update the colorbar, if one exists
phch = get(findall(gcf,'type','image','tag','TMW_COLORBAR'),{'parent'});

if ~isempty(phch)
   colorbar
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Menu callback
function titlecallback(varargin)

global S4M

old = get(gca, 'title');
oldstring = get(old, 'string');
if ischar(oldstring)
   oldstring = cellstr(oldstring);
end
new = inputdlg('Enter new title:', S4M.title, 1, oldstring);
if ~isempty(new)
   set(old, 'string', new);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Menu callback
function xaxiscallback(varargin)

global S4M

old = get(gca, 'xlabel');
oldstring = get(old, 'string');
if ischar(oldstring)
   oldstring = cellstr(oldstring);
end
new = inputdlg('Enter new X-axis label:', S4M.title, 1, oldstring);
if ~isempty(new)
   set(old, 'string', new);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Menu callback
function yaxiscallback(varargin)

global S4M

old = get(gca, 'ylabel');
oldstring = get(old, 'string');
if ischar(oldstring)
   oldstring = cellstr(oldstring);
end
new = inputdlg('Enter new Y-axis label:', S4M.title, 1, oldstring);
if ~isempty(new)
   set(old, 'string', new);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Menu callback
function call3d(varargin)
ax = gca;
%temp = double(get(gco, 'CData'));
handle=findobj(ax,'Tag','image_displayed');
temp=get(handle,'CData');

forient=get(gcf,'PaperOrientation');
switch forient
case 'landscape'
   newfig=lfigure;
case 'portrait'
   newfig=pfigure;
end

if isempty(get(get(ax, 'Parent'), 'Name'))
    set(newfig, 'Name','3D view');
else
    set(newfig, 'Name', [get(get(ax, 'Parent'), 'Name') ', 3D view']);
end
surf(temp, 'LineStyle', 'none');
camlight;
xlabel('X distance [pixels]');
ylabel('Y distance [pixels]');
axis('tight');
box on
