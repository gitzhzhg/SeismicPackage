function aux=s_slice_3d(seismic,varargin)
% Plot a slice of a 3-D dataset.
%
% Written by: E. Rietsch: October 6, 2006
% Last updated: October 9, 2006: Generalized to x-slices and y-slices as well
%
%           aux=s_slice_3d(seismic,varargin)
% INPUT
% seismic   3-D seismic dataset with headers such as "iline_no" and 
%           "xline_no", "CDP" and "offset" that represent surface coordinates
% varargin  one or more cell arrays; the first element of each cell array is a keyword,
%           the other elements are parameters. Presently, keywords are:
%     'colorbar' plot a colorbar; possible values are 'yes' and 'no'.
%           Only used if keyword "style" is 'color'.
%           Default: {'colorbar','yes'}
%     'colormap'  colormap to use to map seismic amplitudes to color. Can be 
%           any of those predefined in MATLAB (such as 'copper','hot'); 
%           type "help graph3d" in MATLAB for a list of color maps. 
%           In addition {'colormap','gray'} creates a gray-scale color map 
%           (the smaller the value the darker the color)
%           Default: {colormap,''}; this creates a blue-to-red colormap
%     'figure'   Specifies if new figure should be created or if the seismic
%           traces should be plotted to an existing figure. 
%           Possible values are 'new' and 'old'. 
%           Default: {'figure','new'}
%     'headers' two strings or a cell vector with two strings representing two 
%           mnemonics that describe the two horizontal dimensions of the dataset. 
%           Examples are {'iline_no','xline_no'} or {'cdp','offset'}.
%           Default: {'headers','iline_no','xline_no'}
%     'orient' Plot orientation. Possible values are: 'portrait' and 'landscape'
%           Default: {'orient','landscape'}
%     'slice'  two-element cell array. The first element is 'time' or one of  
%           the two headers specified via keyword "headers", the other is a 
%           numeric value representing the slice to display
%           Default: {'slice','time',(seismic.first+seismic.last)*0.5}
%     'style'  style of plot; possible values are: 'color','surface',
%           in future: 'mesh','meshc','waterfall','contour','histogram',
%           Default: {'style','color'}   for 3D
% OUTPUT
% aux       auxiliary info; structure whose field are handles of the display
%     figure_handle   handle of the figure
%     image_handle    handle of the image (surface, color, etc)
%
% EXAMPLE
%       seismic=s_data_3d;
%       s_slice_3d(seismic,{'slice','iline_no',110},{'style','color'})
%       s_slice_3d(seismic,{'slice','time',500},{'style','surface'})
%       s_slice_3d(seismic,{'slice','time',500},{'style','color'})


%	Set defaults of input parameters
param.colorbar=true;
param.colormap=[];
param.figure='new';
param.headers={'iline_no','xline_no'};
param.imagemenu=true;
param.orient='landscape';
param.style='color';
param.slice={'time',(seismic.first+seismic.last)*0.5};

%	Replace defaults by actual input parameters
param=assign_input(param,varargin);

if iscell(param.headers{1})
   param.headers=param.headers{1};
end

seismic=double(seismic);  % Works only for double-precision data

%       Convert 2D array to 3D array, based on header values
[vol,y,x,time,yinfo,xinfo,zinfo]=traces2volume(seismic,param.headers{2},param.headers{1});


switch param.slice{1};
case 'time'
   [nearest,idx]=nearest_intersect(param.slice{2},time);
   vslice=squeeze(vol(idx,:,:));
   zinfo={'amp','n/a','Amplitude'};

case param.headers{1}
   [nearest,idx]=nearest_intersect(param.slice{2},x);
   vslice=squeeze(vol(:,:,idx));
   x=y;
   y=time;
%   temp=xinfo;
   xinfo=yinfo;
   yinfo=zinfo;
   zinfo={'amp','n/a','Amplitude'};

case param.headers{2}
   [nearest,idx]=nearest_intersect(param.slice{2},y);
   vslice=squeeze(vol(:,idx,:));
   y=time;
%   temp=zinfo;
%   xinfo=yinfo;
   yinfo=zinfo;
   zinfo={'amp','n/a','Amplitude'};
otherwise
   error(['Unknown slice mnemonic: ',param.slice{1}])

end

%       Set up display
switch param.figure
case 'new'
   switch param.orient
   case 'landscape'
      aux.figure_handle=lfigure;
   
   case 'portrait'
      aux.figure_handle=pfigure;
   
   otherwise
      error(['Unknown plot orientation: ',param.orient])
   end
   
   bgGray

%	Button for menu to change image (color, etc. ...)
   if isyes(param.imagemenu)  &&  strcmpi(param.style,'color')
      myimagemenu     %	Create menu button to interactively change colors, etc.
   end

case 'old'
   aux.figure_handle=gcf;

otherwise
   error(['Unknown figure option: ', param.figure]) 

end

%	Set colormap
if ~isempty(param.colormap)  &&  ~strcmpi(param.colormap,'default')
   try
      cm=colormap(param.colormap);
   catch
      cm=default_seismic_colormap;
      disp('Requested colormap not found; default colormap used instead.')
   end
else
   cm=default_seismic_colormap;
end

%       Create actual plot
switch param.style
case 'color'  
   aux.image_handle=colorplot(vslice,x,y,xinfo,yinfo,zinfo);

   colormap(cm)
   axis tight 
   if isyes(param.colorbar)
      colorbar
   end

case 'surface'
   aux.image_handle=surfaceplot(vslice,x,y,{'info',[xinfo;yinfo;zinfo]});
   axis tight 
   colormap(cm)

otherwise
   error(['Unknown style option: ',param.style]);

end

mytitle(seismic.name)

if strcmp(yinfo{2},seismic.units)
   set(gca,'XAxisLocation','top')
end

if nargout == 0
   clear aux
else
   aux.axis_handle=gca;
end

box on
