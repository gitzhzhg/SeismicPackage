function [seismic,aux]=s_select_polygon(seismic,polygon,varargin)
% Select subset of a 3-D data set either inside or outside a polygonal region
%
% Written by: E. Rietsch: July 20, 2005
% Last updated:
%
%            [seismic,aux]=s_select_polygon(seismic,polygon,varargin);
% INPUT
% seismic    3-D seismic data set
% polygon    two-column matrix with x-coordinates and y coordinates of 
%            polygon corners; the polygon is closed by connecting the 
%            first and last polygon corner
% varargin   one or more cell arrays; the first element of each cell array is a
%            keyword,the other elements are parameters. Presently, keywords are:
%     'headers'  mnemonics of two headers representing the x-coordinate and 
%            y-coordinate of the data set.
%            Default: {'headers','iline_no','xline"no'}
%     'inout'  Select inside or outside of polygon. Possible values are 'in' 
%            and 'out'.
%            Default: {'inout','in'}
%     'orient'  Figure orientation of the base map (if requested). 
%            Possible values are 'landscape' and 'portrait'.
%            Default: {'orient','landscape'}
%     'plot' Plot the base map of selected traces. Possible values are 'yes' and 'no'.
%            Default: {'plot','yes'}
%     'marker'  Marker to use to mark a trace location (see help plot for 
%            possible markers)
%            Default: {'marker','.'}
% OUTPUT
% seismic   selected seismic traces
% aux       structure with auxiliary information
%     'figure_handle' figure handle of the base map of selected traces.


%     Set default values
param.headers={'iline_no','xline_no'};
param.inout='in';
param.orient='landscape';
param.plot='yes';
param.marker='.';

%       Decode and assign input arguments
param=assign_input(param,varargin);

[np,mp]=size(polygon);
if mp ~= 2
   error('"polygon" must be a two-column array.')
end
if np < 3
   error('"polygon" must have at least three rows.')
end

%       Get requested header values
[iline,infox]=s_gh(seismic,param.headers{1});
[xline,infoy]=s_gh(seismic,param.headers{2});

in=inpolygon(iline,xline,polygon(:,1),polygon(:,2));
if strcmpi(param.inout,'out')
   in=~in;
   red=['g',param.marker];
   green=['r',param.marker];
else
   red=['r',param.marker];
   green=['g',param.marker];
end
traces=1:length(iline);

if ~any(in)
   error('No traces within polygonal region.')
end
seismic=s_select(seismic,{'traces',traces(in)});


%       Make plot if requested
if strcmpi(param.plot,'yes')
   if strcmpi(param.orient,'landscape')
      aux.figure_handle=lfigure;
   else
      aux.figure_handle=pfigure;
   end
   plot(iline,xline,red)
   hold on
   plot(iline(in),xline(in),green)
   xlabel(infox{3})
   ylabel(infoy{3})
   mytitle('Basemap: selected (green) and discarded data (red)')
   axis equal
   grid on
%   aux.figure_handle=figure_handle;

else
   if nargout > 1
      aux=[];
   end
end
