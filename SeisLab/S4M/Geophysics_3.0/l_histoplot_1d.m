function aux=l_histoplot_1d(wlog,curve_mnem,edges,varargin)
% Uses a curve of well log "wlog" to create a 1-D histogram
%
% Written by: E. Rietsch: December 26, 2005
% Last updated:
%
%         aux=l_histoplot_1d(wlog,curve_mnem,edges,varargin)
% INPUT
% wlog    well log structure
% curve_mnem  mnemonic of the curve for which the histogram is to be computed
% edges   either the vector of histogram edges for the selected log curve
%             or the number of bins (positive integer)
%             or the bin size [negative number; bin size, then, is abs(edges)].
%         In the second case the first bin starts at the smallest sample of
%         the curve and the last bin ends at the largest sample of the curve;
%         If edges is empty or not given the number of bins is selected by
%         Sturges' rule: edges=1+log2(size(wlog.curves,1)).
%         In the third case the first bin starts at the smallest sample of the
%         curve - bin size and the last bin ends at the largest sample 
%         of the curve + bin size.
% varargin  one or more cell arrays; the first element of each cell array is
%         a keyword string, the following arguments contains a parameter(s). 
%         Accepted keywords are:
%         Default: {'style','line'}
%     'colors'    line color
%         Default: {'colors','r'}
%     'figure'     Specifies if new figure should be created or if the seismic 
%         traces  should be plotted to an existing figure. Possible values
%         are 'new' and any other string. 
%         Default: {'figure','new'} 
%     'linewidth'  only used if "style" is 'line'; width of line
%         Default: {'linewidth',3}
%     'scale'     scale the bars to represent percent
%         Default: {'scale','no'}
%     'style'   possible values are: 'line' (line representation of histogram) 
%                               and  'bar'  (bar representation of histogram)
% OUTPUT
% aux    structure with fields 'handle' (handle of histogram curve) and 
%                              'edges'  (edges of histogram bins)
%                              'nn'     (histogram values; e.g.  "mystairs(edges,nn)")
%                              'figure_handle' handle of figure
%                              'menu_handle' handle of menu button for 
%                                       figure export menu

%       Default input parameters
param.colors='r';
param.figure='new';
param.linewidth=3;
param.orient='landscape';
param.scale='no';
param.style='line';

%	Replace defaults by actual input arguments
param=assign_input(param,varargin);

if strcmpi(param.scale,'no')
   yinfo={'counts','n/a','Counts'};
else
   yinfo={'%','n/a','Percent'};
end

[col,xinfo]=l_gc(wlog,curve_mnem);

%	Set default number of bins (Sturges' formula)
if nargin < 3  || isempty(edges)
   edges=1+log2(length(col));
end


if strcmp(param.figure,'new')
   if strcmpi(param.orient,'landscape')
      figure_handle=lfigure;
   else
      figure_handle=pfigure;
   end
%   aux.menu_handle=figure_export_menu(figure_handle);
end

aux=histoplot1(col,edges,{'xinfo',xinfo},{'yinfo',yinfo},{'style',param.style}, ...
    {'linewidth',param.linewidth},{'colors',param.colors}, ...
    {'scale',param.scale});

mytitle(['Histogram of well log "',wlog.name,'" (curve "',xinfo{3},'")'])

%timeStamp

if nargout > 0
   aux.figure_handle=figure_handle;
else
   clear aux
end
