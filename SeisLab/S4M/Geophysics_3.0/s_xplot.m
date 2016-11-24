function aux=s_xplot(seismic,varargin)
% Function plots seismic data in wiggle-trace form with a specific color scheme
% for negave values.
% By default, positive deflections are filled black.
%
% Written by E. Rietsch: January 30, 2006
% Last updated: February 27, 2006: Improve error checking
%
%             aux=s_xplot(seismic,varargin)
% INPUT
% seismic     seismic structure
% varargin    one or more cell arrays; the first element of each cell array 
%             is a keyword, the other elements are parameters. 
%             Presently, keywords are:
%     'annotation'  2-element or 3-element cell array. Defines a header 
%             mnemonic whose values are used to annotate the horizontal axis. 
%             Default: {'annotation','trace_no'}
%     'aindex annotation index; used to specify which traces to annotate with the
%             value of the header selected via keyword annotation.
%             EXAMPLE {'aindex',10:10:1000}   every tenth trace annotated, 
%                                             starting with trace 10
%             Default: {'aindex',[]}          Matlab-default annotation
%     'deflection'  trace deflection in units of trace spacing. 
%             Default: {'deflection',1.25}
%     'direction'  2-element cell array defining plot direction. Possible values are: 
%             left-to-right, 'l2r', and right-to-left, 'r2l'.  Default: {'direction','l2r')
%     'figure'     Specifies if new figure should be created or if the seismic 
%             traces  should be plotted to an existing figure. Possible values
%             are 'new' and any other string. 
%             Default: {'figure','new'} 
%     'interpol'    2-element cell array which specifies how data should be interpolated in time.
%             Possible values are 'linear', 'cubic', and 'v5cubic'. 
%             'cubic'   - piecewise cubic Hermite interpolation
%             'v5cubic'  - the cubic interpolation from MATLAB 5, which does not
%                  extrapolate and uses 'spline' if X is not equally spaced.
%             Default: {'interpol','v5cubic'}
%     'orient' Plot orientation. Possible values are: 'portrait' and 'landscape'
%             Default: {'orient','landscape'} for more than 10 traces
%                      {'orient','portrait'}  for 10 or fewer traces
%     'peak_fill'   2-element cell array which specifies color of peak fill. Possible values are 
%             all permissible colors or the empty string. In the latter case peaks are not filled. 
%             Default: {'peak_fill','k'}
%     'pixels'      2-element cell array. Minimum number of interpolation points to use for display. 
%             The greater the number the smoother the display if 'interpol' is 'cubic'. 
%             Default: {'pixels',500}
%     'polarity'    2-element cell array. Possible values are 1 and -1;
%             Default: {'polarity',1}
%     'quality'     2-element cell array. Possible values are 'draft' and 'high'
%             'draft' quality (default) is faster and intended for screen and b/w hard copies; 
%             'high' quality is for color copies (there is no difference between
%             'draft' and 'high' quality for screen displays and b/w copies).
%             Default: {'quality','high'}
%     'scale'       2-element cell array which specifies if traces should be scaled individually.
%             Possible values are 'yes', 'no', or the actual scale. This may be a scalar or a
%             vector whose number of elements is equal to the number of traces.
%             separation. The scale actually used must be obtained by specifying the output
%             argument "aux".
%             Default: {'scale','no'}
%     'spacing'     2-element cell array which specifies if traces should be   
%             equidistant ('equal') or non uniformly ('nonequal') spaced; 
%             in the latter case the header mnemonic used for annotation defines 
%             the trace-to-trace separation.
%             Default: {'spacing','equal'}
%     'times'       3-element cell array 
%             {'times',vector of first and last time to plot} or ('times',first,last}. 
%             Default: {'times',seismic.first,seismic.last} which is
%                      equivalent to {'times',[seismic.first,seismic.last]}
%     'title'       2-element cell array consisting of the keyword 'title' and a title string;
%             no title is plotted if the title string is empty.
%             Default: {'title',seismic.name)
%     'traces'      2-element or 3-element cell array. The second element can be an array of 
%             trace numbers or it can be a string. If it is a string it can be a header 
%             mnemonic or it can contain a logical expression involving header values to 
%             include. A "pseudo-header" 'trace_no' can also be used.
%             If the second element is a string containing a header mnemonic there must 
%             be a third element containing a vector of values. (see "s_select")
%             Default:  {'traces',[]} which is equivalent to 
%                       {'traces',1:ntr} where ntr denotes the number of traces in the 
%                              input data set (ntr = size(seismic.traces,2))
%     'tracking' track cursor position; possible values are 'yes', 'no', and ''.
%             In the latter case a tracking button is created if the the
%             seismic is plotted in a new figure. Otherwise it is not.
%             Default: {'tracking',''}
%     'trough_fill' 2-element cell array which specifies color of trough fill. 
%             Possible values are all permissible colors or the empty string. 
%             In the latter case troughs are not filled. 
%             Default: {'trough_fill',''}
%     'wiggle_color'      2-element cell array which specifies color of wiggles. 
%             Possible values are all permissible colors or the empty string. 
%             In the latter case wiggles are not plotted. 
%             Default: {'wiggle_color','k'}
%
% OUTPUT
% aux        optional structure with scale factor(s) used for wiggle plot 
%            (required if seismic data are to be plotted with the same scaling
%             in different plots)
%        'scale'          field with scale factor(s) used in wiggle plot          
%        'figure_handle'  handle of the figure with the plot 
%
% EXAMPLE
%        s_xplot(s_data)


global PARAMETERS4FUNCTION

% aux=[];

if ~isstruct(seismic)
  seismic=s_convert(seismic,1,1);
  seismic.units='Samples';
end

ntr=size(seismic.traces,2);

%     Set default values
param.annotation='trace_no';
if ntr < 11
   param.aindex=1:ntr;
elseif ntr < 21
   param.aindex=1:2:ntr;
else
   param.aindex=[];
end
param.deflection=1.25;
param.direction='l2r';
param.figure='new';
% param.interactive=1;
param.interpol='v5cubic';
param.orient=[];
param.peak_fill='k';
param.pixels=500;
param.polarity=1;
param.quality='high';
param.scale='no';
param.spacing='equal';
param.times=[];
param.title=strrep(seismic.name,'_','\_');
param.traces=[];
param.tracking='yes';
param.trough_fill='';
param.wiggle_color='k';
param.wiggle_width=0.5;

%       Replace default values by input arguments
param=assign_input(param,varargin,'s_wplot');

n=32;
v=(0:1:n-1)'/(n-1);
o=ones(n,1);
z=zeros(n,1);
mycolormap=[o,v,z; o,o,v];
smin=min(seismic.traces(:));

if ~isempty(param.times)
   if isnumeric(param.times) 
      if length(param.times) == 1
         disp(' Only one time (instead of a time interval) specified. No plot created.')
         return
      elseif length(param.times) ~= 2
         disp(' More than two times specified.')
         return
      end
   elseif iscell(param.times)
      param.times=cell2mat(param.times);
   else
      disp('Unknown time-range specification.')
      return
   end
end

s_cplot(s_resample(seismic,seismic.step),{'colormap',mycolormap}, ...
       {'limits',0.6*smin,0},{'imagemenu','no'},{'tracking','no'}, ...
       {'figure',param.figure},{'polarity',param.polarity}, ...
       {'direction',param.direction},{'scale',param.scale}, ...
       {'traces',param.traces},{'times',param.times}, ...
       {'colorbar','no'})
hold on

%       Set parameters for wiggle plot
param.figure='old';
PARAMETERS4FUNCTION.s_wplot.default=param;

if nargout > 0
   aux=s_wplot(seismic);
else
   s_wplot(seismic)
end
