function aux=s_plot_with_header(seismic,varargin)
% Function plots one or more header values above a seismic data set in 
% wiggle-trace form or as a color-image.
%
% Written by: E. Rietsch: August 1, 2005
% Last updated: March 20, 2009: Make plots contiguous; make x-axis annotation of
%                               seismic and header plot the same 
% 
%           aux=s_plot_with_header(seismic,varargin)
% INPUT
% seismic   seismic data set
% varargin  one or more cell arrays; the first element of each cell array is a 
%           keyword, the other elements are parameters. Presently, keywords are:
%      'aindex annotation index; used to annotate specific traces with the
%           value of the header selected via keyword annotation. 
%           (disregarded for color plots)
%           Example: {'aindex',10:10:1000}   every tenth trace annotated, 
%                                            starting with trace 10
%           Default: {'aindex',[]}     this means 1:fix(ntr/10)+1:ntr
%                                      where "ntr" denotes the number of traces                                                       
%      'annotation'    mnemonic of the header to use for trace annotation
%           Default: {'annotation','trace_no'}
%      'colors'    colors to use for the header plot
%           Default: {'colors',[]}   This implies the default color sequence
%                                    'red','blue','green', ...
%      'deflection'    Trace deflection (multiple of trace-to-trace separation)
%           (disregarded for color plots)
%           Default: {'deflection',1.25}
%      'direction'  2-element cell array defining plot direction. Possible values
%           are: left-to-right, 'l2r', and right-to-left, 'r2l'.
%           Default: {'direction','l2r')
%      'headers2plot'  header mnemonic or cell array with the mnemonics of 
%           the headers to plot
%           Default: {'headers2plot','rms'}
%      'header_location'  location of header relative to the seismic; two 
%           possible values: 'above' and 'below'
%           Default: {'header_location','above'}   the header plot is above
%                                                  the seismic plot
%      'height_ratio'   Ratio of header window to seismic window.
%           Default: {'height_ratio',0.5}   this means the header window is
%                                           half as high as the seismic window
%      'interpol'    2-element cell array which specifies how data should be 
%           interpolated in time. For wiggle plot only.
%           Possible values are: 'linear', 'cubic', and 'v5cubic'.              
%             'cubic'    - piecewise cubic Hermite interpolation
%             'v5cubic'  - the cubic interpolation from MATLAB 5, which does not
%                  extrapolate and uses 'spline' if X is not equally spaced.
%           Default: {'interpol','v5cubic'}
%      'legendposition' position of the legend in header window; see "help legend"
%           Default: {'legendposition',1}
%      'linestyle'  style of line(s) for header plot
%                                  '-'      solid;
%                                  '--'     dashed line;
%                                  '.'      dotted line;
%                                  'none'   no connecting lines between markers;
%                                  for other options see the help for "plot"
%           Default: {'linestyle','-'}
%      'linewidth'  width of lines in header plot
%           Default: {'linewidth',1}
%      'linkedzoom'  possible values are 'on' and 'off'. If linked zoom is on 
%           legends will disappear; they can be recreated with 
%           legend(aux.legend_handle,aux.legend_text); but then the
%           linked zoom will no longer be in effect
%           Default: {'linkedzoom','on'}
%      'marker'  marker to use in header plot; see help for function "plot"
%           Default: {'marker','s'}
%      'orient'  Figure orientation; possible values are 'portrait' and 'landscape'
%           Default: {'orient','landscape'}
%      'scale'   Specifies if traces should be scaled individually or not
%           Possible vales are 'yes' and 'no'.
%           Default: {'scale','no'}
%      'shading'  color shading (see Matlab function "shading")
%           possible values are '', 'flat', 'faceted', 'interp'
%           if not empty function "pcolor" is used to create the plot
%           with the type of shading specified. Shading 'interp" tends to 
%           give a more continuous display.
%           Otherwise, function imagesc" is used. This is faster.
%           Default: {'shading',''}
%      'times' first and last time to plot
%           Default: {'times',[]}    this is equivalent to
%                    {'times',seismic.first,seismic.last}
%      'type'  type of seismic plot; possible values are 'wiggle_color' and 'color'
%             Default{ {'type',[]}     this means:
%                    'wiggle_color'  if number of traces <= S4M.ntr_wiggle2color  
%                    'color'   if number of traces  > S4M.ntr_wiggle2color
%      'title'  string with the plot title
%           Default: {'title',seismic.name}
%      'xll'    x-label location; possible values are:
%           'bottom' labels are along all top axes
%           'top'    labels are along all bottom axes
%           'both'   labels are along the top and the bottom axes
%           'talternate'  labels are alternating between the top and the bottom
%                    axes,beginning with the top axis
%           'balternate'  labels are alternating between the bottom and the top
%                    axes, beginning with the bottom axis
%           Default: {'xll','both')  
%      'yll'     y-label location; possible values are:
%           'left'   labels are along all left axes
%           'right'  labels are along all right axes
%           'both'   labels are along the left and the right axes
%           'lalternate'  labels are alternating between the left and the right
%                    axes,beginning with the left axis
%           'ralternate'  labels are alternating between the right and the left
%                    axes, beginning with the left axis
%           Deafault: {'yll','ralternate'}  
% OUTPUT
% aux       structure with handles of plot elements
%     figure_handle      Figure handle
%     axis_handles       Handles of the two axes
%     legend_handle      Handle of the legend
%     legend_text        Text of legend
%     line_handles       Handles of the header curves plotted
%     title_handle       Handle of title (if one was created)
%
% EXAMPLE
%     seismic=s_data;
%     s_plot_with_header(seismic,{'headers2plot','cdp'},{'annotation','CDP'})
%     s_plot_with_header(seismic,{'headers2plot','cdp'},{'annotation','CDP'}, ...
%             {'type','color'},{'header_location','below'},{'shading','interp'})

% UPDATE HISTORY
%     January 30, 2007: Add option for color plot
%     October 26, 2007: Add option to place header plot above and 
%                                 below seismic plot; bug fix


global S4M

param.aindex=[];
param.annotation='trace_no';
param.colors=[];
param.deflection=1.25;
param.direction='l2r';
param.headers2plot='rms';
param.header_location='above';
param.height_ratio=0.25; % Ratio of header window to seismic window;
param.interpol='v5cubic';
param.legendposition=1;
param.linestyle='-';
param.linewidth=1;
param.linkedzoom='yes';
param.marker='s';
param.orient='landscape';
param.scale='no';
param.shading='';
param.type=[];
param.times=[];
param.title=seismic.name;
param.traces=[];
param.xll='both';
param.yll='ralternate';

%       Replace defaults by input parameters
param=assign_input(param,varargin);

%       Select requested subset of traces
if ~isempty(param.traces)
   seismic=s_select(seismic,{'traces',param.traces});
end

ntraces=size(seismic.traces,2);

%       Select default trace annotation indices
if isempty(param.aindex)
   param.aindex=1:fix(ntraces/10)+1:ntraces;
end

%       Select default plot type (color or wiggle)
if isempty(param.type)
   if ntraces <= S4M.ntr_wiggle2color
      param.type='wiggle_color';
   else
      param.type='color';
   end
end

if strcmpi(param.orient,'landscape')
   figure_handle=lfigure;
else
   figure_handle=pfigure;
end
bgGray

%       Create subplot axes
ratio=param.height_ratio;
switch param.header_location
   case 'above'
      handles=mysubplot([ratio,1],1);
      haxis1=handles(1);         % Handle for axes of the header subplot
      haxis2=handles(2);         % Handle for axes of the seismic subplot
   case 'below'
      handles=mysubplot([1,ratio],1);
      % handles=handles([2,1]);
      haxis1=handles(2);         % Handle for axes of the header subplot
      haxis2=handles(1);         % Handle for axes of the seismic subplot

   otherwise
      alert(['Unknowwn header-plot location parameter "',param.header_location,'"; "top" will be used.'])
      handles=mysubplot([ratio,1],1);
      haxis1=handles(1);         % Handle for axes of the header subplot
      haxis2=handles(2);         % Handle for axes of the seismic subplot
end


axes(haxis1)

headers2plot=param.headers2plot;
if ischar(headers2plot)
   headers2plot={headers2plot};
end
nheaders=length(headers2plot);

if strcmpi(param.type,'wiggle_color')
   x=[(1-param.deflection),1:ntraces,ntraces+(param.deflection)];
else
   x=s_gh(seismic,param.annotation);
   x=[x(1)-(x(2)-x(1))*0.5,x,x(end)+(x(end)-x(end-1))*0.5];
end
if strcmpi(param.direction,'r2l')
   x=x(end:-1:1);
elseif ~strcmpi(param.direction,'l2r')
   error(['Unknown plot direction: ',param.direction])
end

ltext=cell(nheaders,1);
hline=zeros(nheaders,1);

%% 	Plot headers
units=cell(nheaders,1);
for ii=1:nheaders
   hvalue=[NaN,s_gh(seismic,headers2plot{ii}),NaN];   
   hline(ii)=line(x,hvalue,'Color',get_color(ii,param.colors), ...
     'LineWidth',param.linewidth,'Marker',get_marker(ii,param.marker), ...
     'MarkerfaceColor',get_color(ii,param.colors),'LineStyle',param.linestyle);  
   ltext{ii}=strrep(headers2plot{ii},'_','\_');
   units{ii}=s_gu(seismic,headers2plot{ii});
end
grid on
axis tight

%       If all headers have the same units of measurement then use them 
%       to label the y-axis
units=unique(units);
if length(units) == 1
   ylabel(units2tex(units{1}))
end

if ~isempty(param.title)
   aux1=mysuptitle(mnem2tex(param.title));
end
box on

%%     Plot seismic data

axes(haxis2)   % Set axes for the seismic data set
switch param.type
case 'wiggle_color'
   s_wplot(seismic,{'aindex',param.aindex},{'figure','old'}, ...
        {'orient',param.orient},{'quality','high'},{'times',param.times}, ...
        {'scale',param.scale},{'deflection',param.deflection}, ...
        {'annotation',param.annotation},{'title',''}, ...
        {'direction',param.direction},{'interpol',param.interpol})

case 'color'
   s_cplot(seismic,{'figure','old'}, ...
        {'orient',param.orient},{'scale',param.scale},{'times',param.times}, ...
        {'annotation',param.annotation},{'title',''},{'colorbar','no'}, ...
        {'direction',param.direction},{'shading',param.shading})
       
otherwise
   error(['Unknown plot type: ',param.type])

end

%     Copy x-axis labels from the seismic subplot and use them for th header
%     subplot
xa=get(haxis2,'XTickLabel');
xt=get(haxis2,'XTick');
xlim=get(haxis2,'XLim');
set(haxis1,'XTickLabel',xa,'XTick',xt,'XLim',xlim);


if strcmpi(param.linkedzoom,'yes')
   linkaxes([haxis1,haxis2],'x')
end

legend_handle=legend(haxis1,ltext,param.legendposition);

if nargout > 0
   aux.figure_handle=figure_handle;
   aux.axis_handles=[haxis1,haxis2];
   aux.legend_handle=legend_handle;
   aux.legend_text=ltext;
   aux.line_handles=hline;
   try
      aux.title_handle=aux1.title_handle;
   catch
   end
end

axis_label_location(handles,param.xll,param.yll)
