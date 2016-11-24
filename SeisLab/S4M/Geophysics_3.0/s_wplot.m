function aux=s_wplot(seismic,varargin)
% Plot seismic data in wiggle-trace, variable-area form or as spikes at the
% sample times (the latter option is meant for the display of reflection coefficients)
%
% See also: s_cplot, s_plot, s_compare
%
% Written by: E. Rietsch: April 15, 2000
% Last updated: July 15, 2009: Replace two menu buttons by one menu. Replace
%                              keyword "wiggle" by "wiggle_color"
% 
%             aux=s_wplot(seismic,varargin)
% INPUT
% seismic     seismic structure
% varargin    one or more cell arrays; the first element of each cell array 
%             is a keyword, the other elements are parameters. 
%             Presently, keywords are:
%     'annotation'  2-element or 3-element cell array. Defines a header 
%             mnemonic whose values are used to annotate the horizontal axis. 
%             Default: {'annotation','trace_no'}
%     'aindex' annotation index; used to specify which traces to annotate with the
%             value of the header selected via keyword annotation.
%             Example {'aindex',10:10:1000}   every tenth trace annotated, 
%                                             starting with trace 10
%                     It is not an error if the upper limit of 'aindex'
%                     (in this case 1000) exceeds the number of traces to
%                     be plotted.
%             Default: {'aindex',[]}          use Matlab-default annotation
%     'background'  background color; possible options are the standard 
%             colors (or their abbreviations) and gray.
%             Default: {'background','white'}
%     'deflection'  trace deflection in units of trace spacing. 
%             Default: {'deflection',1.5} for seismic data
%     'direction'  2-element cell array defining plot direction. Possible values are: 
%             left-to-right, 'l2r', and right-to-left, 'r2l'.
%             Default: {'direction','l2r')
%     'figure_only' specifies if only a figure window should be created; possible values:
%             'yes', 'no'. 
%             Default: {'figure_only','no'}
%     'figure'     Specifies if new figure needs to be created or if the seismic 
%             traces  should be plotted to the current axes of an existing 
%             figure. Possible values are 'new' and any other string. 
%             Default: {'figure','new'}
%     'fontsize' Size of the font for axis annotations, labels, and legends.
%             Default: {'fontsize',11}
%     'interpol'    2-element cell array which specifies how data should be interpolated in time.
%             Possible values are 'linear', 'cubic', and 'v5cubic'. 
%             'cubic'   - piecewise cubic Hermite interpolation
%             'v5cubic'  - the cubic interpolation from MATLAB 5, which does not
%                  extrapolate and uses 'spline' if X is not equally spaced.
%             Default: {'interpol','v5cubic'}
%     'orient' Plot orientation. Possible values are: 'portrait' and 'landscape'
%             Default: {'orient','landscape'} for more than 10 traces
%                      {'orient','portrait'}  for 10 or fewer traces
%     'peak_fill'   2-element cell array which specifies color of peak fill. 
%             Possible values are all permissible colors or the empty string. 
%             In the latter case, peaks are not filled. 
%             Default: {'peak_fill','black'}
%     'pixels'      2-element cell array. Minimum number of interpolation 
%             points to use for display. The greater the number the smoother
%             the display if 'interpol' is 'cubic'. 
%             Default: {'pixels',500}
%     'polarity'    2-element cell array. Possible values are 1 and -1;
%             Default: {'polarity',1}
%     'quality'     2-element cell array. Possible values are 'draft', 'high',
%             and 'spikes'.
%             Values 'draft' and 'high' display the data in form of wiggle traces.
%             'draft' quality (default) is faster and intended for screen
%                     and b/w hard copies; 
%             'high' quality is for color hard copies (there is no visual
%                    difference between 'draft' and 'high' quality for 
%                    screen displays and b/w copies).
%             'spikes' puts a spike at each sample location (intended
%                    for the didplay of reflection coefficients)
%             Default: {'quality','draft'}
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
%     'times'       2-element or 3-element cell array 
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
%     'wiggle_color' 2-element cell array which specifies color of wiggles. 
%             Possible values are all permissible colors or the empty string. 
%             In the latter case wiggles are not plotted. 
%             Default: {'wiggle_color','black'} 
%     'wiggle_width' line width of wiggles; default depends on the "quality
%             factor"
%             Default: {'wiggle_width',0.5} if the "quality" keyword is not
%                                           "spikes"
%                      {'wiggle_width',max(0.25,125/number_of_samples)} if the 
%                                           "quality" keyword is "spikes" 
%
% OUTPUT
% aux        optional structure with scale factor(s) used (required if seismic
%            data are to be plotted with the same scaling in different plots)
%            'scale'   field with scale factor(s) used           
%            'figure_handle'  handle of the figure with the plot 
%
% EXAMPLES
%            seismic=s_data;
%            aux=s_wplot(seismic,{'annotation','cdp'},{'direction','r2l'});
%
%            %    Display matrix as spikes (also used to show reflection coefficients)
%            s_wplot(randn(100,3),{'quality','spikes'})

% UPDATE HISTORY
%            September 1, 2007: bug fix in cursor tracking
%            November 27, 2007: streamlined scale factor computation and annotation
%            February 5,  2008: Fix bug in handling of certain null values
%            March 30, 2008: Save handles of traces in output structure "aux"
%            October 24, 2008: Add option to plot samples as spikes (to better 
%                        display reflection coefficients; see keyword "quality")           
%            November 8, 2008: Make spike-width depend on number of samples;
%                           make the spike-display the default for matrix input
%            January 19, 2009: Improve annotation of matrix display
%            March 7, 2009: Make axes and axis annotations bold
%            March 8, 2009: Make size of axis-related fonts an input parameter
%            July 12, 2009: Reduce default of "wiggle_width" if parameter "quality"
%                              is 'spike'.

global S4M


if ~istype(seismic,'seismic')
   if isnumeric(seismic)
      seismic=s_convert(seismic,1,1);
      seismic.units='Rows';
      isseismic=false;
      seismic=add_header(seismic,1:size(seismic.traces,2),{'columns','n/a','Columns'});
   else
      error('First input argument must be a seismic dataset or a matrix.')
   end
else
   isseismic=true;
end

run_presets_if_needed


%%     Set default values
if isseismic
   param.annotation='trace_no';
else
   param.annotation='columns';
end
param.aindex=[];
param.background='white';
param.deflection=[];
param.direction='l2r';
param.doublebuffer='on';
param.figure='new';
param.figure_only='no';
param.first=1;
param.fontsize=11;
%param.inc=[];
%param.interactive=1;
param.interpol='v5cubic';
param.orient=[];
param.peak_fill='black';
param.pixels=800;
param.polarity=1;
param.quality='';
param.scale='no';
param.spacing='equal';
param.times=[];
param.title=strrep(seismic.name,'_','\_');
try
   param.title(1)=upper(param.title(1));
catch
    % do nothing
end
param.traces=[];
param.tracking='';
param.trough_fill='';
param.wiggle='';    % deprecated; replaced by 'wiggle_color'
param.wiggle_color='black';
param.wiggle_width=[];

%       Replace defaults by actual input arguments
param=assign_input(param,varargin,'s_wplot');


%%    Select requested subset of seismic data
if ~isempty(param.traces)  ||  ~isempty(param.times)
   seismic=select_subset_no1(seismic,param);
end

%%       Check seismic input data to see if they can be plotted
[nsamp,ntr]=size(seismic.traces);
if nargout > 0
   aux=[];
end

if nsamp <= 1
   disp([' Only one sample per trace;  "s_wplot" did not plot the dataset "',seismic.name,'".'])
   return
elseif ntr == 0
   disp([' Alert from "s_wplot": Data set "',seismic.name,'" has no traces to plot.'])
   return
end


%% Set not-yet defined input parameters
if isseismic
   param=set_undefined_parameters4seismic_no2(param,seismic,nsamp);
else
   param=set_undefined_parameters4matrix_no3(param,seismic,nsamp);
end

if ~isempty(param.wiggle)
   param.wiggle_color=param.wiggle;
end

%%       Set indices of trace headers for annotation
if ntr < 21  && isempty(param.aindex)
   if ntr < 11
      param.aindex=1:ntr;
   else
      param.aindex=1:2:ntr;
   end
end

if isempty(param.aindex)
   indices=1:max([round(ntr/7),1]):ntr;
else
   indices=param.aindex(param.aindex > 0 & param.aindex <= ntr);
end


%%    Handle special cases of color for seismic traces
if strcmp(param.peak_fill,'gray')
   param.peak_fill=[0.6,0.6,0.6];
elseif strcmp(param.peak_fill,'none')
   param.peak_fill=[];
end

if strcmp(param.trough_fill,'gray')
   param.trough_fill=[0.6,0.6,0.6];
elseif strcmp(param.trough_fill,'none')
   param.trough_fill=[];
end

if strcmp(param.wiggle_color,'gray')
   param.wiggle_color=[0.6,0.6,0.6];
elseif strcmp(param.wiggle_color,'none')
   param.wiggle_color=[];
end

annotation=s_gh(seismic,param.annotation);
if ntr == 1  || isempty(annotation)
   uniform=true;
   annotlog=false;
else
   ddd=diff(annotation);
   annotlog=(max(ddd)==min(ddd)) && ddd(1) ~= 0 && isempty(param.aindex);
   if annotlog
      uniform=false;
   else
      if strcmp(param.spacing,'equal')
         uniform=true;        % Uniform trace-to-trace distance
      else
         uniform=false;
      end
   end
end


%     Change polarity if requested
if param.polarity < 0
   seismic.traces=-seismic.traces;
end

%       Check if there are null values in the seismic data
isnullvalue=isnull(seismic);

%     Interpolate data if necessary
if (strcmpi(param.interpol,'v5cubic') || strcmpi(param.interpol,'cubic')) && ...
        nsamp < param.pixels  &&  ~strcmp(param.quality,'spikes')
   npix=round(param.pixels/(nsamp-1))*(nsamp-1);
   times=linspace(seismic.first,seismic.last,npix)';
   times_orig=linspace(seismic.first,seismic.last,nsamp)';
   times=unique([times;times_orig]);
  
   if isnullvalue  &&  S4M.matlab_version >= 7 % Turn off warnings caused by NaN's in seismic traces
%      warning('off','MATLAB:interp1:NaNinY')
      yi=nan_interp1(times_orig,seismic.traces,times,param.interpol,0);
      for ii=1:ntr           % If necessary, replace last NaN at the top and 
                             % first NaN at the bottom by zero to terminate
			     % patches 
         idx=find(~isnan(yi(:,ii)));
         if ~isempty(idx) 
            if idx(1) > 1 
	            if (yi(idx(1),ii) > 0  && ~isempty(param.peak_fill))  || ...
                  (yi(idx(1),ii) < 0  && ~isempty(param.trough_fill))
                  yi(idx(1)-1,ii)=0;
 	            end
            end
            if idx(end) < nsamp
	            if (yi(idx(end,ii)) > 0  && ~isempty(param.peak_fill))  || ...
                  (yi(idx(end,ii)) < 0  && ~isempty(param.trough_fill))
                  yi(idx(end)+1,ii)=0;
	            end
            end
         end
      end

   %      warning('on','MATLAB:interp1:NaNinY')
   else
      yi=interp1(seismic.first:seismic.step:seismic.last,seismic.traces,times,param.interpol);
   end

else
%  dti=seismic.step;
   times=(seismic.first:seismic.step:seismic.last)';
   yi=seismic.traces;
end

trace_max=max(abs(yi));


%     Compute horizontal trace locations
if uniform
   xi=1:ntr;
   dx=1;        % Trace-to-trace separation
else
   xi=annotation;
   dx=(max(xi)-min(xi))/(ntr-1+eps);    % Trace-to-trace separation
end


%     Scale data
if ischar(param.scale)
   if strcmpi(param.scale,'yes')
      scale=(dx*param.deflection)./(trace_max+eps)';
      for ii=1:ntr
         yi(:,ii)=yi(:,ii)*scale(ii);
      end
   else
      scale=dx*param.deflection/(max(trace_max)+eps);
      yi=yi*scale;
   end

else
   scale=param.scale;
   if length(scale) == 1
      yi=yi*scale;
   elseif length(scale) == ntr
      for ii=1:ntr
         yi(:,ii)=yi(:,ii)*scale(ii);
      end
   else
      error(' Scale factor must be a scalar or a vector of length equal to the number of traces')
   end
end


%%       Create figure window (unless an existing figure is to be used)
if strcmpi(param.figure,'new')
   if isempty(param.orient)
      if ntr > 10
         figure_handle=lfigure;
      else
         figure_handle=pfigure;
      end
   else
      if strcmpi(param.orient,'portrait')
         figure_handle=pfigure;
      else
         figure_handle=lfigure;
      end
   end

   set(figure_handle,'Color','w','DoubleBuffer',param.doublebuffer)
 
   %     Create menu botton for "Options"
   options_menu_handle=uimenu(figure_handle,'Label','Options','ForegroundColor','b','Tag','options_menu');

   %     Create menu item for scroll bars
   wseismic_scrollbar_menu_item(figure_handle,options_menu_handle,seismic,param.direction);
   
   %     Create menu item for cursor tracking
   yinfo=info4units(seismic.units);
   if isempty(param.tracking)  ||  isyes(param.tracking)
      [dummy,xinfo]=s_gh(seismic,param.annotation);   %#ok First output argument is not required
      y=linspace(seismic.first,seismic.last,nsamp);
      if ~uniform
         xi2use=xi;
      else
         xi2use=s_gh(seismic,param.annotation);
      end
      initiate_3d_tracking4seismic(options_menu_handle,seismic.traces*param.polarity, ...
            xi2use,y,xinfo,yinfo,{'amplitude','','Amplitude'})
   else
      ylabel([yinfo{3},' (',yinfo{2},')'])
   end

elseif strcmpi(param.figure,'old')
   figure_handle=gcf;

else
   error(['Unknown parameter for keyword "figure": "',param.figure,'"'])
end

%%       Handle background in axis area
ha=get(gcf,'CurrentAxes');
if ~strcmpi(param.background,'gray') && ~strcmpi(param.background,'grey')
   try
      set(ha,'Color',param.background)
   catch %#ok
      disp([' Probably illegal background color: "',param.background,'".'])
      keyboard
   end
else
   bgGray
end

axis([min(xi)-dx*max([param.deflection,1]),max(xi)+dx*max([param.deflection,1]),seismic.first,seismic.last]);
hold on

set(ha,'ydir','reverse','XAxisLocation','top')

%    Handle reversal of plot direction
if strcmpi(param.direction,'r2l')
   set(ha,'xdir','reverse')
   yi=-yi;
   temp=param.trough_fill;
   param.trough_fill=param.peak_fill;
   param.peak_fill=temp;
elseif ~strcmpi(param.direction,'l2r')
   error(['Keyword for plot direction is wrong (',param.direction,')'])
end

if strcmpi(param.figure_only,'yes')
   return
end

%    Plot data
switch param.quality
   case 'draft'
      handles=ue_seismic_plot(times,yi,xi,isnullvalue,param);
   case 'high'
      handles=ue_seismic_plot_ps(times,yi,xi,isnullvalue,param);
   case 'spikes'
      handles=ue_seismic_plot_spikes(times,yi,xi,isnullvalue,param);
   otherwise
      error(['Unknown value for keyword "quality": ',param.quality]) 
      
end


%     Add annotation of horizontal axis
if ~annotlog
   v=axis;
   xil=xi(indices);
   annol=annotation(indices);
   add_xaxis([xil',annol'],'',{'location','top'});
end

%	Add label of horizontal axis
%       (if 5 or more traces or annotation other than trace number)
if ~strcmpi(param.annotation,'trace_no')  ||  ntr > 4
    xtext=s_gd(seismic,param.annotation);
    hunits=s_gu(seismic,param.annotation);

    if ~isempty(xtext) && ~strcmpi(xtext,'not available')
       if ~isempty(hunits) && ~strcmpi(hunits,'n/a')
          xtext=[xtext,' (',hunits,')'];
       end
       hxlabel=xlabel(xtext);
       v=get(hxlabel,'Position'); % Preserve position of x-axis label
    else
       hxlabel=xlabel('');
    end
else
    hxlabel=xlabel('');
    set(gca,'XTickLabel',[])
end


if uniform  &&  (~strcmpi(param.annotation,'trace_no')  ||  ntr > 4)
%  set(gca,'XTickLabel',[]);	% Delete original annotation but
   try                          % keep x-axis label at original position
      set(hxlabel,'Position',v);    
   catch  %#ok
   end
end

set(ha,'gridlinestyle','-','box','on','xgrid','off','ygrid','on', ...
      'FontSize',param.fontsize)

%     Make axes, labels, etc. bold
make_axes_bold(ha)

%       Title
if ~isempty(param.title)
   if iscell(param.title)		% Handle multi-line titles
      mytitle(param.title{1})
   else
      mytitle(param.title)
   end
end

zoom off
hold off
   
if nargout > 0
   aux.figure_handle=figure_handle;
   aux.scale=scale;
   aux.handles=handles;
end


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function handles=ue_seismic_plot(times,traces,location,isnullvalue,param)
% Function plot seismic traces at horizontal locations controlled by location

precision=class(traces);

if ~isempty(param.wiggle_color)
   wiggle=true;
else
   wiggle=false;
end

% dt=times(2)-times(1);
times_orig=times;

ntr=size(traces,2);

handles=zeros(ntr,3);

for ii=1:ntr
   skip=false;        
   if isnullvalue
      idx=find(~isnan(traces(:,ii)));
      if isempty(idx)
         skip=true;
      else
         y=traces(idx,ii);
         times=times_orig(idx);
      end
   else
      y=traces(:,ii);
   end
   
              if ~skip
   chg=find(y(1:end-1).*y(2:end) < 0);
   x_zero=abs(y(chg)./(y(chg+1)-y(chg))).*(times(chg+1)-times(chg))+times(chg);
   [x_data,idx]=sort([times(1);times;x_zero;times(end)]);
   y_data=[0;y;zeros(length(x_zero)+1,1,precision)];
   y_data=y_data(idx);
   if ~isempty(param.peak_fill);
      pos=find(y_data >= 0);
      handles(ii,1)=fill(y_data(pos)+location(ii),x_data(pos),param.peak_fill,'EdgeColor','none'); 
   end
   
   if ~isempty(param.trough_fill);
      neg=find(y_data <= 0);
      handles(ii,2)=fill(y_data(neg)+location(ii),x_data(neg),param.trough_fill,'EdgeColor','none'); 
   end

   if wiggle
      handles(ii,3)=line(y_data(2:end-1)+location(ii),x_data(2:end-1),'Color',param.wiggle_color, ...
           'EraseMode','none','LineWidth',param.wiggle_width);
   end
              end
end

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function handles=ue_seismic_plot_ps(times,traces,location,isnullvalue,param)
% Function plots seismic traces at horizontal locations controlled by location
% Avoids vertical lines from top to bottom on some PS interpreters
% Much slower than ue_seismic_plot

precision=class(traces);

if ~isempty(param.wiggle_color)
   wiggle=true; 
else
   wiggle=false;
end 

times_orig=times;

ntr=size(traces,2);

handles=zeros(ntr,3);

for ii=1:ntr
   skip=false;        
   if isnullvalue
      idx=find(~isnan(traces(:,ii)));
      if isempty(idx)
         skip=true;
      else
         y=traces(idx,ii);
         times=times_orig(idx);
      end
   else
      y=traces(:,ii);
   end
              if ~skip
   chg=find(y(1:end-1).*y(2:end) < 0);
   x_zero=abs(y(chg)./(y(chg+1)-y(chg))).*(times(chg+1)-times(chg))+times(chg);
   [x_data,idx]=sort([times(1);times;x_zero;times(end)]);
   y_data=[0;y;zeros(length(x_zero),1,precision);0];
   y_data=y_data(idx);

   if ~isempty(param.peak_fill);
      pos=find(y_data >= 0);  
      ind1=find(diff(pos) > 1); 
      ind=[pos(1),reshape([pos(ind1)';pos(ind1+1)'],1,2*length(ind1)),pos(end)];
      for kk=1:2:length(ind)-1;
         index=ind(kk):ind(kk+1);
         handles(ii,1)=fill(y_data(index)+location(ii),x_data(index),param.peak_fill, ...
             'erasemode','none','EdgeColor','none'); 
      end
   end
 
   
   if ~isempty(param.trough_fill);
      neg=find(y_data <= 0);
      ind=find(diff(neg) > 1); 
      ind=[neg(1),reshape([neg(ind)';neg(ind+1)'],1,2*length(ind)),neg(end)];
      for kk=1:2:length(ind)-1
         index=ind(kk):ind(kk+1);
         handles(ii,2)=fill(y_data(index)+location(ii),x_data(index),param.trough_fill, ...
              'erasemode','none','EdgeColor','none'); 
      end
   end

   if wiggle
      handles(ii,3)=line(y_data(2:end-1)+location(ii),x_data(2:end-1),'Color',param.wiggle_color, ...
         'erasemode','none','LineWidth',param.wiggle_width); 
   end
              end

end

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function handles=ue_seismic_plot_spikes(times,traces,location,isnullvalue,param)
% Function plot seismic traces at horizontal locations controlled by location

precision=class(traces);

timestimes=reshape([times';times';times'],[],1);

[nsamp,ntr]=size(traces);
zero=zeros(1,nsamp,precision);
nulls=NaN(1,nsamp,precision);

handles=zeros(ntr,2);

for ii=1:ntr
   deflection=[traces(:,ii)';zero;nulls]+location(ii);
   if ~isnullvalue  || ~all(isnan(traces(:,ii)))
      %  Plot zero line
      handles(ii,1)=line(0*traces(:,ii)+location(ii),times, ...
          'Color',param.wiggle_color,'EraseMode','none','LineWidth',0.5);
      % Plot spikes
      handles(ii,2)=line(deflection(:),timestimes,'Color',param.wiggle_color, ...
          'EraseMode','none','LineWidth',param.wiggle_width);
   end              

end


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function seismic=select_subset_no1(seismic,param)
% Select requested subset of the seismic data

global S4M

history=S4M.history;       % Preserve value of global variable S4M.history
S4M.history=false;

if isempty(param.times)
   seismic=s_select(seismic,{'traces',param.traces});

elseif iscell(param.times)
   seismic=s_select(seismic,{'traces',param.traces},{'null',NaN}, ...
           {'times',param.times{1},param.times{2}});

else
   seismic=s_select(seismic,{'traces',param.traces},{'null',NaN}, ...
           {'times',param.times(1),param.times(2)});

end

S4M.history=history;		% Restore history setting


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function param=set_undefined_parameters4seismic_no2(param,seismic,nsamp)

%     Set "quality" parameter if it is not an input argument
if isempty(param.quality) 
   param.quality='draft';
   
elseif strcmp(param.quality,'spikes')
   param=set_undefined_parameters4matrix_no3(param,seismic,nsamp);
   return
end


%     Set "wiggle_width" parameter if it is not an input argument
if isempty(param.wiggle_width) 
   param.wiggle_width=0.5;
end

%    Set "deflection" parameter and color if a seismic dataset is input
if isempty(param.deflection) 
   param.deflection=1.5;
end


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function param=set_undefined_parameters4matrix_no3(param,seismic,nsamp)


%     Set "quality" parameter if it is not an input argument
if isempty(param.quality)
   param.quality='spikes';
end

%     Set "wiggle_width" parameter if it is not an input argument
if isempty(param.wiggle_width) 
   param.wiggle_width=max(0.25,125/nsamp);
end

%    Set "deflection" parameter and color if a matrix is input
if all(seismic.traces >= 0)
   param.deflection=0.9;
else
   param.deflection=0.6;
end
param.wiggle_color='red';
