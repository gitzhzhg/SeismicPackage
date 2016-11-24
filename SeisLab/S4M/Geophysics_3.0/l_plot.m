function aux=l_plot(wlog,varargin)
% Function plots log curves; 
% A curve's properties can be changed by right-clicking on that curve and 
% choosing new curve parameters from the pop-up menu (generally more convenient
% than the procedure built into Matlab figures).
%
% See also: l_plot1, l_compare
%
% Written by: E. Rietsch: May 6, 2000
% Last update: March 21, 2009: Make subplots contigous; allow choice of the 
%                              position of the x-axis label
%
%           aux=l_plot(wlog,varargin)
% INPUT
% wlog      log structure
% varargin  one or more cell arrays; the first element of each cell array is a
%           keyword, the other elements are parameters. Presently, keywords are:
%           'annotation' subplot (curve annotation). Possible values are 'mnemonic'
%                     and 'description' which refer to columns 1 or 3 of the
%                     field "curve_info'. Mnemonic is generally much shorter.
%                     Default: {'annotation','mnemonic'} 
%           'axis_limits'  controls axis limits (see help axis); 
%                     possible values are: 'auto' and 'tight'
%                     Default: {'axis_limits','tight'}
%           'color'   color (and line style) of curves. Default {'color','r'}
%                     Colors (and other curve parameters) may be changed 
%                     interactively by right-clicking on a curve and selecting
%                     a new color from the pop-up menu.
%           'curves'  mnemonics of curves to plot. {'curves',[]} means all
%                     curves.
%                     Default: {'curves',[]} 
%                     If S4M.deployed == 1 this brings up a list of curve 
%                               mnemonics for interactive curve selection
%                     otherwise all curves are plotted
%           'depths'  Depth range (or rather range of values of first column);
%                     can be two comma-separated numbers or a two-element vector 
%                     Default: {'depths',wlog.first,wlog.last}    
%                              i.e. the whole depth range
%           'figure'  Specifies if new figure should be created or if the 
%                     seismic traces should be plotted to an existing figure.
%                     Possible values are 'new' and any other string. 
%                     Default: {'figure','new'} 
%           'linewidth' Width of the curves plotted
%                     Default: {'linewidth',0.5}
%           'orient'  plot orientation; possible values are: 'landscape' and 
%                     'portrait'
%                     Default: for four or fewer curves:  {'orient','portrait'
%                              for more than four curves: {'orient','landscape'}
%           'xll'     x-label location; location of labels of the x-axis
%                     Possible values are:
%                     'top'     x-axis labels are at the top
%                     'bottom', x-axis labels are at the bottom
%                     'talternate', x-axis labels alternate between top and 
%                               bottom, beginning at top
%                     'balternate'  x-axis labels alternate between bottom and 
%                               top, beginning at bottom
%                     Default: {'xll','top'}
%
% OUTPUT
% aux     optional output argument. Structure with fields "figure_handle" and
%         "axis_handles" which contains the handles to the axes of all subplots
%           
% EXAMPLES
%         wlog=l_data;
%
%         %  Plot sonic and density log in landscape orientation
%         l_plot(wlog,{'curves','DTp','RHO'},{'orient','portrait'})   
%
%         %  Plot all logs in the depth range from 9000 to 11000 (in terms of log 
%         %  depth units) using line color "blue"; ; alternare x-axis tick-labels
%         %  beginning at the top.
%         l_plot(wlog,{'color','b'},{'depths',9000,11000},{'xll','talternate'}) 
%
%         %  Plot all curves of log structure; output "aux" structure with axis
%         %  handles; alternare x-axis tick-labels beginning at the bottom.
%         %  Changing, say, the x-axis direction from normal to reverse, 
%         %  of the third curve (e.g. a sonic log) can be achieved by
%         aux=l_plot(wlog,{'xll','balternate'}) 
%         set(aux.axis_handles(3),'XDir','reverse') % Reverse the x-axis for 
%                                                   % the third curve

% UPDATE HISTORY
%           July 22, 2007: use "linkaxes"
%           February 21, 2008: Replace parameter "axis_scaling" by "axis_limits" 
%           March 26, 2008: Ask for curve mnemonics if the function is 
%                              called from the command line
%           November 25, 2008: Added parameter to specify the line width


global S4M

aux.figure_handle=[];

if ~istype(wlog,'well_log')
   error(' First input parameter must be a well log')
end
if length(wlog) > 1
   error(' Log structure must have length 1 --- must not be an array')
end

%       Set defaults for input parameters
param.annotation='mnemonic';
param.axis_scaling='';
param.axis_limits='tight';
param.color='r';
param.curves='';
param.depths=[wlog.first,wlog.last];
param.figure='new';
param.linewidth=0.5;
param.orient=[];
param.xll='top';


%        Decode and assign input arguments
param=assign_input(param,varargin);

%        Handle legacy usage
if ~isempty(param.axis_scaling)
   alert('Parameter "axis_scaling" is obsolete. Please use "axis_limits" instead.')
   param.axis_limits=param.axis_scaling;
end

if iscell(param.depths)
   param.depths=cell2num(param.depths);
end

if isempty(param.curves) 
%     Interactively select curves to plot if the function is called in a deployed
%     program or if it is called from the command line
   if S4M.deployed  || length(dbstack) < 2 
                                         
      str=wlog.curve_info(2:end,1);
      [idx,ok] = mylistdlg(str,{'promptstring','Select one or more curves:'},...
                      {'selectionmode','multiple'},...
		      {'previous','l_plot','l_plot1'}, ...
		      {'name','SeisLab: l_plot'});
                      
      if ~ok
         if nargout == 0
            clear aux
         end
         return
      else
         param.curves=wlog.curve_info(idx+1,1);
      end
   else
      param.curves=wlog.curve_info(2:end,1);
   end  
end

if strcmp(param.annotation,'mnemonic')
   idescr=1;          % Curve mnemonics used as subplot titles
else
   idescr=3;          % Curve descriptions used as subplot titles  
end

if ~iscell(param.curves)
   param.curves={param.curves};
end

ncurves=length(param.curves);

if ncurves > 12
   alert('The maximum number of curves that can be displayed is 12')
   ncurves=12;
end
if ncurves == 1 && strcmp(param.curves,'*')
   param.curves=wlog.curve_info(2:end,1)';
   ncurves=length(param.curves);
end

aux.figure_handle=[];

if strcmp(param.figure,'new')   % Create a new figure
   if isempty(param.orient)
      if ncurves > 4
         param.orient='landscape';
      else
         param.orient='portrait';
      end
   end
  
   if strcmpi(param.orient,'landscape')
      figure_handle=lfigure;
      font_size=(60/max([7.5,ncurves]))+1;    % Adjust font size to the number of curves to plot
   elseif strcmpi(param.orient,'portrait') 
      figure_handle=pfigure;
      font_size=(40/max([5,ncurves]))+1;      % Adjust font size to the number of curves to plot
   else
      alert([' Unknown figure orientation (',param.orient',')'])
   end
   
   %     Create menu botton for "Options"
   options_menu_handle=uimenu(figure_handle,'Label','Options','ForegroundColor','b','Tag','options_menu');

   bgGray

else
   font_size=10;
   figure_handle=gcf;
end
index=find(wlog.curves(:,1) >= param.depths(1) & wlog.curves(:,1) <= param.depths(2));

if isempty(index)
   error([' Log has no values in requested depth/time range: ', ...
          num2str(param.depths)])
end
ier=0;

tracking_button=true;

if ncurves > 1
   switch param.orient
   case 'landscape'
      mysuptitle(mnem2tex(wlog.name),{'factor',1})
   case 'portrait'
      mysuptitle(mnem2tex(wlog.name),{'factor',0.5})
   end
else
   mytitle(mnem2tex(wlog.name))
end

%  Create handles for the subplots
if ncurves > 1
   hh=mysubplot(1,ones(1,ncurves));
else
   hh=gca;
end

for ii=1:ncurves
   axes(hh(ii))    % Select subplot "ii"
   pos=get(hh(ii),'Position');
   set(hh(ii),'FontSize',font_size,'Position',[pos(1),pos(2)-0.04,pos(3),0.75]);
   [idx,ier]=curve_index1(wlog,param.curves{ii});
   if isempty(idx)
      disp([' Requested curve mnemonic "',param.curves{ii},'" not available'])
      ier=1;
   elseif length(idx) > 1
      error([' More than one curve with mnemonic "',param.curves{ii},'"'])
   else 
      if ~strcmpi(wlog.curve_info(idx(1),2),'logical') && ...
         ~strcmpi(wlog.curve_info(idx(1),2),'bool')    % Regular curves
         plot(wlog.curves(index,idx(1)),wlog.curves(index,1),param.color, ...
                 'LineWidth',param.linewidth)


         try
            axis(param.axis_limits)
         catch   %#ok don't bother
            disp(['Unknown value for "axis_limits": "',param.axis_limits,'".'])
         end

      else        % Logical curves
          x=wlog.curves(index,idx(1));
          y=wlog.curves(index,1);
          ya=(y(1:end-1)+y(2:end))*0.5;
          lya=length(ya);
          iidx=reshape([1:lya;1:lya],1,2*lya);      
          yy=[y(1);y(1);ya(iidx);y(end);y(end)];
          xx=[0;x(iidx);x(end);x(end);0];
          dxx=diff(xx);
          idx1=find(dxx ~= 0);
          idx2=unique([idx1;idx1+1]);
          fill([0;xx(idx2);0],[yy(1);yy(idx2);yy(end)],param.color,'EdgeColor','none');
      end

      	 %	Create a "menu item" if "tracking_button" is true
      initiate_2d_tracking_item(options_menu_handle,wlog.curve_info(idx(1),:), ...
	                      wlog.curve_info(1,:),tracking_button)
      tracking_button=false;

      set(hh(ii),'XAxisLocation','top')
      title(mnem2tex(wlog.curve_info{idx,idescr}));
      units=wlog.curve_info{idx,2};
      if ncurves > 1
         if ~strcmpi(units,'n/a')
             xlabel(units2tex(wlog.curve_info{idx,2}));
         end
      else
         if ~strcmpi(units,'n/a')
             xlabel([strrep(wlog.curve_info{idx,idescr},'_','\_'),' (',units2tex(wlog.curve_info{idx,2}),')']);
         else
             xlabel(strrep(wlog.curve_info{idx,idescr},'_','\_'));
         end
      end
      set(hh(ii),'YDir','reverse')
      if strcmpi(wlog.curve_info{1,2},'n/a')
         dunits='';
      else
         dunits=[' (',units2tex(wlog.curve_info{1,2}),')'];
      end
      if ii == 1
         ylabel([wlog.curve_info{1,idescr},dunits])
      end
      if ii > 1 && ii < ncurves
         set(hh(ii),'YtickLabel','')
      end
      if ii == ncurves && ii ~= 1
         set(hh(ii),'YAxisLocation','right')
         ylabel([wlog.curve_info{1,idescr},dunits])
      end
   end
   grid
   
   %     Make axes, labels, etc. bold
   make_axes_bold(hh(ii))

end

if ier == 1
   disp([' Available curves: ',cell2str(wlog.curve_info(2:end,1))])
end

%	Create linked zoom
if length(hh) > 1
   axes(hh(1))
   linkaxes(hh,'y')
else
   zoom on
end

axis_label_location(hh,param.xll,'both')


%	Create over-all title

linemenu		% Allow interactive modification of curves

if nargout > 0
   aux.figure_handle=figure_handle;
   aux.axis_handles=hh;
else
   clear aux
end
