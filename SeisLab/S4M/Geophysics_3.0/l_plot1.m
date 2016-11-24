function aux=l_plot1(wlog,varargin)
% Function plots log curves into a single plot window.
% Due to a quirk in MATLAB's legend command the legend is wrong if logical
% curves are plotted together with regular curves and the logical curves are
% NOT the last ones to be plotted.
% Right-clicking on a curve (in select mode) will bring up a menu with choices
% for curve type, color, line thickness, etc. (this is generally more 
% convenient than the procedure built into Matlab figures).
%
% See also: l_compare, l_plot
%
% Written by: E. Rietsch: April 1, 2001
% Last updated: July 16,2009: Add "Options" menu
% 
%
%	        aux=l_plot1(wlog,varargin)
% INPUT
% wlog    log structure
% varargin  one or more cell arrays; the first element of each cell array is a
%         keyword, the other elements are parameters. Presently, keywords are:
%      'figure'  Specifies if new figure should be created or if the seismic 
%                traces should be plotted to an existing figure. Possible   
%                values are 'new' and any other string. 
%                Default: {'figure','new'} 
%      'colors'  color of curves. 
%                Default: {'colors','r','b','g','c','m','k','y'}
%      'curves'  mnemonics of curves to plot. {'curves','*'} means all
%                curves.
%                Default: {'curves',[]}   which brings up a list of curve 
%                             mnemonics for interactive curve selection
%      'depths'  Depth range (or rather range of values of first column); can
%                be two comma-separated numbers or a two-element vector 
%                Default: the whole depth range
%      'linewidth' linewidth of curves. 
%                Default: {'linewidth',0.5}
%      'lloc'    location of label.
%                Default: {'lloc','BestOutside'} i.e. outside of plot 
%      'orient'  plot orientation; possible values are: 'landscape' and 
%                'portrait'
%                Default: {'orient','portrait'}   for four or fewer curves:  
%                         {'orient','landscape'}  for more than four curves: 
%      'scale'   Force independent scaling of the curves (even if they have 
%                the same units of measurements. 
%                Possible values are 'yes' and 'no'.
%                Default: {'scale','no'}
% OUTPUT
% aux     structure with the following field
%      'figure_handle'  handle to the figure 
%
% EXAMPLES  
%      l_plot1(l_data)         % Interactively select curves to plot
%
%      %      Plot sonic and density log in landscape orientation                                
%      l_plot1(l_data,{'curves','DTp','rho'},{'orient','landscape'})
%
%      %      Plot selected logs in the depth range from 4000 to 5000 
%      %      (in terms of log depth units).                                
%      l_plot1(l_data,{'depths',6000,8000},{'scale','yes'})

% UPDATE HISTORY
%      January 20, 2007: define a minimum number of digits for 
%                        log-range display
%      November 22, 2007: streamline code; message (not an abort) if 
%                        none of the specified curves has been found 
%      April 11, 2008: "bool" and 'logical' accepted for logical curves


if ~istype(wlog,'well_log')
   error(' First input argument must be a well log')
end

if length(wlog) > 1
   error(' Log structure must have length 1 --- must not be an array')
end

%       Set defaults for input parameters
param.curves=[];
param.figure='new';
param.orient='portrait';
param.colors={'r','b','g','c','m','k','y'};
param.linewidth=0.5;
param.lloc='BestOutside';
param.depths=[wlog.first,wlog.last];
param.tracking='yes';
param.scale=[];

%       Decode and assign input arguments
param=assign_input(param,varargin);

if iscell(param.depths)
   param.depths=cell2mat(param.depths);
end


%       Get and/or check the mnemonics
if isempty(param.curves)
   str=wlog.curve_info(2:end,1);
   [idx,ok] = mylistdlg(str,{'promptstring','Select one or more curves:'},...
                      {'selectionmode','multiple'},...
		      {'previous','l_plot1','l_plot'}, ...
		      {'name','SeisLab: l_plot1'});
   if ~ok
      return
   else
      param.curves=wlog.curve_info(idx+1,1);
   end
end

if ~iscell(param.curves)
   param.curves={param.curves};
end
if iscell(param.curves{1})
   param.curves=param.curves{1};
end

if length(param.curves) == 1  &&  strcmp(param.curves,'*')
   param.curves=wlog.curve_info(2:end,1)';
end


index=find(wlog.curves(:,1) >= param.depths(1)  &  wlog.curves(:,1) <= param.depths(2));
if isempty(index)
   error([' Log has no values in requested depth/time range: ', ...
          num2str(param.depths)])
end

%       Check if units of measurement of all curves (except lithology curves) 
%       are the same
index1=curve_indices(wlog,param.curves);
index2=index1(index1>0);
if isempty(index2)
   disp(' None of the requested curve mnemonics exists; hence, no plot has been created.')
   return
end

param.curves=param.curves(index1>0);
ncurves=length(index2);

units=wlog.curve_info(index2,2);

same_units=all(ismember(units,units{1}));

irregular=0;  	% For later check if both logical and
ilogical=0;  	% regular curves are to be plotted

if isempty(param.scale)
   if same_units
      scale=false;
   else
      scale=true;
   end
else
   scale=isyes(param.scale);
end

ier=0;
if ~iscell(param.colors)
   param.colors={param.colors};
end
ncol=length(param.colors);
if ncurves > ncol
   alert([' Only ',num2str(ncol),' curve colors defined; ', ...
          'hence not all ',num2str(ncurves),' displayed'])
   ncurves1=ncol;
else
   ncurves1=ncurves;
end

ltext1=cell(1,ncurves1);
ltext2=cell(1,ncurves1);

%       Create figure window if requested
if strcmp(param.figure,'new')
   font_size=10;
   if strcmpi(param.orient,'portrait')
      figure_handle=pfigure;
   else
      figure_handle=lfigure;
   end
   set(figure_handle,'DoubleBuffer','on');
   axis_handle=gca;
   set(axis_handle,'FontSize',font_size);
   if ncurves1 > 1
      hold on
   end
   
   %     Create menu botton for "Options"
   options_menu_handle=uimenu(figure_handle,'Label','Options','ForegroundColor','b','Tag','options_menu');

else
   figure_handle=gcf;
   axis_handle=gca;
end

for ii=1:ncurves1
   idx=index2(ii);
   if ~strcmpi(wlog.curve_info(idx,2),'logical')  &&  ~strcmpi(wlog.curve_info(idx,2),'bool')
      irregular=1;
      if scale
         temp=wlog.curves(index,idx);
         mint=min(temp);
         maxt=max(temp);

         %   Figure out how many digits to display
         nDigits1=no_of_digits(abs(mint),5);
         nDigits2=no_of_digits(abs(maxt),5);

         temp=((temp-mint)+eps/2)/((maxt-mint)+eps);
         plot(temp,wlog.curves(index,1),param.colors{ii},'LineWidth', ...
              param.linewidth)
         ltext1{ii}=[mnem2tex(param.curves{ii}),': '];
         ltext2{ii}=[num2str(mint,nDigits1),' - ',num2str(maxt,nDigits2), ...
                  ' ',units2tex(l_gu(wlog,param.curves{ii}))];
      else
         plot(wlog.curves(index,idx),wlog.curves(index,1),param.colors{ii}, ...
             'LineWidth',param.linewidth)
         ltext1{ii}=[mnem2tex(param.curves{ii}),': '];

      end
      hold on

   else        % Logical curves
      x=wlog.curves(index,idx);
      y=wlog.curves(index,1);
      ya=(y(1:end-1)+y(2:end))*0.5;
      lya=length(ya);
      iidx=reshape([1:lya;1:lya],1,2*lya);      
      yy=[y(1);y(1);ya(iidx);y(end);y(end)];
      xx=[0;x(iidx);x(end);x(end);0];
      dxx=diff(xx);
      idx1=find(dxx ~= 0);
      idx2=unique([idx1;idx1+1]);
      fill([0;xx(idx2);0],[yy(1);yy(idx2);yy(end)],param.colors{ii},'EdgeColor','none');
      ltext1{ii}=mnem2tex(param.curves{ii});
      ltext2{ii}='0 - 1 (logical)';
   end 

end

linemenu	% Allow interactive modification of curves

if ier == 1
   disp([' Available curves: ',cell2str(wlog.curve_info(2:end,1))])
end

if same_units
   xlabel(units2tex(wlog.curve_info{idx,2}));
else
   xlabel('Units are curve-dependent')
end

ylabel(info2label(wlog.curve_info(1,:)));

pos=get(axis_handle,'Position');
set(axis_handle,'YDir','reverse','XAxisLocation','top', ...
        'Position',[pos(1),pos(2)-0.04,pos(3),0.8])


if irregular && ilogical
   alert(' Legend will be wrong if logical curves were not last on the list')
end

%	Handle legend
if scale
   ltext1=[char(ltext1),char(ltext2)];
end

legend(ltext1,'Location',param.lloc)

grid on
zoom on
box on
hold off

%	Create title
mytitle(mnem2tex(wlog.name))

%	Create a "menu item" if "tracking_button" is true
if strcmp(param.figure,'new')  &&  strcmp(param.tracking,'yes')
   initiate_2d_tracking_item(options_menu_handle,{'x','n/a','x'}, ...
	                      wlog.curve_info(1,:))

end

if nargout > 0
   aux.figure_handle=figure_handle;
   aux.axis_handle=axis_handle;
end
