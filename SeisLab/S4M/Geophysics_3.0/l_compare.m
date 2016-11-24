function l_compare(varargin)
% Function plots curves from one or more logs onto one plot. If depth units or
% curve units differ they are automatically converted to those of the first
% curve (provided a conversion is known; see function "ds_unit_conversion")
%
% See also: l_plot, l_plot1, ds_unit_conversion
%
% Written by: E. Rietsch: September 1, 2000
% Last updated: January 30, 2008: replace "l_unit_conversion" by "ds_unit_conversion"
%
%           l_compare(varargin)
% INPUT
% varargin  one or more cell arrays; the first element of each cell array is 
%           either a log structure, or the keyword 'parameters'. 
%           In the first case the following arguments contain a curve 
%           mnemonic, possibly followed by one or more cell arrays with
%           parameters. Each cell array starts with a keyword. The following 
%           keywords are allowed:
%    'color'   Color of the curve.
%           Default: {'color','r'} for the first curve,
%                    {'color','b'} for the second curve, ...
%    'linewidth' Linewidth of the curve
%                      Default{'linewidth',1}
%    'legend'  Legend (annotation) of curve. Default: curve mnemonic       
%
%           If the first element is the keyword 'parameters' the following 
%           arguments are cell arrays; the first element of each cell array
%           is a keyword, the other elements are parameters. 
%           Presently, keywords are:
%    'lloc'   numeric parameter specifying the location of the legend;
%           Default: {'legend',4}
%    'depths' first and last depth to plot (two comma-separated numbers 
%           or a two-element vector)
%           Default: {'depths',d_min,d_max}     where "d_min" = min(log.first)
%                              and "d_max" is max(log.last) for all logs used
%
% EXAMPLES
%           log1=l_data;
%           l_compare({log1,'DTp',{'color','r'},{'linewidth',2}, ...
%                      {'legend','Sonic log'}})
%           log2=log1;
%           l_compare({log1,'Vp',{'color','r'}},{log2,'Vs', ...
%                      {'color','g--'}},{'parameters',{'lloc',2}})
%
%           l_compare({log1,'Vp'},{log1,'Vs'})   % plots two different curves from the same log

% UPDATE HISTORY
%           April 11, 2007: Bug fix


global S4M

if ~iscell(varargin{1})
   varargin={varargin};
end
nargs=length(varargin);

color={'r','b','g','k','c','m','y','r--','b--','g--','k--','c--','m--','y--'};

parameters.lloc=4;
parameters.depths=[];

pfigure
set(gcf,'DoubleBuffer','on')

for ii=nargs:-1:1
   argument=varargin{ii};
   login=argument{1};
   if ~isstruct(login)
      if strcmpi(login,'parameters')
         parameters=assign_input(parameters,argument(2:end));
         break
      else
         error(' First element of input cell array is neither a log nor the keyword "parameters"')
      end
   end
end  

icurve=1;
for ii=1:nargs
  argument=varargin{ii};
  login=argument{1};

            if istype(login,'well_log')
  wlog=login;   % Save log for cursor tracking
  mnemonic=argument{2};

%       Default settings
  compare.linewidth=2;
  compare.color=color{icurve};
  compare.legend=strrep(mnemonic,'_','\_');

%       Decode and assign input arguments
  if length(argument) > 2
     compare=assign_input(compare,argument(3:end));
  end
  
%       Find requested curve
  idx=curve_index1(login,mnemonic);
  curve_units=login.curve_info{idx,2};
  depth_units=login.curve_info{1,2};

  if icurve == 1
     set(gca,'YDir','reverse','XAxisLocation','top');
     hold on
     old_curve_units=curve_units;
     old_depth_units=depth_units;
     ltext=compare.legend;
     xlabel(units2tex(curve_units),'FontSize',13,'FontWeight','bold','Fontname',S4M.font_name);
     ylabel(info2label(login.curve_info(1,:)), ...
          'FontSize',13,'FontWeight','bold','Fontname',S4M.font_name)
  else
     login=ds_unit_conversion(login,{depth_units,old_depth_units},{curve_units,old_curve_units,mnemonic});
     
     try
        ltext=char(ltext,compare.legend);
     catch
        keyboard
     end
  end
 

  curve=login.curves(:,idx);
  depth=login.curves(:,1);
  if ~isempty(parameters.depths)
     if iscell(parameters.depths)
        parameters.depths=cat(2,parameters.depths{:});
     end
     idx=find(depth >= parameters.depths(1) && depth <= parameters.depths(2));
     depth=depth(idx);
     curve=curve(idx);
  end
  plot(curve,depth,compare.color,'LineWidth',compare.linewidth)

  icurve=icurve+1;

            end
end  

legend(ltext,parameters.lloc)
grid on
zoom on
box on


%       Add buttons for cursor tracking   
initiate_2d_tracking(wlog.curve_info(idx,:),wlog.curve_info(1,:))
