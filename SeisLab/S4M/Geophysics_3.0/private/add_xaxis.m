function haxes=add_xaxis(annotation,label,varargin)
% Function adds additional x-axis (opposite to the existing one) with different
% annotation (e.g. ft instead of meter or time instead of depth)
% Note: Only the original axis will change upon a zoom!!
%
% Written by: E. Rietsch, August 8, 2001
% Last updated: February 6, 2002: Fixed bug in location of tick marks
%
%             haxes=add_xaxis(annotation,label,varargin)
% INPUT
% annotation  two-column vector. The first column is the tick annotation location
%             in terms of the existing x-axis. The second column is the value 
%             associated with this location
% label       string with label of new axis
% varargin    one or more cell arrays; the first element of each cell array is,
%             a keyword string the following arguments contains a parameter(s).
%             Accepted keywords are:    
%         'color' Axis color
%                 Default: {'color','k'}
%         'location' Location of axis. Default: opposite existing axis; 
%                 however, the axis location can also be selected via 
%                 keywords 'top' or 'bottom'
%         'tickwidth' line width of ticks.
%                 Default: {'tickwidth',2} thicker tick to make them stand out better.
%         'tickdir' Direction of the ticks; possible values are 'in', 'out', and ''.
%                 The latter uses the tick direction of the already existing y-axis.
%                 Default: {'tickdir','')
%         'tickscale' Scale factor for tick length
%                 Default: {'tickscale',1.5}
% OUTPUT
% haxes   handle of axes

global S4M

if nargin == 1
   label=' ';
end

%       Set defaults input arguments
param.color='k';
param.location=[];
param.tickwidth=2;
param.tickdir='';
param.tickscale=1.5;

%       Use input parameters to change defaults
param=assign_input(param,varargin);

h_old=gca;
v=axis;

%       Determine annotation within range of present axis
index=find(annotation(:,1) >= min(v(1:2)) & ...
      annotation(:,1) <= max(v(1:2)));
use=annotation(index,:);
nticks=length(index);

%       Determine old axis location
old_loc=get(h_old,'Xaxislocation');

if ~isempty(param.location)
   if ~ismember(param.location,{'top','bottom'})
      error(' Location parameter must be either ''top'' or ''bottom''')
   end
   xloc=param.location;
   if strcmp(old_loc,xloc)
      set(h_old,'XTickLabel',blanks(1)')  % Remove existing labels
   end
   if strcmp(xloc,'bottom')
      lr=1;
      xx0=v(3);
   else
      xx0=v(4);
     lr=-1;
   end
else
   if strcmp(old_loc,'top') 
      xloc='bottom';
   else
     xloc='top';
   end
   if strcmp(old_loc,'top')  && strcmp(get(h_old,'Ydir'),'normal') 
      lr=1;
      xx0=v(3);
   else
      xx0=v(4);
      lr=-1;
   end
end

             if strcmp(xloc,old_loc)

haxes=gca;
set(haxes,'XColor',param.color,'XTick',use(:,1),'XTickLabel',use(:,2));
        
             else

%       Reduce figure size to make room for new axis
pos=get(h_old,'Position');
%ld1=pos(2);
%ld2=1-ld1-pos(1);
%ld=max([ld1,ld2]);

%       Plot new axes
haxes=axes('Position',pos, ...
      'XaxisLocation',xloc,'Yaxislocation',get(h_old,'Yaxislocation'), ...
      'Xlim',v(1:2),'Ylim',v(3:4),'XColor',param.color, ...
      'XTick',use(:,1),'XTickLabel',use(:,2), ...
      'XDir',get(h_old,'Xdir'),'YDir',get(h_old,'Ydir'), ...
      'YTick',get(h_old,'YTick'),'YTickLabel',blanks(1)');
set(haxes,'FontName',S4M.font_name)

xlabel(label);

%       Get tick parameters
if ~isempty(S4M)
   set(haxes,'FontName',S4M.font_name)
end


if strcmp(param.tickdir,'out')
  set(haxes,'TickDir','out');
end

%       Restore original axes
axes(h_old);

if strcmp(param.tickdir,'in')
  ticklength=get(h_old,'TickLength');
  tickdir=-1;

  dxx=(v(3)-v(4))*tickdir*lr;
  xx=[xx0,xx0+param.tickscale*dxx*ticklength(1)];
%  xx1=xx0-dxx*ticklength(1);

  for ii=1:nticks
    line([use(ii,1),use(ii,1)],xx,'Color',param.color,'LineWidth',param.tickwidth);
  end
end  

             end
