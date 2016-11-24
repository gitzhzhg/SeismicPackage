function aux=mygrid(d,type,color,linewidth,linestyle)
% Function plots horizontal or vertical grid lines at coordinate(s) d
%
% Written by: E. Rietsch: Sepetember 29, 2006
% Last updated:
% 
%        aux=mygrid(d,type,color,linewidth,linestyle)
% INPUT
% d      array denoting the location of grid lines
%        grid line locations outside the range of axis values are discarded
% type   string variable
%        'v'  vertical lines
%        'h'  horizontal lines
% color   Color of lines (default: color='black')
% linewidth    Line width (default: linewidth=1)
% linestyle    Line style (default: linestyle='-')
% OUTPUT
% aux   structure with auxiliary information
%   'handles'  handles of the grid lines; if there is no output argument 
%        handle visibility is set to 'off'.

if nargin < 3
   color='black';
   linewidth=1;
   linestyle='-';

elseif nargin < 4
   linewidth=1;
   linestyle='-';

elseif nargin < 5
   linestyle='-';

end
  
hold on

v=axis;

switch type
case 'v'
   x=d(d >= v(1) & d <=v(2));  
   handles=zeros(length(x),1);
   for ii=1:length(x);
      handles(ii)=line([x(ii),x(ii)],[v(3),v(4)],'Color',color,'LineStyle', ...
            linestyle,'LineWidth',linewidth);
   end

case 'h'
   x=d(d >= v(3) & d <= v(4));
   handles=zeros(length(x),1);
   for ii=1:length(x);
      handles(ii)=line([v(1),v(2)],[x(ii),x(ii)],'Color',color,'LineStyle', ...
            linestyle,'LineWidth',linewidth);
   end

otherwise
   error(['Unknown argument "type" :',type])

end

if nargout == 1
   aux.handles=handles;
else
   set(handles,'HandleVisibility','off')
end
