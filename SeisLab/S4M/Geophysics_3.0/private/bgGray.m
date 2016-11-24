function bgGray(axisHandle)
% Create a gray axis backgrond; 

if nargin == 0
   axisHandle=gca;
end

set(gcf,'Color','white')
set(axisHandle,'Color',[0.9,0.9,0.9])
