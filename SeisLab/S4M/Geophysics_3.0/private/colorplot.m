function handle=colorplot(matrix,x,y,xinfo,yinfo,zinfo,param)
% Plot entries of a matrix as in a color scale
%
% Written by: E. Rietsch: September 1, 2003
% Last updated: February 17, 2004: Better default values
%
%           colorplot(matrix,x,y,xinfo,yinfo,zinfo,param)
% INPUT
% matrix    matrix to plot
% x         horizontal axis --- x(1) is left
% y         vertical axis   --- y(1) is top
% xinfo     three-element cell array; all elements are strings; first is a 
%           mnemonic (for cursor tracking), second units of measurement, 
%           the third string is the x-axis label
% yinfo     three-element cell array; all elements are strings; first is a 
%           mnemonic (for cursor tracking), second units of measurement, 
%           the third string is the y-axis label
% zinfo     three-element cell array; all elements are strings; first is a 
%           mnemonic (for cursor tracking), second units of measurement, 
%           the third string is the z-axis label (not used)
% EXAMPLE
%           figure
%           colorplot(rand(10,20))

[n,m]=size(matrix);


if nargin < 7
   param.colormap=colormap('jet');
   param.edgecolor='k';
   param.gaps=[0,0];
   param.limits=[];
end
if nargin < 6
   zinfo={'z','n/a','Z'};
end
if nargin < 5
   xinfo={'x','n/a','X'};
   yinfo={'y','n/a','Y'};
end

if nargin < 3
   x=1:m;
   y=(1:n)';
end

%     Check compatibility
if length(x) ~= m || length(y) ~= n
   warning(' Incompatible dimensions in "colorplot".')  %#ok
   keyboard
end

userdata=get(gca,'UserData');

if isempty(param.limits)
   handle=imagesc(x,y,matrix);
else
   handle=imagesc(x,y,matrix,param.limits);
end
grid on

xlabel(info2label(xinfo));
ylabel(info2label(yinfo));


	% Implement cursor tracking
bool=true;
try
   bool=~strcmp(userdata.tag,'display_cursor_location_3d');
catch
   % Do nothing
end

initiate_3d_tracking(matrix,x,y,xinfo,yinfo,zinfo,bool)

if nargout == 0
   clear handle
end
