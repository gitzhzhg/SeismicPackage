function aux=surfaceplot(matrix,varargin)
% Plot entries of a matrix as a surface
%
% Written by: E. Rietsch: September 1, 2003
% Last updated: January 15, 2008: Replace "surf" by "mesh" (no black grid lines).
%
%           surfaceplot(matrix,varargin)   or
%           surfaceplot(matrix,x,y,varargin)
% INPUT
% matrix    matrix to plot
% x         horizontal axis --- x(1) is left (optional)
% y         vertical axis   --- y(1) is top  (optional)
%           either both x and y or neither can be specified 
% varargin  one or more cell arrays; the first element of each cell array is a
%           keyword,the other elements are parameters. Presently, keywords are:
%     'colormap'  colormap to use; type "help graph3d" to see a list of Matlab-
%           supplied colormaps.
%           Default: {'colormap','jet'}
%     'info'  three-by-three cell array; all elements are strings;
%           the first column are the mnemonics (for cursor tracking) for 
%           the three variables
%           the second represents the units of measurement, 
%           the third the labels of the three axes
%           Default: info={'x','n/a','X'; 'y','n/a','Y'; 'z','n/a','Z'};	    
%     'limits' lower and upper limit of values to which colors will be assigned;  
%           Default: {'limits',[]}    this means limits are determined by 
%                                     the data (not yet used)
% OUTPUT
% aux       Structure with auxiliary information
%      'handles'  handles of surface created
%    
% EXAMPLE
%           figure
%           surfaceplot(rand(10,20))

% UPDATE HISTORY
%           February 16, 2007: New input
%           October 16, 2007: Add option to limit the color range


[n,m]=size(matrix);

if nargin > 1  &&  ~iscell(varargin{1})
   x=varargin{1};
   y=varargin{2};
   varargin=varargin(3:end);
%     Check compatibility
   if length(x) ~= m || length(y) ~= n
       disp(' Incompatible dimensions in "colorplot"')
%   keyboard
   end

else
   x=1:m;
   y=(1:n)';
end

%       Set defaults of input parameters
param.colormap='jet';
param.flip_colormap=false;
param.info=[{'x','n/a','X'};
            {'y','n/a','Y'}; 
	    {'z','n/a','Z'}];
param.limits=[];


%       Replace defaults by actual input arguments
param=assign_input(param,varargin);

mycolormap(param.colormap)
[xx,yy]=meshgrid(x,y);

if ~isempty(param.limits)
   if iscell(param.limits)
      param.limits=cell2num(param.limits);
   end
   if length(param.limits) ~= 2  || (param.limits(2)-param.limits(1)) <= 0
      disp(' Parameter "limits" must be empty or have two numeric values with the second larger than the first.')
      disp(' It does not meet these requiremnts and is therefore ignored.')
      param.limits=[];
   end
   colorscale=matrix;
   colorscale(colorscale < param.limits(1))=param.limits(1);
   colorscale(colorscale > param.limits(2))=param.limits(2);
   handles=mesh(xx,yy,matrix,colorscale);

else
   handles=mesh(xx,yy,matrix);
end

grid on

xlabel(info2label(param.info(1,:)));
ylabel(info2label(param.info(2,:)));
zlabel(info2label(param.info(3,:)));


if nargout > 0
   aux.handles=handles;
end
