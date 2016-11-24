function initiate_3d_tracking4seismic(options_menu_handle,matrix,x,y,xinfo,yinfo,zinfo,axis_handle,bool)
% Initiate picking on a 2-d plot with implied z-component (e.g. seismic data)
%
% Written by: E. Rietsch: September 14, 2003
% Last updated: July 15, 2009: Use menu item instead of button
%
%         initiate_3d_tracking4seismic(matrix,x,y,xinfo,yinfo,zinfo,axis_handle,bool)
% INPUT
% matrix  matrix of data plotted
% x       coordinates associated with columns
% y       coordinates associated with rows
% xinfo   info about x-coordinates; horizontal (optional)
% yinfo   info about y-coordinates; vertical (optional)
% zinfo   info about z-coordinates; values of matrix (optional)
% axis_handle   handle of axis that should be tracked
% bool    logical variable; if "bool" is true then menu button will be created
%         Default: function creates a button if it does not already exist

% UPDATE HISTORY
%         November 13, 2005: use "setappdata" to store user data

if nargin < 8
   axis_handle=gca;
end

if nargin < 7
   xinfo={'x','','x'};
   yinfo={'y','','y'};
   zinfo={'z','','z'};
end

if nargin < 4
   [n,m]=size(matrix);
   x=1:m;
   y=1:n;
end

xlabel(info2label(xinfo));
ylabel(info2label(yinfo));
zlabel(info2label(zinfo));

	% Implement cursor tracking

% userdata.tag='display_cursor_location_3d';
userdata.userpointer='crosshair';
userdata.data=matrix;
userdata.ah=axis_handle;

userdata.x=x;
userdata.xformat='%8.5g';
userdata.xname=xinfo{1};
userdata.xunits=units4plot(xinfo{2});

userdata.y=y;
userdata.step=y(2)-y(1);
userdata.yformat='%8.5g';
userdata.yname=yinfo{1};
userdata.yunits=units4plot(yinfo{2});

userdata.zformat='%8.5g';
userdata.zname=zinfo{1};
userdata.zunits=units4plot(zinfo{2});

setappdata(gca,'userdata_3d_tracking',userdata);


%	Check if a menu button for cursor tracking needs to be created

if nargin < 9
   handle=findobj(gcf,'Tag','tracking_button');
   if isempty(handle)
      create_menu_item4tracking({@display_cursor_location4seismic},options_menu_handle);
   end
else
  if bool
     create_menu_item4tracking(@display_cursor_location4seismic,options_menu_handle);
  end
end
