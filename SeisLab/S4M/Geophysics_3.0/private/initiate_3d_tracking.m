function initiate_3d_tracking(matrix,x,y,xinfo,yinfo,zinfo,bool,axis_handle)
% Initiate picking on a 2-d plot with with an implied z-component (matrix values)
%
% Written by: E. Rietsch: September 14, 2003
% Last updated: April 22, 2008: bug fix for case length(x) == 1
%
%        initiate_3d_tracking(matrix,x,y,xinfo,yinfo,zinfo,bool,axis_handle)
% INPUT
% matrix  matrix of data plotted
% x      coordinates associared with columns
% y      coordinates associated with rows
% xinfo  info about x-coordinates (horizontal) in form of 
%        {mnemonic,units of measurement,description};
%        (optional)
% yinfo  info about y-coordinates (vertical) in form of
%        {mnemonic,units of measurement,description};
%        (optional)
% zinfo  info about z-coordinates; values of matrix
%        {mnemonic,units of measurement,description};
%        (optional)
% bool   logicl variable; if "bool" is true then menu button will be created
%        Default: function creates a button if it does not alredy exist
% axis_handle   handle of axis that should be tracked

% UPDATE HISTORY
%        December 10, 2005: use "setappdata" to store user data
%        November 19, 2007: bug fix


[n,m]=size(matrix);

if nargin < 8
   axis_handle=gca;
end

if nargin < 7
   bool=[];
end

if nargin < 6
   xinfo={'x','','x'};
   yinfo={'y','','y'};
   zinfo={'z','','z'};
end

if nargin <3
   x=1:m;
   y=1:n;
end

xlabel(info2label(xinfo));
ylabel(info2label(yinfo));
zlabel(info2label(zinfo));

%	Save userdata possibly stored with the axis handles
%setappdata(gcf,'userdata4axis',get(axis_handle,'UserData'));

	% Implement cursor tracking

% userdata.tag='display_cursor_location_3d';
userdata.userpointer='cross';
userdata.data=matrix;
userdata.ah=axis_handle;

userdata.x=x;
if length(x) > 1
   dx=diff(x);

   if isconstant(dx,0.001);
      userdata.dx=mean(dx);
   else
      userdata.dx=0;
   end
else
   userdata.dx=1;
end

userdata.constantx=m==1 || isconstant(diff(x),0.001);
userdata.xformat='%8.5g';
userdata.xname=xinfo{1};
userdata.xunits=units4plot(xinfo{2});

userdata.y=y;
if length(y) > 1
   dy=diff(y);
   if isconstant(dy,0.001);
      userdata.dy=mean(dy);
   else
      userdata.dy=0;
   end
else
   userdata.dy=1;
end

userdata.yformat='%8.5g';
userdata.yname=yinfo{1};
userdata.yunits=units4plot(yinfo{2});

userdata.zformat='%8.5g';
userdata.zname=zinfo{1};
userdata.zunits=units4plot(zinfo{2});

setappdata(gca,'userdata_3d_tracking',userdata);

if isempty(bool)
   handle=findobj(gcf,'Tag','tracking_button');
   if isempty(handle)
      create_button4tracking({@display_cursor_location_3d});
   end
else
  if bool
     create_button4tracking(@display_cursor_location_3d);
  end
end
