function display_cursor_location4seismic(varargin)  %#ok Gets two dummy input arguments provided by MATLAB
% GUI tool
% To invoke:
%    set(gcf,'WindowButtonMotion',@display_cursor_location4seismic)
% Written by: E. Rietsch: December 12, 2005
% Last updated: October 27, 2006
%
%    display_cursor_location4seismic

drawnow        % Update axes to get the correct limits
axis_handle=gca;
userdata=getappdata(axis_handle,'userdata_3d_tracking');
if isempty(userdata)
   return
end

if isempty(get(gcf,'WindowButtonMotion'))
   bh=findobj(gcf,'Tag',['tracking_button',num2str(gcf)]);
   userdata=get(bh,'UserData');
   userdata.on_off='off';
   set(bh,'Label','Tracking is off','UserData',userdata);
   return
end

pos=get(axis_handle,'CurrentPoint');
xlimits=get(axis_handle,'XLim');
ylimits=get(axis_handle,'YLim');

x=pos(1,1);
y=pos(1,2);

%	Cursor is inside the axes box
if x >= xlimits(1)  &&  x <= xlimits(2)  && ...
   y >= ylimits(1)  &&  y <= ylimits(2)

   m=size(userdata.data,2);
   idx=min(max(round(x),1),m);
   idy=min(max(y,userdata.y(1)),userdata.y(end));
   idy=round((idy-userdata.y(1))/userdata.step)+1;
   
   x=userdata.x(idx);
   y=userdata.y(idy);
   z=userdata.data(idy,idx);
   data2show=['  ',userdata.xname,': ',sprintf(userdata.xformat,x),' ',userdata.xunits,';  ',...
              userdata.yname,': ',sprintf(userdata.yformat,y),' ',userdata.yunits,';  ' ...
              userdata.zname,': ',sprintf(userdata.zformat,z),' ',userdata.zunits];
	 
   set(gcf,'Pointer',userdata.userpointer)

   userdata.hh=uicontrol('Units','pix','pos',[0 0 500 25],'Style','text',...
	'String',data2show,'Horiz','left','BackgroundColor',get(gcf,'Color'),...
	'ForegroundColor',[0 0 0],'Tag','cursor_tracking_data','Userdata',[]);
   setappdata(axis_handle,'userdata_3d_tracking',userdata)

else	% Cursor is outside of the axes box
   if isfield(userdata,'pointer')
      set(gcf,'Pointer',userdata.pointer);	% Save presently used pointer type
   else
      userdata.pointer=get(gcf,'Pointer');      % Restore pointer
      setappdata(axis_handle,'userdata_3d_tracking',userdata)      % set(axis_handle,'UserData',userdata);
   end
   if isfield(userdata,'hh')
      if ishandle(userdata.hh)
         set(userdata.hh,'ForegroundColor',get(gcf,'Color'))
      end
   end 
end
