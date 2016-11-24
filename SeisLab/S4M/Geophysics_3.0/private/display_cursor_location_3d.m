function display_cursor_location_3d(hObject,evdata)  %#ok
% GUI tool
% To invoke:
%    set(gcf,'WindowButtonMotion',@display_cursor_location_3d)

drawnow        % Update axes to get the correct limits

axis_handle=gca;
% userdata=get(axis_handle,'UserData');
userdata=getappdata(axis_handle,'userdata_3d_tracking');
% keyboard
if isempty(userdata)
   return
end
%if ~strcmp(userdata.tag,'display_cursor_location_3d')
%   return
%end
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
if x >= xlimits(1) && x <= xlimits(2) && ...
   y >= ylimits(1) && y <= ylimits(2)

   [n,m]=size(userdata.data);
   if userdata.dx ~= 0
      idx=round((x-userdata.x(1))/userdata.dx)+1;
   else
      idx=find((userdata.x(1:end-1)+userdata.x(2:end))*0.5 > x);
      if isempty(idx)
         idx=m;
      else
         idx=idx(1);
      end
   end
   
   if userdata.dy ~= 0
      idy=round((y-userdata.y(1))/userdata.dy)+1;
   else
      idy=find((userdata.y(1:end-1)+userdata.y(2:end))*0.5 > y);
      if isempty(idy)
         idy=n;
      else
         idy=idy(1);
      end
   end

   try
      x=userdata.x(idx);
   catch  %#ok
      keyboard
   end
   y=userdata.y(idy);
   z=userdata.data(idy,idx);
   data2show=['  ',userdata.xname,': ',sprintf(userdata.xformat,x),' ',userdata.xunits,';  ',...
              userdata.yname,': ',sprintf(userdata.yformat,y),' ',userdata.yunits,';  ' ...
              userdata.zname,': ',sprintf(userdata.zformat,z),' ',userdata.zunits];
   %userdata.userpointer='fullcrosshair'; 
   set(gcf,'Pointer',userdata.userpointer)
   %disp(userdata.userpointer) %Test
   % get(gcf,'Pointer')
   userdata.hh=uicontrol('Units','pix','pos',[0 0 500 25],'Style','text',...
	     'String',data2show,'Horiz','left','BackgroundColor',get(gcf,'Color'),...
	     'ForegroundColor',[0 0 0],'Tag','cursor_tracking_data','Userdata',[]);
   setappdata(axis_handle,'userdata_3d_tracking',userdata)

else	% Cursor is outside of the axes box
   if isfield(userdata,'pointer')
      set(gcf,'Pointer',userdata.pointer);	% Save presently used pointer type
   else
      userdata.pointer=get(gcf,'Pointer');      % Restore pointer
      setappdata(axis_handle,'userdata_3d_tracking',userdata)
   end
   
   if isfield(userdata,'hh')
      if ishandle(userdata.hh)
         set(userdata.hh,'ForegroundColor',get(gcf,'Color'))
      end
   end 
end
