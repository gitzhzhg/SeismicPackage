function display_cursor_location_2d(hObject,evdata)   %#ok
% GUI tool (button_handle is not required)
% To invoke:
%    set(gcf,'WindowButtonMotion',@display_cursor_location_2d)

figure_handle=hObject;
axis_handle=gca;

drawnow        % Update axes to get the correct limits

userdata=getappdata(axis_handle,'userdata_2d_tracking');

if isempty(userdata)
   return
end
if ~strcmp(userdata.tag,'display_cursor_location_2d')
   return
end

if isempty(get(figure_handle,'WindowButtonMotion'))
   bh=findobj(figure_handle,'Tag',['tracking_button',num2str(figure_handle)]);
   userdata=get(bh,'UserData');
   userdata.on_off='off';
   set(bh,'Label','Tracking is off','UserData',userdata);
   return
end

pos=get(axis_handle,'CurrentPoint');
xlimits=get(axis_handle,'XLim');
ylimits=get(axis_handle,'YLim');

if isempty(get(figure_handle,'WindowButtonMotion'))
   return
%   set(gcf,'WindowButtonMotion',@display_cursor_location_2d);
end

x=pos(1,1);
y=pos(1,2);


%	Cursor is inside the axes box

if x >= xlimits(1) && x <= xlimits(2) && ...
   y >= ylimits(1) && y <= ylimits(2)
   
   %    Are there discrete x coordinates and or y-coordinates
   if isfield(userdata,'xcoord')  && ~isempty(userdata.xcoord)
      distx=(x-userdata.xcoord).^2;
      [dummy,idx]=min(distx);
      x=userdata.xcoord(idx);
   end
   if isfield(userdata,'ycoord')  && ~isempty(userdata.ycoord)
      disty=(y-userdata.ycoord).^2;
      [dummy,idx]=min(disty);
      y=userdata.ycoord(idx);
   end
  
   data2show=['  ',userdata.xname,': ',sprintf(userdata.xformat,x),' ',userdata.xunits,';  ',...
              userdata.yname,': ',sprintf(userdata.yformat,y),' ',userdata.yunits];
	 
   set(figure_handle,'Pointer','crosshair')
   userdata.hh=uicontrol('Units','pix','pos',userdata.position,'Style','text',...
	'String',data2show,'Horiz','left','BackgroundColor',userdata.bgcolor,...
	'ForegroundColor',[0 0 0],'Tag','cursor_tracking_data','Userdata',[]);
%   set(axis_handle,'UserData',userdata);
   setappdata(axis_handle,'userdata_2d_tracking',userdata)
   if isempty(get(figure_handle,'WindowButtonMotion'))
      return
%      set(gcf,'WindowButtonMotion',@display_cursor_location_2d);
   end

else	% Cursor is outside of the axes box
   if isfield(userdata,'pointer')
      set(figure_handle,'Pointer',userdata.pointer);	% Save presently used pointer type
   else
      userdata.pointer=get(figure_handle,'Pointer');    % Restore original pointer
%      hh=findobj(figure_handle,'Tag','cursor_tracking_data');    % Find and remove the display
      try
         delete(userdata.hh)                                          % of the cursor tracking data
      catch  %#ok
%      keyboard
      end
      setappdata(axis_handle,'userdata_2d_tracking',userdata)
 %     set(axis_handle,'UserData',userdata);
   end
   if isfield(userdata,'hh')
      if ishandle(userdata.hh)
         set(userdata.hh,'ForegroundColor',get(figure_handle,'Color'))
      end
   end 
end

drawnow
