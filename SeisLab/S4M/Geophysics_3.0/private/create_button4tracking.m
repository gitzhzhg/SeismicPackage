function button_handle=create_button4tracking(type_of_action)
% Create a menu item to track cursor movements
%
% Written by: E. Rietsch: August 31, 2003
% Last updated: January 8, 2004
%
%                button_handle=create_button4tracking(type_of_action)
% INPUT
% type_of_action  cell array or string with the call-back function to use for cursor tracking
%                presently options are:
%                {@display_cursor_location_2d,gca}
%                 @display_cursor_location_3d
%                'g_display_cursor_location_patch'


userdata4button.on_off='off';
userdata4button.button_action=type_of_action;
figure_handle=gcf;

set(figure_handle,'MenuBar','figure')

%button_handle=uimenu('Label','Tracking is off','Tag','tracking_button', ...
%   'ForeGroundColor',[0 0 1],'UserData',userdata4button);

button_handle=uimenu('Label','Tracking is off','Tag','tracking_button', ...
   'ForeGroundColor',[0 0 1]);

setappdata(button_handle,'UserData',userdata4button)

set(button_handle,'Callback',{@tracking,figure_handle})

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function tracking(hObject,evdata,figure_handle)  %#ok
% GUI tool
% Written by: E. Rietsch: August 31, 2003
% Last updated: January 8, 2004: use function handle
%
%            tracking(hObject,evdata,figure_handle)
% INPUT
% hObject    handle of button
% evdata     reserved by Matlab
% figure_handle  handle of figure window

% set(gcf,'MenuBar','none')
% state = uisuspend(gcf);

zoom off

if isempty (hObject)		% Window has no "tracking" button
   disp('No "tracking" button')
   return
end

userdata4button=getappdata(hObject,'UserData');

if strcmp(userdata4button.on_off,'off')
%   zoom off
   userdata4button.on_off='on';
%   set(hObject,'UserData',userdata4button,'Label','Tracking is on');
   set(hObject,'Label','Tracking is on');
   set(figure_handle,'WindowButtonMotionFcn',userdata4button.button_action);

%   userdata4button.zoom_handles=disable_zoom(figure_handle)%test
%   zh=userdata4button.zoom_handles%test
    zoom off
else
   userdata4button.on_off='off';
%   setappdata(hObject,'UserData','userdata4button')
%   set(hObject,'UserData',userdata4button,'Label','Tracking is off');
   set(hObject,'Label','Tracking is off');
   set(figure_handle,'WindowButtonMotionFcn',[]);
   
   hh=findobj(figure_handle,'Tag','cursor_tracking_data');    % Find and remove the display
   delete(hh)                                                 % of the cursor tracking data
%   set(gcf,'menuBar','figure')
%   uirestore(state);

%  set(userdata4button.zoom_handles,'enable','on')
end   
drawnow
setappdata(hObject,'UserData',userdata4button)
