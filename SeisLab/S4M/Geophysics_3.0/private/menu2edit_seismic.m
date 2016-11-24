function menu2edit_seismic(figure_handle,axis_handle)
% Create a menu button that allows one to select plot parameters for wiggle plot
% Written by: E. R.: November 20, 2003
% Last updated: September 3, 2004; button legend in red
%
%                 menu2edit_seismic(figure_handle,axis_handle)
% INPUT 
% figure_handle   handle of the figure which needs to be replotted
% axis_handle     handle of the axis with the seismic data
if nargin == 1
   axis_handle=gca;
end

if nargin == 0
   figure_handle=gcf;
   axis_handle=gca;
end

%	Create menu botton
menu_handle=uimenu(figure_handle,'Label','Plot parameters','ForegroundColor','b');

%	Create submenu items
uimenu(menu_handle,'Label','New layout', ...
           'CallBack',{@g_change_seismic_plot_parameters,figure_handle,axis_handle});
uimenu(menu_handle,'Label','Peak-fill color', ...
           'CallBack',{@peak_fill_color,figure_handle,axis_handle});
uimenu(menu_handle,'Label','Trough-fill color', ...
           'CallBack',{@trough_fill_color,figure_handle,axis_handle});
uimenu(menu_handle,'Label','Wiggle color', ...
           'CallBack',{@wiggle_color,axis_handle});

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function g_change_seismic_plot_parameters(obj,eventdata,figure_handle,axis_handle) %#ok 
% Call-back function creating a window to select plot parameters for wiggle plot
% Written by; E. R.: November 20, 2003
% Last updated:
%
%                 g_change_seismic_plot_parameters(figure_handle)
% INPUT 
% figure_handle   handle of the figure which needs to be replotted


global PARAMETERS4FUNCTION

% figure_handle=gcf;
figure(figure_handle)	% Make figure current
userdata=get(figure_handle,'UserData');
param=userdata.param;
handles=gui_4_seismic_plot_parameters(param);

delete(handles.figure1)		% Delete GUI for parameter selection

if handles.ok
   userdata4axis=get(axis_handle,'UserData');
   delete(userdata4axis.trace_handles.hpeak)
   delete(userdata4axis.trace_handles.htrough)
   delete(userdata4axis.trace_handles.hwiggle)
   PARAMETERS4FUNCTION.s_iplot.default=handles.param;
   PARAMETERS4FUNCTION.s_iplot.default.figure='old';
%   userdata=get(figure_handle,'UserData');
   userdata.param=handles.param;
   set(figure_handle,'UserData',userdata)
   try
      s_iplot(userdata.seismic)
   catch
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [color,ok]=select_colors_for_wiggle_trace(title,default)
% Interactively select a color

ok=true;

colors={'r','g','b','y','k','m','c',[0.6,0.6,0.6],[0 0 0]};
%colors={[1 0 0],[0 1 0],[0 0 1],[1 1 0],[0 0 0],[1 0 1],[0 1 1],[0.5,0.5,0.5],[1 1 1]};
labels={'red','green','blue','yellow','black','magenta','cyan','gray','no color'};

default=find(ismember(labels,default));
if isempty(default)
   error(['Default color "',default,'" is not available'])
end

index=gui4colors(title,labels,colors,default);

if isempty(index)
   ok=false;
   color=[];

elseif index == 9
   color='none';
else
   color=colors{index};
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function peak_fill_color(obj,eventdata,figure_handle,axis_handle) %#ok
% Change color of peak-fill

%figure_handle=gcf;

userdata=get(figure_handle,'UserData');
param=userdata.param;

[color,ok]=select_colors_for_wiggle_trace({'Select color for wiggle peaks'},'black');
if ok
   userdata4axis=get(axis_handle,'UserData');
   if strcmpi(param.direction,'l2r')
      set(userdata4axis.trace_handles.hpeak,'FaceColor',color);
   else
      set(userdata4axis.trace_handles.htrough,'FaceColor',color);
   end
   refresh
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function trough_fill_color(obj,eventdata,figure_handle,axis_handle)  %#ok
% Change color of trough-fill

% figure_handle=gcf;

userdata=get(figure_handle,'UserData');
param=userdata.param;

[color,ok]=select_colors_for_wiggle_trace({'Select color for wiggle troughs'},'no color');

if ok
   userdata4axis=get(axis_handle,'UserData');
   if strcmpi(param.direction,'r2l')
      set(userdata4axis.trace_handles.hpeak,'FaceColor',color);
   else
      set(userdata4axis.trace_handles.htrough,'FaceColor',color);
   end
   refresh
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function wiggle_color(obj,eventdata,axis_handle)  %#ok
% Change color of wiggle

[color,ok]=select_colors_for_wiggle_trace({'Select color for wiggles'},'black');
if ok
   userdata4axis=get(axis_handle,'UserData');

   if strcmp(color,'none')
      set(userdata4axis.trace_handles.hwiggle,'LineStyle','none');
   else
      set(userdata4axis.trace_handles.hwiggle,'Color',color,'LineStyle','-');
   end
   refresh
end


