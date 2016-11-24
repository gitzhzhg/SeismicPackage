function menu_handle=figure_export_menu(figure_handle)
% Creates a menu button on the figure with handle "figure_handle" (or
% the current figure) that allows one to save the figure as a "emf" (Enhanced
% Windows Meta File) for PowerPoint or "eps" (Encapsulated PostScript) for 
% LaTeX, or as a JPEG file; does nothing if the menu button already exists.
%
% Written by: E. Rietsch: November 16, 2003
% Last updated: August 31, 2007: check if figure export menu already exists
%
%         menu_handle=figure_export_menu(figure_handle)
% INPUT
% figure_handle  handle of the figure to which to attach the menu button
%         Default: gcf
%
% EXAMPLE
%         figure
%         plot(1:11)
%         figure_export_menu

            
persistent fig_no

if nargin == 0
   figure_handle=gcf;
end
if isempty(fig_no)
   fig_no=1;
else
   fig_no=fig_no+1;
end

%       Check is a figure-export menu button already exists and return if it does
if ~isempty(findobj(figure_handle,'tag','figure_export_menu'))
   disp('Figure-export menu menu exists.')
   menu_handle=[];
   return
end

%	Create menu botton
menu_handle=uimenu(figure_handle,'Label','Save plot','ForegroundColor','b','Tag','figure_export_menu');

%	Create submenu items
uimenu(menu_handle,'Label','EMF (for PowerPoint)', ...
          'CallBack',{@g_fig2pp,figure_handle,0});
uimenu(menu_handle,'Label','JPEG', ...
          'CallBack',{@g_fig2jpg,figure_handle,0});
uimenu(menu_handle,'Label','EPS (for LaTeX)', ...
          'CallBack',{@g_fig2eps,fig_no,figure_handle});

if nargout == 0
   clear menu_handle 
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function g_fig2pp(hObject,eventdata,figure_handle,reverse) %#ok first two arguments are place holders
% Version of "fig2pp" to be called by a menu callback
% Written by: E. Rietsch: November 16, 2003
% Last updated: May 8, 2004: made subfunction
%
%         g_fig2pp(hObject,eventdata,figure_handle,reverse)
% INPUT
% figure_handle  handle of the figure to which to attach the menu button
% reverse    Reverse the figure background and axis colors and adjust graphs
%            (see "whitebg")
%            S4M.invert_hardcopy must be set to 'off' to have an effect
%           (See figure property 'InvertHardcopy')


fig2pp(figure_handle,reverse);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function g_fig2jpg(hObject,eventdata,figure_handle,reverse) %#ok first two arguments are place holders
% Version of "fig2pp" to be called by a menu callback
% Written by: E. Rietsch: November 16, 2003
% Last updated: May 8, 2004: made subfunction
%
%         g_fig2pp(hObject,eventdata,figure_handle,reverse)
% INPUT
% figure_handle  handle of the figure to which to attach the menu button
% reverse    Reverse the figure background and axis colors and adjust graphs
%            (see "whitebg")
%            S4M.invert_hardcopy must be set to 'off' to have an effect
%           (See figure property 'InvertHardcopy')


fig2jpg(figure_handle,reverse);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function g_fig2eps(hObject,eventdata,repfig,figno) %#ok first two arguments are place holders
% Export figure for use in LaTeX documents
% Written by: E. Rietsch: March 4, 2003
% Last updated: March 19, 2004: use fig2eps which uses report 
%              directory in "S4M.eps_directory"
%
%         g_fig2eps(hObject,eventdata,repfig,figno)
% INPUT
% repfig  unique figure number for report (required)
% figno   number of figure to export
%         Default: figno=gcf


if ischar(repfig)
   repfig=str2double(repfig);
end

if nargin == 0
   error(' Figure number in report is required')
end

if nargin == 1
   figno=gcf;
end

fig2eps(repfig,figno)
