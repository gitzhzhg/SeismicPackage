function figure_handle=pfigure
% Function creates a figure window in portrait mode with label and
% time stamp. Also adds a "Save plot" menu button.
%
% Written by: E. R.: January 19, 2007
% Last updated: February 1, 2008: change position of plot label and date
%
%            figure_handle=pfigure
% OUTPUT
% figure_handle  figure handle

% UPDATE HISTORY
%            August 31, 2007: Add "save plot" menu button
%            November 25, 2007: delete figure handle if there is no output
%                             argument and no figure label   

global PARAMS4PROJECT S4M WF

run_presets_if_needed

figure_handle=figure;
set(figure_handle,'Position',S4M.portrait,'PaperPosition',[0.8 0.5 6.5,8.0], ...
        'PaperOrientation','portrait','Color','w', ...
        'InvertHardcopy',S4M.invert_hardcopy);             
axis_handle=gca;	% Save handle to current axes

bgGray
figure_export_menu

if ~isyes(S4M.figure_labels)  % Do not create labels in the lower left and 
                        % lower right of a figure
   if nargout == 0
      clear figure_handle
   end
   return
end

%	Create new axes for label
h=axes('Position',[0 0 1 1],'Visible','off');

%	Create label for lower right-hand side
if ~isempty(S4M.plot_label)
   txt=strrep(S4M.plot_label,'\_','#&%');  
   txt=strrep(txt,'_','\_'); 
   txt=strrep(txt,'#&%','\_');
else
   txt='';
end

if ~isempty(PARAMS4PROJECT)  && isfield(PARAMS4PROJECT,'name')  &&  ~isempty(txt)
   txt={strrep(PARAMS4PROJECT.name,'_','\_');txt};

elseif ~isempty(WF) && ~isempty(txt)
   txt={strrep(WF.name,'_','\_');txt};
end

%  	Add date/time stamp and plot label
try
   text(0.8,0.02,S4M.time,'FontSize',7); 
catch
end
try
   text(0.04,0.02,txt,'FontSize',7);
catch
end


%	Make axis handle invisible so that the axis cannot be used by accident
set(h,'HandleVisibility','off');

axes(axis_handle);	% Make original axes the current axes
set(axis_handle,'Position',[0.14,0.085,0.79,0.79],'FontName',S4M.font_name);

if nargout == 0
   clear figure_handle
end
