function axis_label_location(handles,xll,yll)
% In a multi-plot figure created by "mysubplot" this function allows selection
% of the axes which should be annotated.
%
% Written by: E. Rietsch: March 19, 2009
% Last updated: 
%
%
% INPUT
% handles matrix of handle values as output by function "mysubplot".
%        if "handles" has "nh" rows and "mh" columns the the figure has
%        "nh" rows and "mh" columns of subplots
% xll    x-label location; possible values are:
%        'bottom'  labels are along all top axes
%        'top'     labels are along all bottom axes
%        'both'    labels are along the top and the bottom axes
%        'talternate'  labels are alternating between the top and the bottom
%                  axes,beginning with the top axis
%        'balternate'  labels are alternating between the bottom and the top
%                  axes, beginning with the bottom axis
% yll    y-label location; possible values are:
%        'left'    labels are along all left axes
%        'right'   labels are along all right axes
%        'both'    labels are along the left and the right axes
%        'lalternate'  labels are alternating between the left and the right
%                  axes,beginning with the left axis
%        'ralternate'  labels are alternating between the right and the left
%                  axes, beginning with the left axis
% 
%
% EXAMPLES
%        lfigure
%        handles=mysubplot([1,2],[1,1,1]);
%        axis_label_location(handles,'both','lalternate')


%%     Remove the axis labels from the "inner" subplots
[ny,nx]=size(handles);

set(handles(2:end-1,:),'XTickLabel',[])
delete_label(handles(2:end-1,:),'XLabel')
%delete_label(handles(2:end-1,:),'YLabel')
set(handles(:,2:end-1),'YTickLabel',[])
% delete_label(handles(:,2:end-1),'XLabel')
delete_label(handles(:,2:end-1),'YLabel')


%%    Handle "outer" X-Axes
switch xll
   case {'bottom'}
      set(handles(end,:),'XAxisLocation','bottom')
      if ny > 1
         set(handles(1,:),'XTickLabel',[])
         delete_label(handles(1,:),'XLabel')
      end
      
   case {'top'}
      set(handles(1,:),'XAxisLocation','top')
      if ny > 1
         set(handles(end,:),'XTickLabel',[])
         delete_label(handles(end,:),'XLabel')
      end
      
   case {'both'}
      set(handles(end,:),'XAxisLocation','bottom')
      set(handles(1,:),'XAxisLocation','top')
   
   case 'talternate'
      set(handles(1,1:2:end),'XAxisLocation','top')
      set(handles(end,2:2:end),'XAxisLocation','bottom')
      if ny > 1
         set(handles(1,2:2:end),'XTickLabel',[])
         delete_label(handles(1,2:2:end),'XLabel')
         set(handles(end,1:2:end),'XTickLabel',[])
         delete_label(handles(end,1:2:end),'XLabel')
      end
      
   case 'balternate'
      set(handles(1,2:2:end),'XAxisLocation','top')      
      set(handles(end,1:2:end),'XAxisLocation','bottom')
      if ny > 1
         set(handles(1,1:2:end),'XTickLabel',[])
         delete_label(handles(1,1:2:end),'XLabel')
         set(handles(end,2:2:end),'XTickLabel',[])
         delete_label(handles(end,2:2:end),'XLabel')
      end
      
   otherwise
      error(['Unknown option for x-axis label: ',xll])
end

%%    Handele "outer" Y-axes
switch yll
   case {'left'}
      set(handles(:,1),'YAxisLocation','left')
      if nx > 1
         set(handles(:,end),'YTickLabel',[])
         delete_label(handles(:,end),'YLabel')
      end
      
   case {'right'}
      set(handles(:,end),'YAxisLocation','right')
      if nx > 1
         set(handles(:,1),'YTickLabel',[])
         delete_label(handles(:,1),'YLabel')
      end
      
   case {'both'}
      set(handles(:,end),'YAxisLocation','right')
      set(handles(:,1),'YAxisLocation','left')
   
   case 'lalternate'
      set(handles(2:2:end,end),'YAxisLocation','right')
      set(handles(1:2:end,1),'YAxisLocation','left')
      if nx > 1
         set(handles(1:2:end,end),'YTickLabel',[])
         delete_label(handles(1:2:end,end),'YLabel')
         set(handles(2:2:end,1),'YTickLabel',[])
         delete_label(handles(2:2:end,1),'YLabel')
      end
      
   case 'ralternate'
      set(handles(1:2:end,end),'YAxisLocation','right')      
      set(handles(2:2:end,1),'YAxisLocation','left')
      if nx > 1
         set(handles(2:2:end,end),'YTickLabel',[])
         delete_label(handles(2:2:end,end),'YLabel')
         set(handles(1:2:end,1),'YTickLabel',[])
         delete_label(handles(1:2:end,1),'YLabel')
      end 
      
   otherwise
      error(['Unknown option for y-axis label: ',yll])
end

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function delete_label(haxis,label)

hh=get(haxis,label);
if isempty(hh)
   return
end

if iscell(hh)
   hh=cell2num(hh);
end
set(hh,'String','')
