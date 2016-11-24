function make_axes_bold(axis_handle)

%     Make axes, labels, etc. bold

set(axis_handle,'FontWeight','bold','LineWidth',2.0,'box','on')
set(get(axis_handle,'XLabel'),'FontWeight','bold')
set(get(axis_handle,'YLabel'),'FontWeight','bold')
