function pos=gg_pickbox(axis_handle)
% Function to draw a rubberband line and return the start and end points
%
%         pos=gg_pickbox(axis_handle)
% INPUT 
% axis_handle  handle of current axes
% field        field in userdata in whoch to store the rectangle coordinates

if nargin == 0
   axis_handle=gca;
end
figure_handle=gcf;

userdata=get(figure_handle,'UserData');	% Save user data associated with figure handle
dblbuff=get(figure_handle,'DoubleBuffer');

hold on;
p1=get(axis_handle,'CurrentPoint');	% Get starting point
p1=p1(1,1:2);				% Extract x and y
lh=plot(p1(1),p1(2),'+:','LineWidth',2);  % Plot starting point
udata.p1=p1;
udata.ah=axis_handle;
udata.lh=lh;
udata.object2delete=lh;
set(figure_handle,'UserData',udata,'WindowButtonMotionFcn','wbmf','DoubleBuffer','on');
set(figure_handle,'WindowButtonUpFcn',{@delete_fig_object,figure_handle})
waitfor(lh)
%waitforbuttonpress;
p2=get(axis_handle,'Currentpoint');	% Get end point
%keyboard
pos=[p1(1),p2(1,1),p1(2),p2(1,2)];

%	Restore user data and handles associated with figure
set(figure_handle,'Userdata',userdata,'WindowButtonMotionFcn','', ...
    'DoubleBuffer',dblbuff); 

% delete(lh);				% Delete box

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function wbmf
%	Window motion callback function

utemp=get(gcf,'UserData');
ptemp=get(utemp.ah,'CurrentPoint');
ptemp=ptemp(1,1:2);
set(utemp.object2delete,'XData',[utemp.p1(1),ptemp(1),ptemp(1),utemp.p1(1),utemp.p1(1)], ...
            'YData',[utemp.p1(2),utemp.p1(2),ptemp(2),ptemp(2),utemp.p1(2)]);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function delete_fig_object(hObject,evdata,figure_handle)  %#ok
% Delete the graphics object that is saved in figure user data structure and
% has the field name "object2delete"
% Written by: E. Rietsch: November 27, 2003
% Last updated:

% figure_handle=gcf;
userdata=get(figure_handle,'UserData');
if isfield(userdata,'object2delete')
   delete(userdata.object2delete)
   set(figure_handle,'WindowButtonMotionFcn',[])
   set(figure_handle,'WindowButtonUpFcn',[])
end
