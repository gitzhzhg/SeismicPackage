function menu_handle=cseismic_scrollbar_menu_item(figure_handle,options_menu_handle,seismic,plotdir)
% Function creates a menu button on the figure with handle "figure_handle" 
% that allows one to add scroll bars to a color seismic plot
%
% Written by: E. Rietsch, August 22, 2005
% Last updated: November 5, 2005: Save axis limits before user querry
%
%            menu_handle=cseismic_scrollbar_menu(figure_handle,seismic,plotdir)
% INPUT
% figure_handle  handle of the figure to which to attach the menu button
% seismic    seismic data set to be displayed; needed to provide limits for 
%            the scrollbar boxes
% plotdir    plot direction
% OUTPUT
% menu_handle  handle of the menu button created by this function


ntr=size(seismic.traces,2);
tmax=seismic.last-seismic.first;
tmin=3*seismic.step;

%	Create menu button
menu_handle=uimenu(options_menu_handle,'Label','Add scrollbars','ForegroundColor','b');

set(menu_handle,'Callback',{@seismic_scrollbar,gca,menu_handle,ntr,tmin,tmax,plotdir});

if nargout == 0
   clear menu_handle
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function seismic_scrollbar(hObject,evdata,axis_handle,menu_handle,ntr,tmin,tmax,plotdir) %#ok

label=get(menu_handle,'Label');

if strcmp(label,'Add scrollbars')
   set(menu_handle,'Label','Remove scrollbars');
else
   reset_scroll_button(axis_handle,menu_handle)
   set(menu_handle,'Label','Add scrollbars');
   return
end

zoom out

%       Prompt user for time and trace range
prompt={'Enter the number of traces to display:','Enter the time range to display:'};
name='Scroll parameters';
numlines=1;

defaultanswer=mat2cellstrings([min(50,ntr),min(500,tmax)]);

ier1=true;
ier2=true;

%	Save present axis limits
v=axis;
setappdata(menu_handle,'axislimits',v)

while any(ier1)  ||  any(ier2)
    answer=inputdlg(prompt,name,numlines,defaultanswer);

    if isempty(answer)
       reset_scroll_button(axis_handle,menu_handle)
       return
    end

    [ntraces,ier1]=check_numeric(answer{1},'Number of traces',{'minmax',[1,1]}, ...
          {'bounds',1,ntr});
    [timerange,ier2]=check_numeric(answer{2},'Time range',{'minmax',[1,1]}, ...
          {'bounds',tmin,tmax});
end


%       This reduces flickering when the axis is updated
set(gcf,'doublebuffer','on');

%       Set appropriate axis limits and settings
% set(axis_handle,'xlim',[v(1),v(1)+ntraces],'ylim',[v(3),v(3)+timerange]);

%       Generate constants for use in uicontrol initialization
pos=get(axis_handle,'position');
dx=(v(2)-v(1))/ntr;
xmin=dx*round(2*v(1)/dx)/2;
xmax=dx*round(2*v(2)/dx)/2;

ymin=round(v(3));
ymax=round(v(4));

xtraces=ntraces*(xmax-xmin)/ntr;

if ntraces < ntr
%       Create a slider below the x-axis
   xsliderpos=[pos(1),        pos(2)-0.030, pos(3), 0.029];
   sstep1=max(1/(ntr-ntraces),0.01);
   if strcmp(plotdir,'l2r')
       hxslider=uicontrol('style','slider',...
          'units','normalized','Position',xsliderpos,...
          'min',xmin,'max',xmax-xtraces,'value',xmin, ...
          'SliderStep',[sstep1,min(1,10*sstep1)]);
	
   else
       hxslider=uicontrol('style','slider',...
          'units','normalized','Position',xsliderpos,...
          'min',xmin,'max',xmax-xtraces,'value',xmax-xtraces, ...
          'SliderStep',[sstep1,min(1,10*sstep1)]);
   end       
   set(hxslider,'Callback',{@scrollx,axis_handle,xtraces,xmin,xmax,plotdir})
   set(gca,'XLim',[xmin,xmin+xtraces])
      no_xscrollbar=false;
else
   hxslider=1.1;
   no_xscrollbar=true;
end

if timerange < tmax
   ysliderpos=[pos(1)+pos(3)+0.002, pos(2),      0.018,   pos(4)];
   sstep2=min(1,timerange/(ymax-ymin));
   hyslider=uicontrol('style','slider',...
       'units','normalized','Position',ysliderpos,...
       'min',ymin,'max',ymax-timerange,'value',ymax-timerange, ...
       'SliderStep',[max(sstep2/10,8/(ymax-ymin)),sstep2]);
   set(hyslider,'Callback',{@scrolly,axis_handle,timerange,ymin,ymax})

   set(gca,'YLim',[ymin, ymin+timerange])
      no_yscrollbar=false;
else
   hyslider=1.1;
   no_yscrollbar=true;
end
setappdata(menu_handle,'sliders',[hxslider,hyslider])

if no_xscrollbar && no_yscrollbar
   set(menu_handle,'Label','Add scrollbars');
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function scrollx(hObject,evdata,axis_handle,ntraces,xmin,xmax,plotdir) %#ok

if strcmp(plotdir,'l2r')
   xlimits=get(gcbo,'value')+[0,ntraces];
else
   xlimits=xmax-get(gcbo,'value')+[-ntraces,0]+xmin;
end
  
set(axis_handle,'XLim',xlimits)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function scrolly(hObject,evdata,axis_handle,timerange,ymin,ymax) %#ok
ylimits=ymax-get(gcbo,'value')+[-timerange,0]+ymin;
%ylimits=ylimits;

set(axis_handle,'YLim',ylimits)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function reset_scroll_button(axis_handle,menu_handle)

set(menu_handle,'Label','Add scrollbars');
sliders=getappdata(menu_handle,'sliders');
try
   delete(sliders(1))
catch
end
try
   delete(sliders(2))
catch
end
axislimits=getappdata(menu_handle,'axislimits');
axis(axis_handle,axislimits)
