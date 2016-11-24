function turnoffstandardtoolbar(hfig)

if(nargin<1)
    hfig=gcf;
end

showstate=get(groot,'showhiddenhandles');
set(groot,'showhiddenhandles','on');
hkids=get(hfig,'children');
htoolbar=[];
for k=1:length(hkids)
    if(strcmp(get(hkids(k),'tag'),'FigureToolBar'))
        htoolbar=hkids(k);
    end
end
if(~isempty(htoolbar))
    hkids=get(htoolbar,'children');
    for k=1:length(hkids)
        if(strcmp(get(hkids(k),'tag'),'Exploration.ZoomOut'))
            set(hkids(k),'state','off')
        elseif(strcmp(get(hkids(k),'tag'),'Exploration.ZoomIn'))
            set(hkids(k),'state','off')
        elseif(strcmp(get(hkids(k),'tag'),'Exploration.Pan'))
            set(hkids(k),'state','off')
        elseif(strcmp(get(hkids(k),'tag'),'Exploration.Rotate'))
            set(hkids(k),'state','off')
        elseif(strcmp(get(hkids(k),'tag'),'Exploration.DataCursor'))
            set(hkids(k),'state','off')
        elseif(strcmp(get(hkids(k),'tag'),'Standard.EditPlot'))
            set(hkids(k),'state','off')
        end
    end
end

set(groot,'showhiddenhandles',showstate);
