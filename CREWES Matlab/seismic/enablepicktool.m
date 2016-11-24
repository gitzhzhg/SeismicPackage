function enablepicktool(hfig)

if(nargin<1)
    hfig=gcf;
end

hpicktool=findobj(hfig,'tag','picktool');
if(~isempty(hpicktool))
    set(hpicktool,'visible','on','enable','on');
end