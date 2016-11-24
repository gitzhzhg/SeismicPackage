function PI_LaunchPicktool(varargin)

hmasterfig=gcf;

hpicktool=findobj(hmasterfig,'tag','picktool');

udat=get(hpicktool,'userdata');

if(isempty(udat))
    return;
end

seis=udat{1};
t=udat{2};
x=udat{3};

picktool(seis,t,x,hmasterfig);