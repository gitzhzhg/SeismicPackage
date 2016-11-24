function [newcolors,newnames]=picktool_colorchooser(names,oldcolors,msg,posxy)

%see if the last name is empty. This is a signal that we are creating a new
%horizon and need a name and a color.
newhor=0;
thishor=1;
newcolors=[];
newnames=[];
if(isempty(names{end}))
    newhor=1;
    newname=inputdlg('Specify a name for the new horizon:','Name needed');
    if(isempty(newname))
        return;
    end
    names(end)=newname;
    oldcolors{end}=[1 1 1];
    thishor=length(oldcolors);
end

newcolors=oldcolors;
newnames=names;

width=400;height=400;
d=dialog('position',[posxy width height],'Name',msg);
width=.25;sep=.02;
height=.4;
xnow=.1;ynow=.3;
uicontrol(d,'style','listbox','string',names,'value',thishor,...
    'units','normalized','position',[xnow,ynow,width,height],'callback',...
    @choose_horizon,'tag','horizonlist');
ynow=ynow+height+sep;
height2=.08;
uicontrol(d,'style','text','String','Horizon names','units','normalized',...
    'position',[xnow,ynow,width,.5*height2]);

xnow=xnow+width+sep;
ynow=.4;
width=.25;height=.25;
uicontrol(d,'style','pushbutton','string','Old color','units','normalized',...
    'position',[xnow,ynow,width,height],'tag','oldcolor','backgroundcolor',oldcolors{thishor},...
    'userdata',oldcolors);

xnow=xnow+sep+width;
uicontrol(d,'style','pushbutton','string','New color','units','normalized',...
    'position',[xnow,ynow,width,height],'tag','newcolor',...
    'callback',@choose_color,'backgroundcolor',newcolors{thishor},'userdata',newcolors);
ynow=ynow+height+sep;
uicontrol(d,'style','text','String','Push button to select new color','units','normalized',...
    'position',[xnow,ynow,width,height2]);

xnow=.1;
ynow=.1;
width=.25;
height=.1;
uicontrol(d,'style','pushbutton','string','Done','units','normalized',...
    'position',[xnow,ynow,width,height],'backgroundcolor',.9*[1 1 1],...
    'foregroundcolor',[1 0 0],'callback',@done)

xnow=.5;
ynow=.1;
width=.25;
height=.1;
uicontrol(d,'style','pushbutton','string','Cancel','units','normalized',...
    'position',[xnow,ynow,width,height],'backgroundcolor',.9*[1 1 1],...
    'foregroundcolor','k','callback',@cancel)

uiwait(d);

    function choose_horizon(d,callbackdata)
        hlist=findobj(gcf,'tag','horizonlist');
        hold=findobj(gcf,'tag','oldcolor');
        hnew=findobj(gcf,'tag','newcolor');
        thishor=get(hlist,'value');
        % oldcolors=get(hold,'userdata');
        % newcolors=get(hnew,'userdata');
        set(hold,'backgroundcolor',oldcolors{thishor});
        set(hnew,'backgroundcolor',newcolors{thishor});
    end

    function choose_color(d,callbackdata)
        hlist=findobj(gcf,'tag','horizonlist');
        hnew=findobj(gcf,'tag','newcolor');
        thishor=get(hlist,'value');
        c=uisetcolor(newcolors{thishor});
        newcolors{thishor}=c;
        set(hnew,'backgroundcolor',c);
    end

    function done(d,callbackdata)
        delete(gcf);
    end

    function cancel(d,callbackdata)
        newcolors=0;
        delete(gcf);
    end

end


    