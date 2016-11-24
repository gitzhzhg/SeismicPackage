function pititle(name)

title(name)
if(~iscell(name))
    set(gcf,'name',name,'numbertitle','on');
else
    set(gcf,'name',name{1},'numbertitle','on');
end