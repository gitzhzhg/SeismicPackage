function [h,hminlabel,hmaxlabel,hlabel]=labelledslider(varargin)

nargs=length(varargin);

if(floor(nargs/2)*2~=nargs)
    error('arguments must be in name-value pairs');
end

parent=[];
pos=[];
minval=[];
maxval=[];
val=[];
label=[];
fontsize=10;



for k=1:2:nargs
    name=varargin{k};
    if(~ischar(name))
        error(['expecting input argument ' int2str(k) ' to be a string'])
    end
    switch name
        case 'parent'
            parent=varargin{k+1};
            if(~isgraphics(parent))
                error('parent must be a valid graphics object');
            end
        case 'position'
            pos=varargin{k+1};
            if(any(pos)>1 || any(pos<0))
                error('position must be in normalized units (all values between 0 and 1)');
            end
            if(length(pos)~=4 || ~isnumeric(pos))
                error('position must be a length 4 vector')
            end
        case 'minval'
            minval=varargin{k+1};
            if(~isnumeric(minval) || length(minval)~=1)
                error('minval must be a scalar number');
            end
        case 'maxval'
            maxval=varargin{k+1};
            if(~isnumeric(maxval) || length(maxval)~=1)
                error('maxval must be a scalar number');
            end
            if(maxval<=minval)
                error('maxval must be greater than minval');
            end
        case 'val'
            val=varargin{k+1};
            if(~isnumeric(val) || length(val)~=1)
                error('val must be a scalar number');
            end
            if(val<minval || val>maxval)
                error('val must be between minval and maxval');
            end
        case 'label'
            label=varargin{k+1};
            if(~ischar(label))
                error('label must be a character string');
            end
        case 'fontsize'
            fontsize=varargin{k+1};
            if(~isnumeric(fontsize) || length(fontsize)~=1)
                error('fontsize must be a scalar number');
            end
            if(fontsize<1 || fontsize>60)
                error('invalid fontsize, must be between 1 and 60')
            end
        otherwise
            error('unrecognized parameter name')
    end
end


if(isempty(parent))
    error('parent must be specified')
end
if(isempty(pos))
    error('position must be specified');
end
if(isempty(minval))
    error('minval must be specified');
end
if(isempty(maxval))
    error('maxval must be spefcified');
end
if(isempty(val))
    error('val must be specified');
end

h=uicontrol('parent',parent,'style','slider','units','normalized',...
    'position',pos,'min',minval,'max',maxval,'value',val);

ht=.02*(fontsize/10);
wd=.3*pos(3);
hminlabel=uicontrol('parent',parent,'style','text','units','normalized',...
    'string',num2str(minval),'position',[pos(1), pos(2)-ht,wd,ht],...
    'fontsize',fontsize);
hmaxlabel=uicontrol('parent',parent,'style','text','units','normalized',...
    'string',num2str(maxval),'position',[pos(1)+pos(3)-wd, pos(2)-ht,wd,ht],...
    'fontsize',fontsize);

wd=pos(3);
hlabel=uicontrol('parent',parent,'style','text','units','normalized',...
    'string',label,'position',[pos(1), pos(2)+pos(4),wd,ht],...
    'fontsize',fontsize);