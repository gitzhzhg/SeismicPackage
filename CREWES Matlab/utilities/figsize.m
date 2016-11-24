function figsize(wd,ht,hfig)
% FIGSIZE: set the size of a figure as a fraction of screensize
%
% figsize(wd,ht,hfig)
%
% wd ... figure width as a fraction of screen width. So, a value of .5 means
%   the figure is half as wide as the screen. A value greater than 1 or
%   less than 0 is silly.
% ht ... figure height as a fraction of screen height. So, a value of .5 means
%   the figure is half as high as the screen. A value greater than 1 or
%   less than 0 is silly.
% hfig ... handle of the figure to be resized
%  *********** default = gcf ************
%
% Note: In most cases, the figure lower left corner will be placed 100
% pixels above and 100 pixels to the right of the screen lower left corner.
% However, if the resizing causes the figure top or figure right edge to
% move off the screen then the figure position is adjusted to the lower
% left corner of the screen.
%
if(nargin<3)
    hfig=gcf;
end

figure(hfig);

if(ht>1)
    ht=1;
end

if(wd>1)
    wd=1;
end

% pos=get(hfig,'position');
sz=get(0,'screensize');

wdpix=wd*sz(3);
htpix=ht*sz(4);

x0pix=100;y0pix=100;
margin=100;
if(x0pix+wdpix+margin>=sz(3))
    x0pix=1;
end
if(y0pix+htpix+margin>=sz(4))
    y0pix=1;
end

set(hfig,'position',[x0pix y0pix wdpix htpix])
