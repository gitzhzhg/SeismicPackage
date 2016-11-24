function h=plotblocky(vblock,xblock,klr,lw,ls,dir,haxe)
%
% h=plotblocky(vblock,xblock,klr,lw,ls,haxe)
%
% plot a blocky function as a staircase of values
%
% vblock ... vector of piecewise constant values (the blocky function)
% xblock ... vector of block boundaries of length 1 greater than vblock.
%           vblock(k) extends from xblock(k) to xblock(k+1)
% klr ... color to plot with
% lw ... line width
% ******** default =1 ********
% ls ... linestyle
% ******** default ='-' ********
% dir ... direction of the plot. Either 'v' (vertical or 'h' (horizontal)
% ******** default = 'v' ***********
% haxe ... handle of axes to plot in
% ******** default is the current axes *******

if(nargin<7)
    haxe=gca;
end

if(nargin<6)
    dir='v';
end

if(nargin<5)
    ls='-';
end

if(nargin<4)
    lw=1;
end

if(~(length(vblock)+1==length(xblock)))
    error('xblock must be 1 sample longer than vblock')
end

vblock2=zeros(2*length(vblock),1);
xblock2=vblock2;
vblock2(1:2:end-1)=vblock;
vblock2(2:2:end)=vblock;
xblock2(1:2:end-1)=xblock(1:end-1);
xblock2(2:2:end)=xblock(2:end);

axes(haxe);


if(strcmp(dir,'v'))
    h=line(vblock2,xblock2,'color',klr,'linestyle',ls,'linewidth',lw/2);
else
    h=line(xblock2,vblock2,'color',klr,'linestyle',ls,'linewidth',lw/2);
end