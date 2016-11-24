function xlabelfontsize(ffactor,haxe)
%
% xlabelfontsize(ffactor,haxe)
%
% ffactor ... scale factor for fonts
% ******* default 2 *******
% haxe ... axes handle
% ******* default gca ******

if(nargin<2)
    haxe=gca;
end
if(nargin<1)
    ffactor=2;
end

xl=get(haxe,'xlabel');
fs=get(xl,'fontsize');
set(xl,'fontsize',ffactor*fs);
%