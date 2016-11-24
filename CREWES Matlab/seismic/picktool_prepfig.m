function picktool_prepfig(hfig,haxe,pctwid,marginpixels)
%
% hfig ... handle of figure window where seismic is displayed
% haxe ... handle of axes where seismic is displayed
% pctwid ... seismic axes will be enlarged to this percent of the screen
%       width. 
% ************* default 70% ***************
% marginpixels ... size will be adjusted so that the right margin of the
%       figure window has at least this many pixels open to place the
%       picking controls.
% ************* default 250 ***************
%

if(nargin<3)
    pctwid=85;
end
if(nargin<4)
    marginpixels=300;
end

pctht=pctwid;

%define placement of lower left corner
x0=100;
y0=100;

%get the screensize
ssize=get(0,'screensize');%this will be in pixels
swidth=ssize(3);%screenwidth in pixels
sheight=ssize(4);
%get the figure size
fsize=get(hfig,'position');%this is also pixels
fwidth=fsize(3);
fheight=fsize(4);
%get the axes size
% aunits=get(haxe,'units');%needed to restore later
% set(haxe,'units','pixels');
% asize=get(haxe,'position');
% awidth=asize(3);%current axes width in pixels
% aheight=asize(4);%current axes height in pixels

fwidthnew=swidth*pctwid/100;
fhtnew=sheight*pctht/100;

set(gcf,'position',[x0 y0 fwidthnew fhtnew])

%adjust width of seismic axes to make room for buttons
apos=get(haxe,'position');%should be normalized units
rtmargin=1-apos(1)-apos(3);%rt margin in normalized units
rtmarginpix=rtmargin*fwidthnew;
if(rtmarginpix<marginpixels)
    %shrink the seismic axes a bit
    rtmarginnew=marginpixels/fwidthnew;
    awidthnew=1-apos(1)-rtmarginnew;
    set(haxe,'position',[apos(1:2) awidthnew apos(4)]);
end
