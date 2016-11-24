function arrow3(x,y,z,xzAng,xyAng,length,colour,style,width,mid,barbW)
%function arrow3(x,y,z,xzAng,xyAng,length,colour,style,width,mid,barbW)
%  Plot a set of identical arrows at 3D positions defined by the
%     x, y, and z row vectors 
%The input parameters are
%x       .... Mid points of arrows in X
%y       .... Mid points of arrows in Y
%z       .... Mid points of arrows in Z
%xzAng   .... Angle from the Z axis (as in spherical co-ords)
%xyAng   .... Angle from the X axis in the XY plane
%length  .... Arrow length
%colour  .... Arrow colour ('k','b','g','r',.....)
%style   .... Arrow style ('-',':',.....)
%width   .... Arrow lines width(1,2,.....)
%mid     .... Arrow centre line length, fraction of arrow length
%barbW   .... Arrow barb width, fraction of arrow length
%
% P.M. Manning, Dec 2011
%
% NOTE: This SOFTWARE may be used by any individual or corporation for any purpose
% with the exception of re-selling or re-distributing the SOFTWARE.
% By using this software, you are agreeing to the terms detailed in this software's
% Matlab source file.

% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by
% its 'AUTHOR' (identified above) and the CREWES Project.  The CREWES
% project may be contacted via email at:  crewesinfo@crewes.org
%
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) This SOFTWARE may be used by any individual or corporation for any purpose
%    with the exception of re-selling or re-distributing the SOFTWARE.
%
% 2) The AUTHOR and CREWES must be acknowledged in any resulting publications or
%    presentations
%
% 3) This SOFTWARE is provided "as is" with no warranty of any kind
%    either expressed or implied. CREWES makes no warranties or representation
%    as to its accuracy, completeness, or fitness for any purpose. CREWES
%    is under no obligation to provide support of any kind for this SOFTWARE.
%
% 4) CREWES periodically adds, changes, improves or updates this SOFTWARE without
%    notice. New versions will be made available at www.crewes.org .
%
% 5) Use this SOFTWARE at your own risk.
%
% END TERMS OF USE LICENSE
if nargin<7
  colour = 'k'; %black
end
if nargin<8
  style = '-';  %solid line
end
if nargin<9
  width = 1;   %line width
end
if nargin<10
  mid = 99;     %plot at mid arrow, 0-none
end
if nargin<11
  barbW = 0.3;  %fraction of length for barb 
end
% xzAng(le) (theta) is 0 in positive z direction, increases  to positive x
% xyAng(le) (phi) is 0 in positive x direction, increases  to positive y
crw = barbW;                    %Width of cross
brbw = barbW*0.5;               %Width of barb
brbc = 1-brbw*3;                %Distance to barb
halfl = length*0.5;
% Specify end points of each straight line segment
xnk = x-sin(xzAng)*cos(xyAng)*halfl;
xpt = x+sin(xzAng)*cos(xyAng)*halfl;
ynk = y-sin(xzAng)*sin(xyAng)*halfl;
ypt = y+sin(xzAng)*sin(xyAng)*halfl;
znk = z-cos(xzAng)*halfl;
zpt = z+cos(xzAng)*halfl;
% Make arrow point
xbarb1 = x+brbc*sin(xzAng)*cos(xyAng)*halfl-brbw*cos(xzAng)*sin(xyAng)*halfl;
xbarb2 = x+brbc*sin(xzAng)*cos(xyAng)*halfl+brbw*cos(xzAng)*sin(xyAng)*halfl;
ybarb1 = y+brbc*sin(xzAng)*sin(xyAng)*halfl-brbw*cos(xzAng)*cos(xyAng)*halfl;
ybarb2 = y+brbc*sin(xzAng)*sin(xyAng)*halfl+brbw*cos(xzAng)*cos(xyAng)*halfl;
zbarb1 = z+brbc*cos(xzAng)*halfl+brbw*sin(xzAng)*halfl;
zbarb2 = z+brbc*cos(xzAng)*halfl-brbw*sin(xzAng)*halfl;
if mid>0
	% Mark centre of arrow
	xm2 = x-crw*cos(xzAng)*sin(xyAng)*halfl;
	xm1 = x+crw*cos(xzAng)*sin(xyAng)*halfl;
	ym2 = y-crw*cos(xzAng)*cos(xyAng)*halfl;
	ym1 = y+crw*cos(xzAng)*cos(xyAng)*halfl;
	zm2 = z+crw*sin(xzAng)*halfl;
	zm1 = z-crw*sin(xzAng)*halfl;
    xm = [xnk,xm1,xbarb1,xbarb2;xpt,xm2,xpt,xpt];
    ym = [ynk,ym1,ybarb1,ybarb2;ypt,ym2,ypt,ypt];
    zm = [znk,zm1,zbarb1,zbarb2;zpt,zm2,zpt,zpt];
else
    xm = [xnk,xpt,xpt;xpt,xbarb1,xbarb2];
    ym = [ynk,ypt,ypt;ypt,ybarb1,ybarb2];
    zm = [znk,zpt,zpt;zpt,zbarb1,zbarb2];
end
h = plot3(xm,ym,zm);
set(h,'color',colour,'LineStyle',style,'LineWidth',width);