function h=drawpick(x0,t0,dtdx,dx,figno,col,lw)
% DRAWPICK: draw a line on top of an image (e.g. seismic)
%
% drawpick(x0,t0,dtdx,figno,dx,col,lw)
%
% This function makes it easy to draw a "pick" on top of a seismic
% image. The pick is defined by three numbers (x0,t0,dtdx) that give
% the center of the pick and the timedip. The pick is drawn with the
% specified timedip centered at (x0,t0).
%
% (x0,t0) ... position of the center of the line
% dtdx ... timedip of the line
%	******** default 0 ********
% dx ... horizontal dimension of the line
%	******** default width-of-axes/20 *******
% figno ... figure number to draw in
%       ******** default gcf ********
% col ... color to draw with
%	******** default 'r' *******
% lw ... line thickness to draw with
%	******** lw = 2 **********
%
% h ... handle of the drawn line
%
% G.F. Margrave, CREWES, July 2000
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



if(nargin<3) dtdx=0; end
if(nargin<4)
	hax=get(figno,'currentaxes');
	xlims=get(hax,'xlims');
	xw=diff(xlims)/20;
end
if(nargin<5) figno=gcf; end
if(nargin<6) col='r'; end
if(nargin<7) lw=2; end
	
x1=x0-dx/2;
x2=x0+dx/2;
t1=t0+dtdx*(x1-x0);
t2=t0+dtdx*(x2-x0);
figure(figno);

h=line([x1 x2],[t1 t2],[1 1],'color',col,'linewidth',lw)