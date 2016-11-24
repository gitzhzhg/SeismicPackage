function y=boxkar(x,xnot,xwid,yht,pct)
%
% y=boxkar(x,xnot,xwid,yht,pct)
% 
% x    ... x coordinate axis
% xnot ... center of the boxcar
% xwid ... width of the boxcar
% yht  ... height of the boxcar
% ********* default =1 *********
% pct ... the ends of the boxcar will have a raised cosine taper of width
%         pct*xwid.
% ********* default =0 *********
% ****Requirement 0<=pct<=50
%
% y ... a column vector the same length as x containing the boxcar
%
% Notes: To window a signal, s, use sw2=s.*box; where box is a boxcar
% window designed to be the same length as s. If the time coordinate for s
% is t, then box=boxkar(t,mean(t),(t(end)-t(1))/10,1); is a unit-height
% boxcar window placed in the center of t with width 10% of the span of
% t. sw will contain a lot of zeros. To truncate the windowed signal to
% only nonzero samples, use ind=box>0;swt=sw(ind);twt=t(ind); and swt will
% be the windowed, truncated signal and twt will be its time coordinate.
%
% by G.F. Margrave, 2015
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

%
if(nargin<5)
    pct=0;
end
if(pct<0 || pct>50)
    error('pct must be between 0 and 50');
end
if(nargin<4)
    yht=1;
end
if(~between(x(1),x(end),xnot,2))
    error('xnot not contained in x')
end
y=zeros(size(x(:)));
ind=between(xnot-.5*xwid,xnot+.5*xwid,x);
y(ind)=yht;
if(pct>0)
    xtaper=pct*xwid/100;
    inwin1=near(x,xnot-.5*xwid,xnot-.5*xwid+xtaper);
    taper1=.5+.5*cos(pi*(x(inwin1)-x(inwin1(1)))/(x(inwin1(end))-x(inwin1(1)))+pi);
    y(inwin1)=y(inwin1).*taper1;
    inwin2=near(x,xnot+.5*xwid-xtaper,xnot+.5*xwid);
    taper2=.5+.5*cos(pi*(x(inwin2)-x(inwin2(1)))/(x(inwin2(end))-x(inwin2(1))));
    y(inwin2)=y(inwin2).*taper2;
end
    
    