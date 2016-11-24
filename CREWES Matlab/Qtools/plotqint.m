function hh=plotqint(Qint,z0,zq,kol,marker,lw,haxe,markerflag)
% PLOTQINT ... plots interval Q values with a line showing the depth range
%
% hh=plotqint(Qint,z0,zq,kol,marker,lw,haxe,markerflag)
%
% This is a simple poltting utility that allows both the interval Q values,
% and the size of the interval to be displayed.
%
% Qint ... vector of interval Q values
% z0 ... vector (same length as Qint) giving the start depths of the
%       averaging interval for each Qint
% zq ... vector (same length as Qint) giving the end depths of the
%       averaging interval for each Qint 
% kol ... colour of the line(s)
% ********** default = 'b' ************
% marker ... marker to place at the Q value
% ********** default = 'o' *************
% lw ... vector of length 1 giving the linewidths. lw(1) is the linewidth
%        for the line connecting the Q values. lw(2) is the linewidth of
%        the dotted lines showing the averaging interval.
% ********** default = [1 .1] *************
% NOTE: lw(2) may be set to zero to suppress drawing of the dotted line.
% haxe ... handle of the axis to plot in
% ********** default = gca ***********
% markerflag ... 0 means place the marker midway between z0 and zq
%                1 means place the marker at zq
% ********** default = 0 ***********
%
% G.F. Margrave, 2014, CREWES
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

if(nargin<8)
    markerflag=0;
end
if(nargin<7)
    haxe=gca;
end
if(nargin<6)
    lw=[1 .1];
end
if(nargin<5)
    marker='o';
end
if(nargin<4)
    kol='b';
end
axes(haxe);

if(markerflag==0)
    zm=(z0+zq)/2;
elseif(markerflag==1)
    zm=zq;
else
    error('invalid markerflag')
end

hh=zeros(length(Qint)+1);

hh(1)=line(zm,Qint,'color',kol);
set(hh(1),'marker',marker,'linewidth',lw(1));

for k=1:length(Qint)
    if(lw(2)<0)
        hh(k+1)=line([z0(k) zq(k)],[Qint(k) Qint(k)],'color',kol);
        set(hh(k+1),'linestyle',':','linewidth',lw(2));
    else
        hh(k+1)=nan;
    end
end
    