function PI_lmptreset(action)
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

h=get(gcf,'userdata');
if(strcmp(action,'lmptreset'))
%     slavefig=gcf;
    hmaster=h{6};
    set(0,'currentfigure',hmaster');
else
%     hlimbox=h{14};
%     limdat=get(hlimbox,'userdata');
%     slavefig=limdat{3};
end
hmsg=h{2};
hlimbox=h{14};
limdat=get(hlimbox,'userdata');
limlndat=limdat{2};


% [top bottom left right] - reseting Lines
parentaxis=get(limlndat(1),'parent');
xdat=get(parentaxis,'xlim');
ydat=get(parentaxis,'ylim');
set(limlndat(1),'ydata',[ydat(2) ydat(2)],'xdata',[xdat(1) xdat(2)]);
set(limlndat(2),'ydata',[ydat(1) ydat(1)],'xdata',[xdat(1) xdat(2)]);
set(limlndat(3),'xdata',[xdat(1) xdat(1)],'ydata',[ydat(1) ydat(2)]);
set(limlndat(4),'xdata',[xdat(2) xdat(2)],'ydata',[ydat(1) ydat(2)]);
allnums=[ydat xdat];

%update hlimbox userdata
limdat{3}=allnums;
set(hlimbox,'userdata',limdat);

limptdat=limdat{1};
% [upperleft upperright lowerleft lowerright] - reseting points
set(limptdat(1),'ydata',ydat(2),'xdata',xdat(1));
set(limptdat(2),'ydata',ydat(2),'xdata',xdat(2));
set(limptdat(3),'ydata',ydat(1),'xdata',xdat(1));
set(limptdat(4),'ydata',ydat(1),'xdata',xdat(2));
plotimage('limboxrescale');

limcent=limdat{4};
set(limcent,'ydata',(ydat(2)-ydat(1))/2+ydat(1),'xdata',(xdat(2)-xdat(1))/2+xdat(1));
stringinfo='Limit lines have been reset to match axes.';
set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);