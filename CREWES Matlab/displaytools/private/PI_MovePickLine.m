function PI_MovePickLine()
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

%global PICKS PICKCOLOR XAXISTOP ZOOM_VALUE ZOOM_LOCKS
global PICKCOLOR

delete(findobj(gcf,'type','line','tag','PICKMARKER'));
delete(findobj(gcf,'type','text','tag','PICKTEXT'));
hobj=gco;
set(gcf,'windowbuttondownfcn','plotimage(''PickMoveClose'')',...
    'windowbuttonupfcn','',...
    'windowbuttonmotionfcn','');
xdat=get(hobj,'xdata');
ydat=get(hobj,'ydata');
% creating markers to
btfcn='plotimage(''MovePickLineStart'')';
pickmarker1=line(xdat(1),ydat(1),'marker','.',...
    'buttondownfcn',btfcn,'color',PICKCOLOR,'tag','PICKMARKER',...
    'userdata',{hobj 1},'markersize',26);
pickmarker2=line(xdat(2),ydat(2),'marker','.',...
    'buttondownfcn',btfcn,'color',PICKCOLOR,'tag','PICKMARKER',...
    'userdata',{hobj 2},'markersize',26);
picktext1=text(xdat(1), ydat(2),['*' num2str(xdat(1)) '*'],...
    'color',[0 0 0],'fontsize',8,'tag','PICKTEXT',...
    'verticalalignment','bottom','horizontalalignment','Left',...
    'visible','off');
% textstr=char(['X:' num2str(xdat(2))],['Y:' num2str(ydat(1))]);
set(hobj,'userdata',[pickmarker1 pickmarker2 picktext1],'buttondownfcn',btfcn);