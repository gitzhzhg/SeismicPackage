function moveline(action)
% MOVELINE ... a simple utility to move a line around
%
% moveline
%
% MOVELINE is designed to allow the user to click on a line object in a
% figure window and drag it around. The mouse click can be with any button
% and only line objects can be moved. This is mainly a teaching tool to
% demo how to write interactive code.
%
% moveline ... after creating a figure window with at least one line object
%               in it, just type moveline and then click and drag your 
%               line(s) around.
%
% G.F. Margrave, CREWES, 2010
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

if(nargin<1) action='init'; end
if(strcmp(action,'init'))
  set(gcf,'windowbuttondownfcn','moveline(''click'')');
  return;
end
if(strcmp(action,'click'))
    hline=gco;
    obj=get(gco,'type');
    if(~strcmp(obj,'line'))
       msgbox('Click on a line!!!')
       return;
    end

    pt=get(gca,'currentpoint');
    set(hline,'userdata',pt(1,1:2));
    %set(hline,'erasemode','xor','linestyle','.');

    set(gcf,'windowbuttonmotionfcn','moveline(''move'')');
    set(gcf,'windowbuttonupfcn','moveline(''fini'')');
    return;
end
if(strcmp(action,'move'))
    hline=gco;

    pt1=get(hline,'userdata');
    pt2=get(gca,'currentpoint');
    pt2=pt2(1,1:2);

    del=pt2-pt1;

    x=get(hline,'xdata');
    y=get(hline,'ydata');

    set(hline,'xdata',x+del(1));
    set(hline,'ydata',y+del(2));
    set(hline,'userdata',pt2);

    return;
end

if(strcmp(action,'fini'))
    hline=gco;
    x=get(hline,'xdata');
    y=get(hline,'ydata');
    xmax=max(x);xmin=min(x);
    ymax=max(y);ymin=min(y);
    set(hline,'erasemode','normal','linestyle','-');
    set(hline,'marker','none');
    set(gcf,'windowbuttondownfcn','moveline(''click'')');
    set(gcf,'windowbuttonmotionfcn','');
    set(gcf,'windowbuttonupfcn','');
    %set(gca,'xlim',xlims);
    %set(gca,'ylim',ylims);
    xlims=get(gca,'xlim');
    ylims=get(gca,'ylim');
    if(xmin<xlims(1) || xmax>xlims(2))
        xint=round(diff(xlims)/100)*10;
        xmin=floor(xmin/xint)*xint;
        xmax=ceil(xmax/xint)*xint;
        set(gca,'xlim',[min([xlims(1) xmin]) max([xlims(2) xmax])]);
    end
    if(ymin<ylims(1) || ymax>ylims(2))
        yint=round(diff(ylims)/100)*10;
        ymin=floor(ymin/yint)*yint;
        ymax=ceil(ymax/yint)*yint;
        set(gca,'ylim',[min([ylims(1) ymin]) max([ylims(2) ymax])]);
    end

    return;
end	