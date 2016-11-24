function PI_limptmove2()
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

hobj=gco;
axes1=gca;
% original positions of lines, will be import when reposition
pts=get(axes1,'userdata');
xdat=get(axes1,'xlim');
ydat=get(axes1,'ylim');
cpt=get(axes1,'currentpoint');
newxpt=sort([xdat(1) cpt(1,1) xdat(2)]);
newypt=sort([ydat(1) cpt(1,2) ydat(2)]);
newpt=[newxpt(2) newypt(2)];
set(hobj,'xdata',newxpt(2),'ydata',newypt(2));
h=get(gcf,'userdata');
hlimbox=h{14};
limdata=get(hlimbox,'userdata');
limptdat=limdata{1};
limlndat=limdata{2};
lntop=limlndat(1);
lnlft=limlndat(3);
limcent=limdata{4};
wpt=find(hobj==limptdat);
% 1 - Move
% 0 - Hold
ptstatus={{[1 1] [0 1] [1 0] [0 0]},...
        {[0 1] [1 1] [0 0] [1 0]},...
        {[1 0] [0 0] [1 1] [0 1]},...
        {[0 0] [1 0] [0 1] [1 1]}};
ptstatus=ptstatus{wpt};
cormv={'xdata' 'ydata'};
for ii=1:4
    movept=limptdat(ii);
    ps=ptstatus{ii};
    for jj=1:2
        if(ps(jj)==1)
            set(movept,cormv{jj},newpt(jj));
        end
    end
end
% 1 - Move
% 0 - Hold
lnmv1={[1 0 1 1] [1 0 0 0] [1 1 0 1] [0 0 0 1]};
lnmv2={[0 1 1 1] [0 1 0 0] [0 0 0 1] [1 1 0 1]};
lnmv3={[1 0 0 0] [1 0 1 1] [1 1 1 0] [0 0 1 0]};
lnmv4={[0 1 0 0] [0 1 1 1] [0 0 1 0] [1 1 1 0]};
lnmv={lnmv1 lnmv2 lnmv3 lnmv4};
lnmv=lnmv{wpt};
% cormv={'xdata' 'xdata' 'ydata' 'ydata'};
ptmv=[newxpt(2) newxpt(2) newypt(2) newypt(2)];
for ii=1:4;
    moveln=limlndat(ii);
    lm=lnmv{ii};
    mdat=1:4;
    for jj=1:4
        if(lm(jj)==1)
            mdat(jj) = ptmv(jj);
        else
            dd=pts{ii};
            mdat(jj) = dd(jj);
        end
    end
    xmdat=sort([mdat(1) mdat(2)]);
    ymdat=sort([mdat(3) mdat(4)]);
    set(moveln,'ydata',ymdat,'xdata',xmdat);
end
xdat=get(lntop,'xdata');
ydat=get(lnlft,'ydata');
set(limcent,'xdata',(xdat(2)-xdat(1))/2+xdat(1),'ydata',(ydat(2)-ydat(1))/2+ydat(1));