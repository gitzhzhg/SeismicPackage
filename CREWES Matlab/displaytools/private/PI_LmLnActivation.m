function PI_LmLnActivation
% -------------------------------
% ----- Limit Box Callbacks -----
% -------------------------------
%
% This section deals with the call backs for the Limit box that acts
% similar to the slave and master routine that is present plotimage
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

%global SCALE_OPT NUMBER_OF_COLORS GRAY_PCT CLIP COLOR_MAP NOBRIGHTEN PICKS PICKCOLOR XAXISTOP ZOOM_VALUE ZOOM_LOCKS

mainax=findobj(gcf,'type','axes','tag','MAINAXES');
%posax=findobj(gcf,'type','axes','tag','POSITIONAXES');
h=get(gcf,'userdata');
hmsg=h{2};
hi=h{5};
%hscale=h{6};
%hclip=h{7};
hmaster=h{10};
hlimbox=h{14};
ckval=get(hmaster,'value');
cklimbox=get(hlimbox,'value');
if(cklimbox==1)
    set(hlimbox,'value',2);
else
    set(hlimbox,'value',1);
end
if(ckval==3)
    % figure has been slaved to another, asking user to unslave or not
    checkwuser=questdlg('Do you want to unslave present figure?',...
        'Slave Alert','Yes','No','Cancel','Cancel');
    switch checkwuser
    case 'Yes'
        set(hmaster,'value',1);
    case 'No'
        set(hlimbox,'value',1);
        return   
    case 'Cancel'
        set(hlimbox,'value',1);
        return
    end
end
chval=get(hlimbox,'value');
% 1 - Limit box will be shut off
% 2 - Limit Box will be activated
limdat=get(hlimbox,'userdata');
checklns=findobj(gcf,'type','line','tag','LIMITLINE');
if(~isempty(limdat)&&~isempty(checklns))
    % setting limit box off
    if(chval==1)
        set(limdat{1},'visible','off');
        set(limdat{2},'visible','off');
        set(limdat{4},'visible','off');
        set(findobj(gcf,'type','uimenu','tag','LIMITBOXMENU'),'label','Limit Box: ON');
        stringinfo='Visibility of Limits lines has been turned off';
    elseif(chval==2)
        set(findobj(gcf,'type','uimenu','tag','LIMITBOXMENU'),'label','Limit Box: OFF');
        set(limdat{1},'visible','on');
        set(limdat{2},'visible','on');
        set(limdat{4},'visible','on');
        stringinfo='Visibility of Limits lines has been turned on';
    end
else
    % need to build lines, points and measurementfig
    axes1=mainax;
    xdat=get(axes1,'xlim');
    xmin=xdat(1);
    xmax=xdat(2);
    ydat=get(axes1,'ylim');
    ymin=ydat(1);
    ymax=ydat(2);
    col=[1 0 1];
    limlnstyle='--';
    limlnbd='plotimage(''limlnmove'')';
    limlntag='LIMITLINE';
    limptmk='.';
    limptmksz=18;
    limptbd='plotimage(''limptmove'')';
    limpttag='LIMITPOINT';
    lwid=2;
    ddy=get(hi,'ydata');
    ddx=get(hi,'xdata');
    xmin1=xmin; xmax1=xmax; ymin1=ymin; ymax1=ymax;
    dy=(ydat(2)-ydat(1))/120;
    ymin=max([ymin+dy ddy(1)]);
    ymax=min([ymax-dy ddy(end)]);
    dx=(xdat(2)-xdat(1))/120;
    xmin=max([xmin+dx ddx(1)]);
    xmax=min([xmax-dx ddx(end)]);
    dy=0;
    dx=0;
    % plotting lines
    % bottom
    limln1=line([xmin xmax],[ymax-dy ymax-dy],'linestyle',limlnstyle,'buttondownfcn',...
        limlnbd,'color',col,'tag',limlntag,'linewidth',lwid);
    % top
    limln2=line([xmin xmax],[ymin+dy ymin+dy],'linestyle',limlnstyle,'buttondownfcn',...
        limlnbd,'color',col,'tag',limlntag,'linewidth',lwid);
    % Left Side
    limln3=line([xmin+dx xmin+dx],[ymin ymax],'linestyle',limlnstyle,'buttondownfcn',...
        limlnbd,'color',col,'tag',limlntag,'linewidth',lwid);
    % Right Side
    limln4=line([xmax-dx xmax-dx],[ymin ymax],'linestyle',limlnstyle,'buttondownfcn',...
        limlnbd,'color',col,'tag',limlntag,'linewidth',lwid);
    
    % Plotting Points
    % Upper Left
    limpt1=line(xmin+dx,ymax-dy,'marker',limptmk,'buttondownfcn',limptbd,'color',col,'tag',limpttag,...
        'markersize',limptmksz);
    % Upper Right
    limpt2=line(xmax-dx,ymax-dy,'marker',limptmk,'buttondownfcn',limptbd,'color',col,'tag',limpttag,...
        'markersize',limptmksz);
    % Lower left
    limpt3=line(xmin+dx,ymin+dy,'marker',limptmk,'buttondownfcn',limptbd,'color',col,'tag',limpttag,...
        'markersize',limptmksz);
    % Lower Right
    limpt4=line(xmax-dx,ymin+dy,'marker',limptmk,'buttondownfcn',limptbd,'color',col,'tag',limpttag,...
        'markersize',limptmksz);
    
    % Center Point
    limcent=line((xmax1-xmin1)/2+xmin1,(ymax1-ymin1)/2+ymin1,'marker','x','buttondownfcn','plotimage(''limcentmove'')','color',col,...
        'tag',limpttag,'userdata',3,'markersize',8);
    
    % [upper left upper right lower left lower right] [top bottom left side right side]
    limptpositions=[limpt1 limpt2 limpt3 limpt4];
    limlnpositions=[limln1 limln2 limln3 limln4];
   
    mh=[ymin+dy ymax-dy xmin+dx xmax-dx];
    limdata={limptpositions limlnpositions mh limcent};
    set(hlimbox,'userdata',limdata);
    stringinfo='Limit lines have been activated';

    set(findobj(gcf,'type','uimenu','tag','LIMITBOXMENU'),'label','Limit Box: OFF');
end
set(hmsg,'string',stringinfo,'backgroundcolor',[1 1 1]);