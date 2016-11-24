function PI_limptmove(action)
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
global PICKS

delete(findobj(gcf,'type','line','tag','PICKMARKER'));
h=get(gcf,'userdata');
hlimbox=h{14};
limdat=get(hlimbox,'userdata');
st=get(gcf,'selectiontype');

if(strcmp(st,'alt'))
    % right click, creating menu
    axes1=findobj(gcf,'type','axes','tag','AXES1');
    if(ishandle(get(axes1,'userdata')));
        delete(get(axes1,'userdata'));
    end
    cmenu=uicontextmenu;
    set(gco,'uicontextmen',cmenu);
    uimenu(cmenu,'label','Line Options','callback','plotimage(''mvmenuclose'')');
    m2=uimenu(cmenu,'label','Line Style','separator','on');
    uimenu(m2,'label','- (Solid line)','callback','plotimage(''limlnoptions'')','userdata',[1 1]);
    uimenu(m2,'label','-- (Segmented line)','callback','plotimage(''limlnoptions'')','userdata',[1 2]);
    uimenu(m2,'label','_. (Line dot)','callback','plotimage(''limlnoptions'')','userdata',[1 3]);
    uimenu(m2,'label',': (Dotted line)','callback','plotimage(''limlnoptions'')','userdata',[1 4]);
    m3=uimenu(cmenu,'label','Marker');
    uimenu(m3,'label','*','callback','plotimage(''limlnoptions'')','userdata',[2 1]);
    uimenu(m3,'label','x','callback','plotimage(''limlnoptions'')','userdata',[2 2]);
    uimenu(m3,'label','o','callback','plotimage(''limlnoptions'')','userdata',[2 3]);
    uimenu(m3,'label','+','callback','plotimage(''limlnoptions'')','userdata',[2 4]);
    uimenu(m3,'label','.','callback','plotimage(''limlnoptions'')','userdata',[2 5]);
    m4=uimenu(cmenu,'label','Color');
    uimenu(m4,'label','Red','callback','plotimage(''limlnoptions'')','userdata',[3 1]);
    uimenu(m4,'label','Green','callback','plotimage(''limlnoptions'')','userdata',[3 2]);
    uimenu(m4,'label','Blue','callback','plotimage(''limlnoptions'')','userdata',[3 3]);
    uimenu(m4,'label','Black','callback','plotimage(''limlnoptions'')','userdata',[3 4]);
    uimenu(m4,'label','Default','callback','plotimage(''limlnoptions'')','userdata',[3 5]);
    m5=uimenu(cmenu,'label','Spawn Options','separator','on');
    ttl=get(gca,'title');
    ttludat=get(ttl,'userdata');
    nm1=ttludat{3};
    if(strcmp(nm1,'Amp Spectrum'))
        uimenu(m5,'label','None Yet For Spectrums');
    else
        uimenu(m5,'label','Spawn New Figure','callback',@PI_SpawnPlotImage,...
            'userdata',[]);
        uimenu(m5,'label','F-K Amp Spectrum','callback',@PI_limbox_transforms,...
            'userdata',[],'separator','on');
        uimenu(m5,'label','F-X Amp Spectrum','callback',@PI_limbox_transforms,...
            'userdata',[]);
    end
    uimenu(cmenu,'label','Reset Data','callback','plotimage(''lmptresetmenu'')');

    set(axes1,'userdata',cmenu);
else
    if(~isempty(PICKS))
        for ii=1:size(PICKS,1)
            CheckFigure=PICKS{ii,1};
            if(CheckFigure==gcf)
                checkpics=findobj(gcf,'type','line','tag','PICKS');
                if(isempty(checkpics))
                    PICKS{ii,2}=[];
                    PICKS{ii,3}=[];
                    break
                end
                %set(PICKS{ii,3},'erasemode','xor');
            end
        end
    end
    axes1=gca;
    % setting points and lines erase mode to xor
%     lmlm=findobj(gcf,'type','uicontrol','tag','LIMITLINEMASTER');
%     h=get(gcf,'userdata');
%     hlimbox=h{14};
%     limdat=get(hlimbox,'userdata');
%     limpts=limdat{1};
    limlns=limdat{2};
%     limcent=limdat{4};
%     set(limlns,'erasemode','xor');
%     set(limpts,'erasemode','xor');
%     set(limcent,'erasemode','xor');
    set(gcf,'windowbuttonupfcn','plotimage(''limmoveend'')');
    if(strcmp(action,'limptmove'))
        set(gcf,'windowbuttonmotionfcn','plotimage(''limptmove2'')');
        % need to store line positions [top bottom left side right side]
        pts=cell(5,1);
        for ii=1:4
            xdat=get(limlns(ii),'xdata');
            ydat=get(limlns(ii),'ydata');
            tdat=[xdat ydat];
            pts{ii}=tdat;
        end
        pts{5}=get(gca,'currentpoint');
        set(axes1,'userdata',pts);
    elseif(strcmp(action,'limlnmove'))
        set(gcf,'windowbuttonmotionfcn','plotimage(''limlnmove2'')');
        pt=get(axes1,'currentpoint');
        set(axes1,'userdata',pt);
    elseif(strcmp(action,'limcentmove'))
        set(gcf,'windowbuttonmotionfcn',@PI_limcentmove2);
        pt=get(axes1,'currentpoint');
        set(axes1,'userdata',pt);
    end
    set(gcf,'windowbuttonupfcn','plotimage(''limmoveend'')');
end