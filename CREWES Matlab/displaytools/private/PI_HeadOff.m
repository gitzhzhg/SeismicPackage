function StopOrGo=PI_HeadOff(action)
%
% function StopOrGo=PI_HeadOff(action)
%
% action ... PickMoveClose does things.
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

%global SCALE_OPT NUMBER_OF_COLORS GRAY_PCT CLIP COLOR_MAP NOBRIGHTEN PICKS PICKCOLOR XAXISTOP ZOOM_VALUE ZOOM_LOCKS
global PICKS ZOOM_VALUE ZOOM_LOCKS

hposax=findobj(gcf,'type','axes','tag','POSITIONAXES');
hmainax=findobj(gcf,'type','axes','tag','MAINAXES');

if(strcmp(get(hmainax,'visible'),'off'))
    return
end

CheckClick=get(gcf,'selectiontype');
StopOrGo='STOP';

if(strcmp(get(gca,'tag'),'POSITIONAXES'))
    box=selboxfini;
    if(length(box)==2)
        if(~ishandle(box{2}))
            return
        end
        delete(box{2});
    end
    return
end

if(strcmp(CheckClick,'normal'))
    if(strcmp(action,'PickMoveClose'))
        set(gcf,'name','Click & Hold MB1 on Markers to move.  MB3 menu, or function change to stop line moving');
        StopOrGo='STOP';
        return
    end
    StopOrGo='GO';
    % skipping
elseif(crisgraphics(gco,'image') && strcmp(CheckClick,'alt'))
    h=get(gcf,'userdata');
    hzoompick=h{9};
    value=get(hzoompick,'value');
    delete(findobj(gcf,'type','line','tag','PICKMARKER'));
    delete(findobj(gcf,'type','text','tag','PICKTEXT'));
    switch value
    case 1
        selboxinit('plotimage(''zoom'')',1);
        set(gcf,'name','Seismic Image Plot, Simplezooming installed (Use MB1)');
    case 2
        drawlineinit('plotimage(''pick'')');
        set(gcf,'name','Seismic Image Plot, Picking resumed (Use MB1)');
    case 3
        drawlineinit('plotimage(''pick'')');
        set(gcf,'name','Seismic Image Plot, Picking new (Use MB1)');
        set(hzoompick,'userdata',[]);
    end
    set(gcf,'pointer','arrow');
    h=get(gcf,'userdata');
    hlimbox=h{14};
    val=get(hlimbox,'value');
    lmboxquestion={'Limit Box: ON' 'Limit Box: OFF'};
    sizequestion={'Publishable Figure: ON' 'Publishable Figure: OFF'};
    cmenu=uicontextmenu;
    set(gco,'UIContextMenu',cmenu);
    m1=uimenu(cmenu,'label','Zoom Options');
    uimenu(m1,'label','Publish Zoom limits','callback','plotimage(''zoomoptions'')','userdata',{1 'publish'});

    if(isempty(ZOOM_VALUE))
        jj=1;
    else
        jj=2;
    end
    MenuLabel={'No Limits Published' 'Match Zoom To Published Limits'};
    en={'off' 'on'};
    uimenu(m1,'label',MenuLabel{jj},'callback','plotimage(''zoomoptions'')','userdata',{1 'match'},...
        'enable',en{jj});

    MenuLabel={'Lock Zoom' 'Unlock Zoom'};
    UDat={{1 'lock'} {1 'unlock'}};
    MenuCall={' ' 'plotimage(''zoomoptions'')'};
    jj=1;
    for ii=1:size(ZOOM_LOCKS,1)
        CheckLock=ZOOM_LOCKS(ii,1);
        if(CheckLock==gcf)
            jj=2;
            break
        end
    end
    m2=uimenu(m1,'label',MenuLabel{jj},'userdata',UDat{jj},'enable','on',...
        'callback',MenuCall{jj});
    if(jj~=2)
        for ii=1:size(PICKS,1)
            if(PICKS{ii,1}~=gcf)
                haxs=findobj(PICKS{ii,1},'type','axes','tag','MAINAXES');
                ttl=get(haxs,'title');
                str=get(ttl,'string');
                if(iscell(str))
                    str=str{1};
                end
                uimenu(m2,'label',['Figure: ' str],...
                    'callback','plotimage(''zoomoptions'')','userdata',{PICKS{ii} 'lock'});
            end
        end
    end
    ttl=get(hmainax,'title');
    ttludat=get(ttl,'userdata');
    nm1=ttludat{3};
    m1=uimenu(cmenu,'label','Import Picks','enable','on');
    if(~isempty(PICKS))
        if(~isempty(PICKS{1,1}))
            set(m1,'enable','off');
            for ii=1:size(PICKS,1)
                if(PICKS{ii,1}==gcf)
                else
                    haxs=findobj(PICKS{ii,1},'type','axes','tag','MAINAXES');
                    ttl=get(haxs,'title');
                    ttludat=get(ttl,'userdata');
                    if(~iscell(ttludat))
                        
                    else
                        nm2=ttludat{3};
                        if(strcmp(nm1,nm2))
                            str=get(ttl,'string');
                            if(iscell(str))
                                str=str{1};
                            end
                            uimenu(m1,'label',['Figure: ' str],...
                                'userdata',PICKS{ii, 2},'callback','plotimage(''ImportPicks'')');
                            set(m1,'enable','on');
                        end
                    end
                end
            end
        else
            set(m1,'enable','off');
        end
    else
        set(m1,'enable','off');
    end
    uimenu(cmenu,'label','Rename Axes','callback',@PI_axis_options,'separator','on');
    uimenu(cmenu,'label','Resample Axes','callback',@PI_axis_options);
    
    uimenu(cmenu,'label',lmboxquestion{val},'callback','plotimage(''LmLnActivation'')',...
        'separator','on');
    checkcolor=get(gcf,'color');
    if(sum(checkcolor)==3)
        ii=2;
    else
        ii=1;
    end
    uimenu(cmenu,'label','Send to Clipboard','callback',@PI_axis_options,'separator','on');
    if(strcmp(get(hposax,'visible'),'on'))
        uimenu(cmenu,'label','Hide Image Controls','callback',@PI_axis_options);
    else
        uimenu(cmenu,'label','Show Image Controls','callback',@PI_axis_options);
    end
    uimenu(cmenu,'label','Data Stats','callback',@PI_axis_options);
    uimenu(cmenu,'label',sizequestion{ii},'callback','plotimage(''figuresizechange'')',...
        'separator','on');
    StopOrGo='STOP';
elseif(crisgraphics(gco,'image')&& strcmp(CheckClick,'extend'))
    % allowing for zoom motion
    h=get(gcf,'userdata');
    hi=h{5};
    dat=get(hi,'cdata');
    if(size(dat,1)*size(dat,2)>=600000)
        StopOrGo='STOP';
        return
    end
    plotimage('zoomscroll');
    StopOrGo='STOP';
elseif(strcmp(CheckClick,'alt'))
    % building uicontext menu for Spawned Plot Image
    cmenu=uicontextmenu;
    set(gcf,'UIContextMenu',cmenu);
    sep='on';
    if(length(findobj(gcf,'type','uimenu','tag','PLOTIMAGEMASTERFIGURE'))==1)
        sep='on';
        uimenu(cmenu,'label','Change Globals','callback',@PI_Global);
        uimenu(cmenu,'label','Spawn New Plot Image','callback','plotimage(''SpawnPlotImage'')','separator',sep);
    end
    checkcolor=get(gcf,'color');
    if(sum(checkcolor)==3)
        ii=2;
    else
        ii=1;
    end
    uimenu(cmenu,'label','Send to Clipboard','callback',@PI_axis_options,'separator','on');
    if(strcmp(get(hposax,'visible'),'on'))
        uimenu(cmenu,'label','Hide Image Controls','callback',@PI_axis_options);
    else
        uimenu(cmenu,'label','Show Image Controls','callback',@PI_axis_options);
    end
    uimenu(cmenu,'label','Data Stats','callback',@PI_axis_options);
    sizequestion={'Publishable Figure: ON' 'Publishable Figure: OFF'};
    uimenu(cmenu,'label',sizequestion{ii},'callback','plotimage(''figuresizechange'')',...
        'separator',sep);
    
end