function theodef(action,arg2)

% THEODEF is the central part of a dialog used by LOGEDIT to define
% reflection coefficient sections. Inititate the dialog by calling
% THEODEFINIT and finish
% it by calling THEODEFFINI.
%
% G.F. Margrave November 1994
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

if( strcmp(action,'init'))
    stuff=get(gca,'userdata');
    %     set(hax,'userdata',{transfer msg sonics densities wavelets hmasterfig ...
    %     densopts holealg a m sonicnum densitynum wletnum sectionopt typeopt...
    %     nameopt name});
    transfer=stuff{1};
    msg=stuff{2};
    sonics=stuff{3};
    densities=stuff{4};
    wavelets=stuff{5};
    hmasterfig=stuff{6};
    densopt=stuff{7};
    holealg=stuff{8};
    a=stuff{9};
    m=stuff{10};
    sonicnum=stuff{11};
    densitynum=stuff{12};
    wletnum=stuff{13};
    sectionopt=stuff{14};
    typeopt=stuff{15};
    nameopt=stuff{16};
    name=stuff{17};
    
    fgkol=[0 0 0];
    bgkol=[0 1 1];
    
    %make a new figure
    hfig=figure('visible','off');
    set(hfig,'menubar','none');
    
%     pos=get(hfig,'position');
    
    height=20;
    figheight=10*height+nameopt*height;
    if(wletnum)
        figheight=figheight+height;
    end
    if(typeopt)
        figheight=figheight+height;
    end
    figwidth=402;
    sep=1;
    xnow=sep;
    ynow=figheight-height-sep;
    width=400;
    uicontrol('style','text','position',[xnow,ynow,width,height],'string',...
        'Theogram Computation Specifications','foregroundcolor','r');
    
    xnow=sep;
    ynow=ynow-height-sep;
    if(strcmp(msg,''))
        msg='Blue highlights show important choices';
    end
    hmsg=uicontrol('style','text','position',[xnow,ynow,width,height],'string',...
        msg);
    
    xnow=sep;
    ynow=ynow-height-sep;
    width=140;
    if(sectionopt)
        lbl='Sonic Log Section:';
    else
        lbl='Sonic Log:';
    end
    uicontrol('style','text','string',lbl,'position',...
        [xnow,ynow,width,height]);
    
    xnow=xnow+width+sep;
    width=260;
    hsonicsec=uicontrol('style','popupmenu','string',sonics,'position',...
        [xnow,ynow,width,height],'callback','theodef(''setsection'')',...
        'value',sonicnum,'backgroundcolor',bgkol,'foregroundcolor',fgkol);
    
    xnow=sep;
    ynow=ynow-height-sep;
    width=140;
    if(sectionopt)
        lbl='Density Log Section:';
    else
        lbl='Density Log:';
    end
    uicontrol('style','text','string',lbl,'position',...
        [xnow,ynow,width,height]);
    
    xnow=xnow+width+sep;
    width=260;
    hdensitysec=uicontrol('style','popupmenu','string',densities,'position',...
        [xnow,ynow,width,height],'value',densitynum);
    
    if(wletnum) %wavelets if requested
        xnow=sep;
        ynow=ynow-height-sep;
        width=140;
        uicontrol('style','text','string','Wavelet:','position',...
            [xnow,ynow,width,height]);
        
        xnow=xnow+width+sep;
        width=260;
        hwavelet=uicontrol('style','popupmenu','string',wavelets,'position',...
            [xnow,ynow,width,height],'value',wletnum,...
            'backgroundcolor',bgkol,'foregroundcolor',fgkol);
    else
        hwavelet=0;
    end
    
    xnow=sep;
    ynow=ynow-height-sep;
    width=140;
    uicontrol('style','text','string','Density Options:','position',...
        [xnow,ynow,width,height]);
    
    xnow=xnow+width+sep;
    width=260;
    if(sectionopt)
        doptmsg=['Constant Density' '|Exclusivly Gardners'...
            '|Density Section plus Gardners' '|Exclusively Density Section'];
    else
        doptmsg=['Constant Density' '|Exclusivly Gardners'...
            '|Density Log plus Gardners' '|Exclusively Density Log'];
    end
    hdensopt=uicontrol('style','popupmenu','string',doptmsg,...
        'position',[xnow,ynow,width,height],'callback','theodef(''densopt'')',...
        'value',densopt+1,'backgroundcolor',bgkol,'foregroundcolor',fgkol);
    
    ynow=ynow-height-sep;
    xnow=sep;
    width=150;
    uicontrol('style','text','string','Hole Filling Algorithm:',...
        'position',[xnow,ynow,width,height]);
    
    xnow=xnow+width+sep;
    width=150;
    hholealg=uicontrol('style','popupmenu','string',...
        'Constant|Linear|Mean|Layer Mean|Layer Trend',...
        'position',[xnow,ynow,width,height],'value',holealg);
    
    xnow=sep;
    ynow=ynow-height-sep;
    width=400;
    if(densopt+1>1 && densopt+1<4)
        vis='on';
    else
        vis='off';
    end
    
    
    hgardnerlbl=uicontrol('style','text','string',...
        'Gardners relation is density=a*(vins)^m. Provide a and m:',...
        'position',[xnow,ynow,width,height],'visible',vis);
    
    xnow=sep;
    ynow=ynow-height-sep;
    width=40;
    halbl=uicontrol('style','text','string','a:','position',...
        [xnow,ynow,width,height],'visible',vis);
    
    xnow=xnow+width+sep;
    width=60;
    ha=uicontrol('style','edit','string',num2str(a),'position',...
        [xnow,ynow,width,height],'backgroundcolor',bgkol,...
        'foregroundcolor',fgkol,'visible',vis);
    
    xnow=xnow+width+sep;
    width=40;
    hmlbl=uicontrol('style','text','string','m:','Position',...
        [xnow,ynow,width,height],'visible',vis);
    
    xnow=xnow+sep+width;
    width=60;
    hm=uicontrol('style','edit','string',num2str(m),'position',...
        [xnow,ynow,width,height],'backgroundcolor',bgkol,...
        'foregroundcolor',fgkol,'visible',vis);
    
    if(typeopt)
        ynow=ynow-height-sep;
        xnow=sep;
        width=150;
        uicontrol('style','text','string','Theogram Type:',...
            'position',[xnow,ynow,width,height]);
        
        xnow=xnow+width+sep;
        width=250;
        htype=uicontrol('style','popupmenu','string',...
            'Primaries only|Attenuated Primaries plus Multiples',...
            'position',[xnow,ynow,width,height],'value',typeopt,...
            'backgroundcolor',bgkol,'foregroundcolor',fgkol);
    else
        htype=0;
    end
    
    if(nameopt)
        xnow=sep;
        ynow=ynow-height-sep;
        width=150;
        uicontrol('style','text','string','Name the theograms:','Position',...
            [xnow,ynow,width,height]);
        
        xnow=xnow+width+sep;
        width=250;
        secnames=get(hsonicsec,'string');
        if(length(name)==1)
            name=['theo: ' secnames(sonicnum,:)];
        else
            name=char(name);
        end
        hname=uicontrol('style','edit','string',name, 'position',...
            [xnow,ynow,width,height],'backgroundcolor',bgkol,'foregroundcolor',fgkol);
    else
        hname=0;
    end
    
    %the buttons
    xnow=sep;
    ynow=ynow-height-sep;
    width=60;
    % the done button is enabled when the mode is switched to compute
    hdone=uicontrol('style','pushbutton','string','Done','position',...
        [xnow,ynow,width,height],'callback','theodef(''done'');',...
        'userdata',transfer,'enable','on');
    
    xnow=xnow+width+sep;
    width=60;
    hcancel=uicontrol('style','pushbutton','string','Cancel','position',...
        [xnow,ynow,width,height],'callback','theodef(''cancel'');');
    
    set(hfig,'userdata',{hmasterfig,hmsg,hsonicsec,hdensitysec,hdensopt,...
        hholealg,hgardnerlbl,halbl,ha,hmlbl,hm,hdone,hcancel hname ...
        hwavelet htype});
    
    pospar=get(hmasterfig,'position');
    
    px=pospar(1)+pospar(3)/2-figwidth/2;
    py=pospar(2)+pospar(4)/2-figheight/2;
    set(hfig,'position',[px py figwidth figheight]);
    set(hfig,'visible','on');
    
    return;
    
end

%
% handle the done button
%
if(strcmp(action,'done'))
    h=get(gcf,'userdata');
    hdone=h{12};
    
    theodef('getparameters',gcf);
    
    %call the transfer function
    transfer=get(hdone,'userdata');
    close(gcf);
    
    eval(transfer);
    
    return;
end

%
% do a cancel
%
if(strcmp(action,'cancel'))
    h=get(gcf,'userdata');
    hmasterfig=h{1};
    hdone=h{12};
    
    transfer=get(hdone,'userdata');
    
    hax=get(hmasterfig,'currentaxes');
    
    set(hax,'userdata',-1);
    
    close(gcf);
    
    eval(transfer);
    
    return;
    
end

%
% the density option
%
if(strcmp(action,'densopt'))
    h=get(gcf,'userdata');
    hdensitysec=h{4};
    hdensopt=h{5};
    hgardnerlbl=h{7};
    halbl=h{8};
    ha=h{9};
    hmlbl=h{10};
    hm=h{11};
    flag=get(hdensopt,'value');
    flag=flag-1;
    fgkol=[0 0 0];
    bgkol=[0 1 1];
    
    
    if(flag==1||flag==2)
        set(hgardnerlbl,'visible','on');
        set(halbl,'visible','on');
        set(ha,'visible','on');
        set(hmlbl,'visible','on');
        set(hm,'visible','on');
    else
        set(hgardnerlbl,'visible','off');
        set(halbl,'visible','off');
        set(ha,'visible','off');
        set(hmlbl,'visible','off');
        set(hm,'visible','off');
    end
    if( flag==2 || flag==3)
        set(hdensitysec,'backgroundcolor',bgkol,'foregroundcolor',fgkol);
    else
        set(hdensitysec,'backgroundcolor',[.702 .702 .702],'foregroundcolor','k');
    end
    
    return;
    
end

%
%
%
if(strcmp(action,'getparameters'))
    hfig=arg2;
    h=get(hfig,'userdata');
    hmasterfig=h{1};
    hmsg=h{2};
    hsonicsec=h{3};
    hdensitysec=h{4};
    hdensopt=h{5};
    hholealg=h{6};
    ha=h{9};
    hm=h{11};
%     hdone=h(12);
%     hcancel=h(13);
    hname=h{14};
    hwavelet=h{15};
    htype=h{16};
    hax=get(hmasterfig,'currentaxes');
    
    %get density option
    densopt=get(hdensopt,'value')-1;
    
    if( densopt==1 || densopt==2)
        % get the gardners coefficients
        
        %get a
        a=sscanf(get(ha,'string'),'%f');
        if( a<0 || isempty(a) || isnan(a) )
            set(hax,'userdata',-1);
            figure(hfig);
            set(hmsg,'string','coefficient a must be a positive real number');
            error('coefficient a must be a positive real number');
        end
        
        %get m
        m=sscanf(get(hm,'string'),'%f');
        if( isempty(m) || m<0 || m>1 )
            set(hax,'userdata',-1);
            figure(hfig);
            set(hmsg,'string','Exponent m must be between 0 and 1');
            error('Exponent m must be between 0 and 1');
        end
    else
        a=-1;
        m=-1;
    end
    
    
    
    %get the name
    if (isgraphics(hname))
        if(hname~=0)
            name=get(hname,'string');
            if( strcmp(name,'') || strcmp(name,' ') || length(name)>30 )
                set(hax,'userdata',-1);
                figure(hfig);
                set(hmsg,'string','Provide a non-blank name, max of 30 chars');
                error('Provide a non-blank name');
            end
        else
            name=[];
        end
    else
        name='';
    end
    
    % get the section choices
    sonicnum=get(hsonicsec,'value');
    densitynum=get(hdensitysec,'value');
    
    %get the wavelet choice
    if(isgraphics(hwavelet))
        waveletnum=get(hwavelet,'value');
    else
        waveletnum=0;
    end
    
    %the theogram type
    if(isgraphics(htype))
        theotype=get(htype,'value');
    else
        theotype=0;
    end
    
    
    %the hole algorithm
    holealg=get(hholealg,'value');
    
    %store this stuff
    
    set(hax,'userdata',[sonicnum densitynum waveletnum densopt holealg...
        a m theotype abs(name)]);
    
    return;
end

%
%
%
if(strcmp(action,'setsection'))
    h=get(gcf,'userdata');
    hsection=h{3};
    hname=h{14};
    
    secnames=get(hsection,'string');
    
    secnum=get(hsection,'value');
    
    if(isgraphics(hname))
        set(hname,'string',['theo: ' secnames(secnum,:)]);
    end
    
    return;
end