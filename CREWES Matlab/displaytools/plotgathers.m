function ha=plotgathers(gathers,xs,ys,xlabels,ylabels,titles,secondarytitle)
%PLOTGATHERS ... plot a set of identical gathers in a conveniant way
%
% plotgathers(gathers,xs,ys,xlabels,ylabels,titles,secondarytitle)
%
% Creates an image plot (with optional wiggle traces) where each gather is
% displayed in an identical size axes. The axes are all full-sized and
% stacked on top of one another with only the topmost axes visible at any
% time. User interface controls are provided to step through the gathers
% and to toggle back and forth between gathers of interest. The gathers are
% all displayed in true relative amplitude to each other and controls are
% provided to specify clipping and to display wiggle traces on top of the
% image plot.  More controls are provided to copy any images to the
% clipboard for pasting into PowerPoint and to make movies of the gathers.
% The "copy_to_clipboard" option only works for Windows but on other
% machines (Mac or Unix) it will create a tif file. The movie option
% provides a good method to show the gathers in PowerPoint. To use the
% movie option, first display the starting gather and then set the slider
% increment. Clicking the "Make movie" button will then cause a movie file
% (*.avi) to be created where each gather is shown for 1 second.
%
% gathers ... cell array or 3D matrix of seismic trace gathers (2D matrices).
%       The 3D matrix form may be used if the gathers are all of the same
%       size, while the cell array option works for any sizes. Any number
%       of gathers is permitted. If the gathers are all identical in size,
%       then the x and y coordinates may be simple vectors, otherwise they
%       should be cells.
% xs ... a cell array of the same length as gathers where each cell
%       contains the x coordinate vector of the corresponding gather. The
%       length of the x coordinate vector must be the same as the number of
%       columns of the corresponding gather. NOTE: If all gathers are
%       identical in size, then xs may be a single vector of coordinates.
% ys ... a cell array of the same length as gathers where each cell
%       contains the y coordinate vector of the corresponding gather. The
%       length of the y coordinate vector must be the same as the number of
%       rows of the corresponding gather. NOTE: If all gathers are
%       identical in size, then ys may be a single vector of coordinates.
% xlabels ... a cell array of the same length as gathers where each cell
%       contains the x axis label (a text string) of the corresponding gather.
%       NOTE: Maye be input as a single text string which will be used to
%       label the x axis on each gather.
% ylabels ... a cell array of the same length as gathers where each cell
%       contains the y axis label (a text string) of the corresponding gather.
%       NOTE: Maye be input as a single text string which will be used to
%       label the y axis on each gather.
% titles ... a cell array of the same length as gathers where each cell
%       contains the gather title (a text string) of the corresponding
%       gather. NOTE: This may not be input as a single title. Each gather
%       must have a unique title.
% secondarytitle ... a single test string that will appear as a second
%       title line on each snapshot
% *********** default = '' ************
%
% ha ... vector of handles of the axes of each gather
%
% For an example of the use of PLOTGATHERS, see DEMO_AFD_MAKESNAPSHOTS in
% the finitedif toolbox.
%
% by Gary Margrave, January 2014
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
global BIGFIG_X
if(ischar(gathers))
    action=gathers;
else
    action='init';
end

if(strcmp(action,'init'))
    
    if(~iscell(gathers))
        [l1,l2,l3]=size(gathers);
        if((l1-1)*(l2-1)==0)
            %this means a vector has been input
            error('gathers must be a cell array or 3D matrix')
        end
        tmp=gathers;
        gathers=cell(1,l3);
        for k=1:l3
            gathers{k}=tmp(:,:,k);
        end
        clear tmp
    end
    ngath=length(gathers);
    if(iscell(ys))
        if(length(ys)~=ngath)
            error('if a cell array, length(ys) must equal length(gathers)');
        end
    else
        tmp=ys;
        ys=cell(1,ngath);
        [ys{:}]=deal(tmp);
    end
    if(iscell(xs))
        if(length(xs)~=ngath)
            error('if a cell array, length(xs) must equal length(gathers)');
        end
    else
        tmp=xs;
        xs=cell(1,ngath);
        [xs{:}]=deal(tmp);
    end
    if(iscell(ylabels))
        if(length(ylabels)~=ngath)
            error('if a cell array, length(ylabels) must equal length(gathers)');
        end
    else
        tmp=ylabels;
        ylabels=cell(1,ngath);
        [ylabels{:}]=deal(tmp);
    end
    if(iscell(xlabels))
        if(length(xlabels)~=ngath)
            error('if a cell array, length(xlabels) must equal length(gathers)');
        end
    else
        tmp=xlabels;
        xlabels=cell(1,ngath);
        [xlabels{:}]=deal(tmp);
    end
    if(iscell(titles))
        if(length(titles)~=ngath)
            error('if a cell array, length(titles) must equal length(gathers)');
        end
    else
        tmp=titles;
        titles=cell(1,ngath);
        [titles{:}]=deal(tmp);
    end
    
    if(nargin<7)
        secondarytitle=[];
    end
    
    %go through the gathers to get the min and max
    amin=10^4;
    amax=-amin;
    for k=1:ngath
        test=min(gathers{k}(:));
        if(test<amin)
            imin=k;
            amin=test;
        end
        test=max(gathers{k}(:));
        if(test>amax)
            imax=k;
            amax=test;
        end
    end
    %measure mean and stddev
    numbers=[gathers{imin}(:);gathers{imax}(:)];%use these two gathers for statistics
    aveamp=mean(numbers);
    stddev=std(numbers);
    adat=[amin amax aveamp stddev];
    clip=inf;
    figure
    ha=cell(1,ngath);%axes handles
    hi=ha;%image handles
    pos=[0.250    0.1100    0.7250    0.8];
    axlims=zeros(ngath,4);
    for k=1:ngath
        ha{k}=axes('position',pos);
        gath=gathers{k};
        if(iscomplex(gath))
            error('complex input not supported in plotgathers')
        end
        x=xs{k};
        y=ys{k};
        if(length(x)~=size(gath,2))
            error(['Gather ' int2str(k) ' has an x coordinate that is the wrong size']);
        end
        if(length(y)~=size(gath,1))
            error(['Gather ' int2str(k) ' has an y coordinate that is the wrong size']);
        end
        % plotgath is an internal function that plots each gather. It
        % returns a handle to the image plot.
        hi{k}=plotgath(ha{k},gath,x,y,xlabels{k},ylabels{k},...
            titles{k},adat,clip,'seisclrs',1,'r',4,120,secondarytitle);

        %capture the nominal axis limits to control unzooming
        axlims(k,1:2)=get(ha{k},'xlim');
        axlims(k,3:4)=get(ha{k},'ylim');
        visaxes(ha{k},axlims(k,:));%turn axes off
    end
    
    visaxes(ha{1},axlims(1,:));%turn on first axes
    %     hpanel=buttonarray(gcf,[.01 .87 .2 .08],ngath,ones(1,ngath));
    %user data of hpanel is an 3-by-ngath matrix with the first row giving
    %the values of the radio buttons, the second row giving the handles of
    %the buttons, and the third row giving the handles of the text labels
    xnow=.01;ynow=.85;width=.15;height=.1;
    huipan1=uipanel('position',[xnow ynow width height]);
    xnow=.01;ynow=.4;width=.98;height=.58;
    hchlbl=uicontrol(huipan1,'style','text','string',{'Chose gather';...
        '(All gathers are shown in';'true relative amplitude.)'},...
        'value',1,'units','normalized','position',...
        [xnow,ynow,width,height],'userdata',axlims);
    xnow=.01;ynow=0;width=.98;height=.35;
    hchoose=uicontrol(huipan1,'style','popupmenu','string',titles,...
        'value',1,'units','normalized','position',[xnow,ynow,width,height],...
        'callback','plotgathers(''choose'');',...
        'tooltipstring','Select gather','userdata',{1 ha});
    %user data of hchoose is the index of the displayed gather followed by
    %the vector of all axes handles
    
    %make a uipanel to put the slider and its increment control in
    xnow=.01; ynow=.77;width=.15;height=.07;
    huipan2=uipanel('position',[xnow ynow width height]);
    %     xnow=.01;ynow=ynow-height-.005;width=.15;height=.05;
    s1=1/(ngath-1);s2=2*s1;%slider steps
    hslider=uicontrol(huipan2,'style','slider','min',1,'max',ngath,'value',1,...
        'units','normalized','position',[.01,.6,.98,.35],...
        'callback','plotgathers(''slide'');','sliderstep',sort([s1 s2]),...
        'tooltipstring','step through gathers','userdata',1);
    %userdata of hslider is the previous position of the slider. This
    %allows implementing slider wraparopund.
    %the above does not work because the Matlab slider does not execute the
    %callback when the slider is asked to move beyond the maximum or
    %minimum. So forget you ever read this comment.
    %     xnow=.01+width;ynow=ynow-height;width=.03;height=.05;
    hinc=uicontrol(huipan2,'style','popupmenu','value',1,...
        'string',{'1';'2';'3';'4';'5';'6'},...
        'units','normalized','position',[.01,.1,.22,.4],...
        'callback','plotgathers(''step'');','userdata',[s1 s2],...
        'tooltipstring','set slider step');
    hinclbl=uicontrol(huipan2,'style','text','string','Slider increment',...
        'units','normalized','position',[.24 .03 .7 ,.4],...
        'horizontalalignment','left');
    
    xnow=.01;ynow=.67;width=.15;height=.09;
    huipan3=uipanel('position',[xnow,ynow,width,height]);
    levels=[50 30 25 20 15 10 9 8 7 6 5 4 3 2 1 .5 .25 .1 .01 .001];
    hcliplbl=uicontrol(huipan3,'style','text','string','Chose clip level',...
        'units','normalized','position',[.01,.4,.45,.58],...
        'userdata',levels);
    levels=cellstr(num2str(levels'));
    levels{1}='none';
    %     xnow=.01;ynow=ynow-height-.005;width=.15;height=.05;
    hclip=uicontrol(huipan3,'style','popupmenu','string',levels,...
        'value',1,'units','normalized','position',[.01,0,.45,.4],...
        'callback','plotgathers(''clip'');',...
        'tooltipstring','Specify clipping','userdata',adat);
    wigmax=[0 50 30 20 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1];
    thiswig=16;
    wigglemax=wigmax(thiswig);
    hwiglbl=uicontrol(huipan3,'style','text','string','Max trace excursion',...
        'units','normalized','position',[.51,.4,.45,.58],...
        'userdata',wigmax);
    wigmax=cellstr(num2str(wigmax'));
    wigmax{1}='none';
    %     xnow=.01;ynow=ynow-height-.005;width=.15;height=.05;
    hwig=uicontrol(huipan3,'style','popupmenu','string',wigmax,...
        'value',thiswig+1,'units','normalized','position',[.51,0,.45,.4],...
        'callback','plotgathers(''wiggle'');',...
        'tooltipstring','Specify trace excursion','userdata',wigglemax);
    xnow=.01;ynow=.56;width=.15;height=.1;
    hbg=uibuttongroup('position',[xnow ynow width height],'userdata',1,...
        'selectionchangefcn','plotgathers(''wt'');');
    uicontrol('parent',hbg,'style','radiobutton','string','Wiggle traces off',...
        'value',1,'units','normalized','position',[0,2/3,1,1/3],'min',0,...
        'max',1,'tag','nowt');
    uicontrol('parent',hbg,'style','radiobutton','string','Wiggle traces',...
        'value',0,'units','normalized','position',[0,1/3,1,1/3],'min',0,...
        'max',1,'tag','wt');
    uicontrol('parent',hbg,'style','radiobutton','string','Wiggle traces variable area',...
        'value',0,'units','normalized','position',[0,0,1,1/3],'min',0,...
        'max',1,'tag','wtva');
    xnow=.01;ynow=ynow-.06;width=.04;height=.05;
    hmark=uicontrol('style','pushbutton','string','Mark','Units','Normalized',...
        'position',[xnow ynow width height],'callback','plotgathers(''mark'');',...
        'tag','mark','userdata',zeros(1,ngath),'tooltipstring','mark for toggling');
    xnow=xnow+width+.01;width=.04;height=.05;
    hunmark=uicontrol('style','pushbutton','string','unMark','Units','Normalized',...
        'position',[xnow ynow width height],'callback','plotgathers(''unmark'');',...
        'tag','unmark','userdata',hmark,'tooltipstring','unmark for toggling');
    xnow=xnow+width+.01;width=.05;height=.05;
    hunmarkall=uicontrol('style','pushbutton','string','unMarkAll','Units','Normalized',...
        'position',[xnow ynow width height],'callback','plotgathers(''unmark'');',...
        'tag','unmarkall','userdata',hmark,'tooltipstring','disable toggling');
    xnow=.01;ynow=ynow-height;width=.15;height=.05;
    htoggle=uicontrol('style','pushbutton','string','Toggle bewteen marked gathers','Units','Normalized',...
        'position',[xnow ynow width height],'callback','plotgathers(''toggle'');',...
        'tag','toggle','visible','on','tooltipstring','cycle through marked gathers',...
        'enable','off');
    
    xnow=.01;ynow=ynow-.06;width=.07;height=.05;
    hpolarity=uicontrol('style','text','string',{' ';'Normal polarity'},'Units','Normalized',...
        'position',[xnow ynow width height],'backgroundcolor','c',...
        'userdata',ones(1,ngath),'tooltipstring','polarity of current gather');
    xnow=xnow+.01+width;width=.07;height=.05;
    hflip=uicontrol('style','pushbutton','string','Flip polarity','Units','Normalized',...
        'position',[xnow ynow width height],'callback','plotgathers(''flip'');',...
        'tag','flip','userdata',zeros(1,ngath),'tooltipstring','flip polarity');
    %make a copy button
    if(ispc)
        buttit='Copy current image to clipboard';
        callback='plotgathers(''clipboard'')';
    else
        buttit='Write current image to TIFF file';
        callback='plotgathers(''clipboard'')';
    end
    xnow=.01;width=.15;height=.05;ynow=ynow-.06;
    hcopybutton=uicontrol('style','pushbutton','string',buttit,'units','normalized',...
        'position',[xnow,ynow,width,height],'callback',callback);
    
    %make movie button and message
    xnow=.01;width=.15;height=.05;ynow=ynow-.07;
    hmoviebutton=uicontrol('style','pushbutton','string','Make movie','units','normalized',...
        'position',[xnow,ynow,width,height],'callback','plotgathers(''makemovie'');');
    msg{1}='Display starting axes for movie.';
    msg{2}='Slider increment is frame step.';
    xnow=.01;width=.15;height=.05;ynow=ynow-.06;
    uicontrol('style','text','string',msg,'Units','Normalized',...
        'position',[xnow ynow width height]);
    
    %zoom message
    xnow=.01;width=.15;height=.1;ynow=ynow-.12;
    msg{1}='Zoom: Click the left mouse button and draw a rectangle.';
    msg{2}='Unzoom: Click the left mouse button once.';
    uicontrol('style','text','string',msg,'Units','Normalized',...
        'position',[xnow ynow width height]);
    
    handles={hchlbl hchoose hcliplbl hclip hslider hbg hmark hunmark ...
        hunmarkall htoggle hpolarity hflip hinc hinclbl hwig hwiglbl ...
        hcopybutton hmoviebutton};
    set(gcf,'userdata',{handles;xs;ys;xlabels;ylabels;titles;ha;hi;secondarytitle});
    %enlarge to 1/4 screen size
    screen=get(0,'screensize');
    f1=1/4;f2=1/2;
    position=[f1*screen(3) f1*screen(4) f2*screen(3) f2*screen(4)];
    if(~isempty(BIGFIG_X))
        bigfig;
    else
        set(gcf,'Units','Pixels','position',position);
    end
    bigfont(gcf,1.5,1);whitefig;
    simplezoom(1,'plotgathers(''zoom'');');
elseif(strcmp(action,'choose'))
    figdat=get(gcf,'userdata');
    handles=figdat{1};
    hchlabel=handles{1};
    hchoose=handles{2};
    axlims=get(hchlabel,'userdata');
    hslider=handles{5};
    ichoice=get(hchoose,'value');
    udat=get(hchoose,'userdata');
    icurrent=udat{1};
    haxes=udat{2};%vectors of axes handles
    hachoice=haxes{ichoice};
    hacurrent=haxes{icurrent};
    visaxes(hacurrent,axlims(icurrent,:));
    visaxes(hachoice,axlims(ichoice,:));
    udat{1}=ichoice;
    set(hchoose,'userdata',udat);
    set(hslider,'value',ichoice);
    hpolarity=handles{11};
    polarities=get(hpolarity,'userdata');
    if(polarities(ichoice)==1)
        set(hpolarity,'string',{' ';'Normal Polarity'},'backgroundcolor','c');
    else
        set(hpolarity,'string',{'Reverse Polarity'},'backgroundcolor',[1 .6 .6]);
    end
elseif(strcmp(action,'clip'))
    figdat=get(gcf,'userdata');%figure data
    handles=figdat{1};
    xs=figdat{2};
    ys=figdat{3};
    xlabels=figdat{4};
    ylabels=figdat{5};
    titles=figdat{6};
    secondarytitle=figdat{9};
    hi=figdat{8};
    hchoose=handles{2};
    hclip=handles{4};
    hcliplbl=handles{3};
    hpolarity=handles{11};
    hwig=handles{15};
    wigglemax=get(hwig,'userdata');
    polarities=get(hpolarity,'userdata');
    clipflag=get(hclip,'value');%cliplevel flag
    levels=get(hcliplbl,'userdata');
    if(clipflag==1)
        clip=inf;
    else
        clip=levels(clipflag);
    end
    udat=get(hchoose,'userdata');%axes data
    adat=get(hclip,'userdata');%amp data
    hbg=handles{6};
    wtflag=get(hbg,'userdata');
    ha=udat{2};
    for k=1:length(ha)
        imagedata=polarities(k)*get(hi{k},'cdata');
        hi{k}=plotgath(ha{k},polarities(k)*imagedata,xs{k},ys{k},xlabels{k},ylabels{k},...
            titles{k},adat,clip,'seisclrs',wtflag,'r',wigglemax,120,secondarytitle);
    end
    figdat{8}=hi;
    set(gcf,'userdata',figdat);
    if verLessThan('matlab','8.4')
        bigfont(gcf,1,1);
    else
        bigfont(gcf,1.5,1);
    end

elseif(strcmp(action,'wiggle'))
    figdat=get(gcf,'userdata');
    handles=figdat{1};
    hwig=handles{15};
    hwiglbl=handles{16};
    thiswig=get(hwig,'value');
    wigmax=get(hwiglbl,'userdata');
    if(thiswig==1)
        wigglemax=inf;
    else
        wigglemax=wigmax(thiswig-1);
    end
    set(hwig,'userdata',wigglemax);
    plotgathers('clip');
elseif(strcmp(action,'slide'))
    figdat=get(gcf,'userdata');
    handles=figdat{1};
    hchoose=handles{2};
    hslider=handles{5};
    newchoice=round(get(hslider,'value'));
    prevchoice=get(hslider,'userdata');
    if(newchoice==prevchoice)
        %indicates slider wraparound; however, this does not work
        ngath=get(hslider,'max');
        if(newchoice==1)
            newchoice=ngath;
        else
            newchoice=ngath;
        end
    end
    set(hchoose,'value',newchoice);
    set(hslider,'userdata',newchoice);
    plotgathers('choose');
elseif(strcmp(action,'step'))
    figdat=get(gcf,'userdata');
    handles=figdat{1};
    hslider=handles{5};
    hinc=handles{13};
    factor=get(hinc,'value');
    step=get(hinc,'userdata');
    set(hslider,'sliderstep',sort([factor*step(1) factor*step(2)]));
elseif(strcmp(action,'wt'))
    figdat=get(gcf,'userdata');
    handles=figdat{1};
    hbg=handles{6};
    hk=get(hbg,'children');
    for k=1:length(hk)
        tag=get(hk(k),'tag');
        if(strcmp(tag,'nowt'))
            v=get(hk(k),'value');
            if(v==1)
                set(hbg,'userdata',1);
            end
        elseif(strcmp(tag,'wt'))
            v=get(hk(k),'value');
            if(v==1)
                set(hbg,'userdata',2);
            end
        elseif(strcmp(tag,'wtva'))
            v=get(hk(k),'value');
            if(v==1)
                set(hbg,'userdata',3);
            end
        end
    end
    plotgathers('clip');
elseif(strcmp(action,'mark'))
    figdat=get(gcf,'userdata');
    handles=figdat{1};
    hchoose=handles{2};
    hmark=handles{7};
    htoggle=handles{10};
    currentchoice=get(hchoose,'value');
    titles=get(hchoose,'string');
    chosen=get(hmark,'userdata');
    if(chosen(currentchoice)==0)
        chosen(currentchoice)=1;
        titles{currentchoice}=[titles{currentchoice} '***'];%mark with ***
        set(hmark,'userdata',chosen);
        set(hchoose,'string',titles)
    end
    if(strcmp(get(htoggle,'enable'),'off'))
        set(htoggle,'enable','on');
    end
elseif(strcmp(action,'unmark'))
    figdat=get(gcf,'userdata');
    handles=figdat{1};
    hchoose=handles{2};
    hmark=handles{7};
    hunmark=handles{8};
    %hunmarkall=handles(9);
    htoggle=handles{10};
    currentunchoice=get(hchoose,'value');
    titles=get(hchoose,'string');
    chosen=get(hmark,'userdata');
    if(gco==hunmark)
        if(chosen(currentunchoice)==1)
            chosen(currentunchoice)=0;
            tmp=titles{currentunchoice};
            titles{currentunchoice}=tmp(1:end-3);%remove ***
        end
    else
        %unmark all
        for k=1:length(chosen)
            if(chosen(k)==1)
                chosen(k)=0;
                tmp=titles{k};
                titles{k}=tmp(1:end-3);%remove *
            end
        end
    end
    set(hchoose,'string',titles)
    set(hmark,'userdata',chosen)
    if(sum(chosen)==0)
        set(htoggle,'enable','off');
    end
elseif(strcmp(action,'toggle'))
    figdat=get(gcf,'userdata');
    handles=figdat{1};
    hchlabel=handles{1};
    hchoose=handles{2};
    axlims=get(hchlabel,'userdata');
    hslider=handles{5};
    hmark=handles{7};
    chosen=get(hmark,'userdata');
    imarked=find(chosen==1);%these are the gathers that we toggle between,
    %there should always be at least one of these or the toggle button will
    %be disabled
    
    ichoice=get(hchoose,'value');%the current gather
    ind=find(ichoice==imarked);%Determine if the displayed gather is a marked one
    if(isempty(ind))
        %so here, there is no marked gather displayed
        ichoice=imarked(1);%so we go to the first marked one
    elseif(ind==length(imarked))
        %here, the displayed gather is the last marked gather
        ichoice=imarked(1);%cycle to the first gather
    else
        %if here, then the displayed gather is either the first or not the
        %last marked gather
        ichoice=imarked(ind+1);%toggle to the next one
    end
    udat=get(hchoose,'userdata');
    icurrent=udat{1};%this is the presently displayed gather
    haxes=udat{2};%this is the vector of all axes
    hachoice=haxes{ichoice};%the axes of the gather to be displayed
    hacurrent=haxes{icurrent};%the axes of the current gather
    visaxes(hacurrent,axlims(ichoice,:));%toggle the current gather off
    visaxes(hachoice,axlims(icurrent,:));%toggle the chose gather on
    udat{1}=ichoice;%update the user data
    set(hchoose,'userdata',udat);
    set(hchoose,'value',ichoice);
    set(hslider,'value',ichoice);%set the slider
    hpolarity=handles{11};
    polarities=get(hpolarity,'userdata');
    if(polarities(ichoice)==1)
        set(hpolarity,'string',{' ';'Normal Polarity'},'backgroundcolor','c');
    else
        set(hpolarity,'string',{'Reverse Polarity'},'backgroundcolor',[1 .6 .6]);
    end
elseif(strcmp(action,'flip'))
    figdat=get(gcf,'userdata');
    handles=figdat{1};
    hpolarity=handles{11};
    polarities=get(hpolarity,'userdata');
    hchoose=handles{2};
    udat=get(hchoose,'userdata');
    icurrent=udat{1};%the index of the current gather
    oldpolarity=polarities(icurrent);
    newpolarity=oldpolarity*-1;
    polarities(icurrent)=newpolarity;
    if(newpolarity==-1)
        set(hpolarity,'string',{'Reverse Polarity'},'backgroundcolor',[1 .6 .6]);
    else
        set(hpolarity,'string',{' ';'Normal Polarity'},'backgroundcolor','c');
    end
    set(hpolarity,'userdata',polarities)
    %now flip the plot polarity
    
    %get scaling information
    hclip=handles{4};
    hcliplbl=handles{3};
    clipflag=get(hclip,'value');%cliplevel flag
    levels=get(hcliplbl,'userdata');
    if(clipflag==1)
        clip=inf;
    else
        clip=levels(clipflag);
    end
    hwig=handles{15};
    wigglemax=get(hwig,'userdata');
    udat=get(hchoose,'userdata');%axes data
    adat=get(hclip,'userdata');%amp data
    hbg=handles{6};
    wtflag=get(hbg,'userdata');%are we plotting wiggles??
    ha=udat{2};%axis handles
    hi=figdat{8};%get the image handles
    imagedata=oldpolarity*get(hi{icurrent},'cdata');%recover the data from the current image
    xs=figdat{2};
    ys=figdat{3};
    xlabels=figdat{4};
    ylabels=figdat{5};
    titles=figdat{6};
    secondarytitle=figdat{9};
    hi{icurrent}=plotgath(ha{icurrent},newpolarity*imagedata,xs{icurrent},...
        ys{icurrent},xlabels{icurrent},ylabels{icurrent},...
        titles{icurrent},adat,clip,'seisclrs',wtflag,'r',wigglemax,120,secondarytitle);
    figdat{8}=hi;
    set(gcf,'userdata',figdat);
    if verLessThan('matlab','8.4')
        bigfont(ha{icurrent},1,1);
    else
        bigfont(ha{icurrent},1.5,1);
    end
elseif(strcmp(action,'zoom'))
    figdat=get(gcf,'userdata');
    handles=figdat{1};
    hchlabel=handles{1};
    hchoose=handles{2};
    axlims=get(hchlabel,'userdata');
    haxes=figdat{7};
    %get the limits on the current axis
    xl=get(gca,'xlim');
    yl=get(gca,'ylim');
    %get the original (nominal limits)
%     icurrent=find(gca==haxes);
    %icurrent=findaxes(gca,haxes);
    udat=get(hchoose,'userdata');
    icurrent=udat{1};%the index of the current gather
    xlnom=axlims(icurrent,1:2);
    ylnom=axlims(icurrent,3:4);
    unzoom=0;
    if(diff(xlnom)<diff(xl))
        xl=xlnom;
        unzoom=1;
    end
    if(diff(xl)==1)
        xl=xlnom;
        unzoom=1;
    end
    if(diff(ylnom)<diff(yl))
        yl=ylnom;
        unzoom=1;
        
    end
    if(diff(yl)==1)
        yl=ylnom;
        unzoom=1;
    end
    if(~isempty(xl)&& ~isempty(yl))
        for k=1:length(haxes)
            if(unzoom==1)
                set(haxes{k},'xlim',axlims(k,1:2),'ylim',axlims(k,3:4));
            else
                set(haxes{k},'xlim',xl,'ylim',yl);
            end
        end
    end
elseif(strcmp(action,'clipboard'))
    hideui; %hide any user interface controls
    %expand the current axis
    ud=get(gcf,'userdata');
    handles=ud{1};
    haxes=ud{7};
    hchoose=handles{2};
    iaxis=get(hchoose,'value');
    pos=get(haxes{iaxis},'position');
    set(haxes{iaxis},'position',[.11, pos(2) .85 pos(4)])
    if(ispc)
        print -dbitmap %copy the figure to the clipboard (windows only)
    else
        print -dtiff
    end
    set(haxes{iaxis},'position',pos);
    unhideui
elseif(strcmp(action,'makemovie'))
    %get the output file name
    [file,path]=myuifile(gcf,'*.avi','Choose the output file','put');
    fullfilename=[path file];
    if(file==0)
        return;
    end
    %try opening it to see if it is locked by another progam
    [fid,msg]=fopen(fullfilename,'w');
    if(fid==-1)
        msgbox(['Unable to open file, message given: ' msg],'Movie Failure');
        return
    else
        fclose(fid);
    end
    %determine the starting axes and the increment
    figdat=get(gcf,'userdata');
    handles=figdat{1};
    hchoose=handles{2};
    hinc=handles{13};
    udat=get(hchoose,'userdata');
    ha=udat{2};%vector of axes handles
    icurrent=udat{1};%this is the index of the starting axes
    step=get(hinc,'value');%this is the slider increment
    %put up confirmation dialog
    q=char('Starting axes:','Ending axes:','Axes increment:','Frame rate (frames/sec):','Output file name:');
    a1=cell2char(get(hchoose,'string'));
    a2=cell2char(get(hinc,'string'));
    a3='1';
    a=char(a1,a1,a2,a3,fullfilename);
    flags=[icurrent length(ha) step 1 1];
    askthingsinit('plotgathers(''makemovie2'');',q,a,flags,'Please verify this Movie information');
elseif(strcmp(action,'makemovie2'))
    figdat=get(gcf,'userdata');
    handles=figdat{1};
    hslider=handles{5};
    hchoose=handles{2};
    udat=get(hchoose,'userdata');
    ha=udat{2};%vector of axes handles
    
    a=askthingsfini;
    if(a==-1)
        return;
    end
    fullfilename=a(5,:);
    step=sscanf(a(3,:),'%f');
    framerate=sscanf(a(4,:),'%f');
    
    axesnames=get(hchoose,'string');
    acurrent=deblank(a(1,:));
    aend=deblank(a(2,:));
    for k=1:length(axesnames)
        if(strcmp(acurrent,axesnames{k}))
            icurrent=k;
        end
         if(strcmp(aend,axesnames{k}))
            iend=k;
        end
    end
    
    if(icurrent==iend)
        telluserinit('Movie length is only one frame, movie aborted')
        return
    end
    
    video_obj=VideoWriter(fullfilename);
    video_obj.FrameRate=framerate;%movie will play at 1 second per frame by default
    open(video_obj)
    
    if(icurrent>iend); step=-step; end
    
    kframes=icurrent:step:iend;%these are the axes that will play in the movie
    nframes=length(kframes);
    hideui; %hide any user interface controls
    for k=1:nframes;
        %display the next axes
        set(hchoose,'value',kframes(k));
        set(hslider,'userdata',kframes(k));
        plotgathers('choose');
        %expand the current axis to fill the figure
        haxes=figdat{7};
        iaxis=get(hchoose,'value');
        pos=get(haxes{iaxis},'position');
        set(haxes{iaxis},'position',[.11, pos(2) .85 pos(4)])
        %now take the frame
        frame=getframe(gcf);
        writeVideo(video_obj,frame);
        %restore the axes to the original size
        set(haxes{iaxis},'position',pos);
    end
    %return to starting frame
    set(hchoose,'value',kframes(1));
    set(hslider,'userdata',kframes(1));
    plotgathers('choose');
    unhideui;%restore the user interface controls
    msgbox(['Movie file ' fullfilename ' created containing ' int2str(nframes) ' frames'],'Movie Success');
end
function visaxes(ha,axlims)
%toggles the visible state of the axes ha. If it is on, then it is set to
%off, if off then it becomes on. This is done to the axis and to all its
%children (image, traces, titles, labels, etc)
state=get(ha,'visible');
hk=get(ha,'children');
if(strcmp(state,'on'))
    state='off';
else
    state='on';
    set(gcf,'currentaxes',ha);
end
for k=1:length(hk)
    set(hk(k),'visible',state);%children
end
%check axis limits
xl=get(ha,'xlim');
if(xl(1)<axlims(1))
    xl(1)=axlims(1);
end
if(xl(2)>axlims(2))
    xl(2)=axlims(2);
end
yl=get(ha,'ylim');
if(yl(1)<axlims(3))
    yl(1)=axlims(3);
end
if(yl(2)>axlims(4))
    yl(2)=axlims(4);
end
set(ha,'visible',state,'xlim',xl,'ylim',yl);%axis

function hi=plotgath(hax,gather,x,y,xlbl,ylbl,titl,a,clip,klrmap,wtflag,wtcolor,wigglemax,nwiggles,secondarytitle)
%
% hax    ... axis handle to plot in
% gather ... matrix
% x      ... column coordinate of gather
% y      ... row coordinate of gather
% xlbl   ... string for x axis label
% ylbl   ... string for y axis label
% titl   ... string for titles
% a      ... 4 length vector to determine scaling [amin amax amean stddev]
% clip   ... value of clipping parameter
% klrmap ... string naming colormap to be used
% wtflag ... 1 to plot wiggle traces, 0 otherwise
% wtcolor ... color of wiggle traces
% wigglemax ... maximum trace excursion for wiggle traces (in trace spacings)
% nwiggles ... maximum number of wiggle traces (can cause decimation)
% secondarytitle ... optional second title line
% hi     ... handle of image plot
vis=get(hax,'visible');
set(gcf,'currentaxes',hax);
%set colormap limits
if(isinf(clip))
    aa=max(abs(a));
    cmin=-aa;
    cmax=aa;
else
    cmin=max([a(1) a(3)-clip*a(4)]);
    cmax=min([a(2) a(3)+clip*a(4)]);
end
hi=imagesc(x,y,gather,[cmin cmax]);
colormap(klrmap);
brighten(.4);
xlabel(xlbl);ylabel(ylbl);
if(isempty(secondarytitle))
    title(titl);
else
    title({titl,secondarytitle})
end
if(wtflag>1)
    dx=mean(diff(x));
    ntraces=size(gather,2);
    if(isinf(clip))
        amax=max(abs(a(1:2)));
    else
        amax=clip*a(4);%so amax is clip times the stddev
    end
    if(ntraces<=nwiggles)
        for k=1:ntraces
            s=gather(:,k);%trace
            ss=2*s*dx/amax;%s==amax converts to ss=2*dx which is two trace spacings
            ind=find(abs(ss)>dx*wigglemax);
            if(~isempty(ind))
                ss(ind)=sign(ss(ind))*dx*wigglemax;
            end
            if(wtflag==2)
                line(ss+x(k),y,'color',wtcolor)
            else
                wtva(ss+x(k),y,wtcolor,x(k),1,1,1);
            end
        end
    else
        xnew=linspace(x(1),x(end),nwiggles);
        inew=round((xnew-x(1))/dx)+1;
        xnew=(inew-1)*dx;
        dxnew=mean(diff(xnew));
        ntraces=length(xnew);
        for k=1:ntraces
            s=gather(:,inew(k));%trace
            ss=2*s*dxnew/amax;
            ind=find(abs(ss)>dx*wigglemax);
            if(~isempty(ind))
                ss(ind)=sign(ss(ind))*dx*wigglemax;
            end
            if(wtflag==2)
                line(ss+xnew(k),y,'color',wtcolor)
            else
                wtva(ss+xnew(k),y,wtcolor,xnew(k),1,1,1);
            end
        end
    end
end
if(strcmp(vis,'on'))
    set(hax,'visible','off');
else
    set(hax,'visible','on');
end
xl=get(hax,'xlim');yl=get(hax,'ylim');
visaxes(hax,[xl yl]);