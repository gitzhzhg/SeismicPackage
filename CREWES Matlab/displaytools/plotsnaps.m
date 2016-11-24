function ha=plotsnaps(snaps,vel,xs,zs,xlabels,zlabels,titles,vtitle,cmap,secondarytitle)
%plotsnaps ... plot a set of wave snapshots in a conveniant way
%
% plotsnaps(snaps,vel,x,z,xlabel,zlabel,titles,vtitle,cmap,secondarytitle)
%
% Creates an image plot where each snapshot is displayed in an identical
% size axes. The snapshots are shown superimposed on top of the velocity
% model for easy correlation of the wavefield and the model. The axes are
% all full-sized and stacked on top of one another with only the topmost
% axes visible at any time. User interface controls are provided to step
% through the snapshots and to toggle back and forth between snaps of
% interest. The snaps are all displayed in true relative amplitude to each
% other and controls are provided to specify clipping and axes aspect
% ratio. More controls are provided to copy any images to the clipboard for
% pasting into PowerPoint and to make movies of the snapshots. The
% "copy_to_clipboard" option only works for Windows but on toher machines
% (Mac or Unix) it will create a tif file. The movie option provides a good
% method to show the snaps in PowerPoint. To use the movie option, first
% display the starting snapshot and then set the slider increment. Clicking
% the "Make movie" button will then cause a movie file (*.avi) to be
% created where each snapshot is shown for 1 second.
%
% snaps ... cell array or 3D matrix of seismic snapshots (snaps=2D matrices).
%       The 3D matrix form may be used if desired. Any number of snaps is
%       permitted.
% vel ... velocity model used to make the snapshots. Must be a matrix the
%       same size as one of the snaps. The velocity model is always shown
%       as the first image.
% x ... vector of x coordinates for vel and the snaps
% z ... vector of z coordinates for vel and the snaps
% zs ... a cell array of the same length as snaps where each cell
% xlabel ... A single text string which will be used to
%       label the x axis on each snapshot.
% zlabel ... a single text string which will be used to
%       label the z axis on each snapshot.
% titles ... a cell array of the same length as snaps where each cell
%       contains the snapshot title (a text string) of the corresponding
%       snapshot. NOTE: This may not be input as a single title. Each snapshot
%       must have a unique title. If any two snapshots have the same title
%       then other features such as movie making may not work properly.
% vtitle ... title of the velocity matrix. Again this should be unique.
% cmap ... colormap used for the displays. Must be a string.
% ************** default 'jet' **************
% secondarytitle ... a single test string that will appear as a second
%       title line on each snapshot
% *********** default = '' ************
%
% ha ... vector of handles of the axes of each snapshot
%
% For an example of the use of plotsnaps, see DEMO_AFD_MAKESNAPSHOTS in
% the finitedif toolbox.
%
% NOTE: The snapshots are superimposed on the velocity model by using the
% snapshot to perturb the values in the velocity matrix according to
% vel_perturbed=vel.*(1+A*snapshot/amax) where amax is the maximum
% amplitude found on any snapshot, vel is the velocity model, and A is
% determined by the amplitude level selected in the user interface.  Thus
% the wavefield and the velocity model will be rendered with the same color
% map. For a more conventional amplitude display of your snapshots, try
% using plotgathers instead.
%
% by Gary Margrave, July 2014
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
if(ischar(snaps))
    action=snaps;
else
    action='init';
end

if(strcmp(action,'init'))
    
    if(nargin<9)
        cmap='jet';
    end
    if(nargin<10)
        secondarytitle='';
    end
    
    if(~iscell(snaps))
        [l1,l2,l3]=size(snaps);
        if((l1-1)*(l2-1)==0)
            %this means a vector has been input
            error('snaps must be a cell array or 3D matrix')
        end
        tmp=snaps;
        snaps=cell(1,l3);
        for k=1:l3
            snaps{k}=tmp(:,:,k);
        end
        clear tmp
    end
    ngath=length(snaps);
    if(iscell(zs))
        if(length(zs)~=ngath)
            error('if a cell array, length(ys) must equal length(snaps)');
        end
    else
        tmp=zs;
        zs=cell(1,ngath+1);
        [zs{:}]=deal(tmp);
    end
    if(iscell(xs))
        if(length(xs)~=ngath)
            error('if a cell array, length(xs) must equal length(snaps)');
        end
    else
        tmp=xs;
        xs=cell(1,ngath+1);
        [xs{:}]=deal(tmp);
    end
    if(iscell(zlabels))
        if(length(zlabels)~=ngath)
            error('if a cell array, length(ylabels) must equal length(snaps)');
        end
    else
        tmp=zlabels;
        zlabels=cell(1,ngath+1);
        [zlabels{:}]=deal(tmp);
    end
    if(iscell(xlabels))
        if(length(xlabels)~=ngath)
            error('if a cell array, length(xlabels) must equal length(snaps)');
        end
    else
        tmp=xlabels;
        xlabels=cell(1,ngath+1);
        [xlabels{:}]=deal(tmp);
    end
    if(length(titles)~=ngath)
        error('if a cell array, length(titles) must equal length(snaps)');
    end
    
    %insert the velocity model as the first snapshot
    snaps{ngath+1}=snaps{ngath};%this grows the snapshots by one
    titles{ngath+1}=titles{ngath};
    for k=ngath-1:-1:1
        snaps{k+1}=snaps{k};
        titles{k+1}=titles{k};
    end
    snaps{1}=vel;
    titles{1}=vtitle;
    
    %go through the snaps to get the min and max
    amin=10^4;
    amax=-amin;
    for k=2:ngath+1
        test=min(snaps{k}(:));
        if(test<amin)
            imin=k;
            amin=test;
        end
        test=max(snaps{k}(:));
        if(test>amax)
            imax=k;
            amax=test;
        end
    end
    %measure mean and stddev
    numbers=[snaps{imin}(:);snaps{imax}(:)];%use these two snaps for statistics
    aveamp=mean(numbers);
    stddev=std(numbers);
    adat=[amin amax aveamp stddev 0];
    Ak=1;%starting amplitude level
    A=10;%empirical amplitude factor
    adat(5)=A;
    figure
    ha=cell(1,ngath+1);%axes handles
    hi=ha;%image handles
    pos=[0.250    0.1100    0.7250    0.80];
    axlims=zeros(4,ngath+1);
    for k=1:ngath+1
        ha{k}=axes('position',pos);
        if(iscomplex(snaps{k}))
            error('complex input not supported in plotsnaps')
        end
        if(k==1)
            gath=snaps{k};
        else
            gath=snaps{k}+1i*snaps{1};
        end

        x=xs{k};
        z=zs{k};
        if(length(x)~=size(gath,2))
            error(['snapshot ' int2str(k) ' has an x coordinate that is the wrong size']);
        end
        if(length(z)~=size(gath,1))
            error(['snapshot ' int2str(k) ' has a z coordinate that is the wrong size']);
        end
        hi{k}=plotgath(ha{k},gath,x,z,xlabels{k},zlabels{k},...
            titles{k},adat,Ak,cmap,secondarytitle);
%         axis equal
%         ylim([min(z) max(z)])
        visaxes(ha{k});%turn axes off
        %capture the nominal axis limits to control unzooming
        axlims(k,1:2)=get(ha{k},'xlim');
        axlims(k,3:4)=get(ha{k},'ylim');
    end
    
    visaxes(ha{1});%turn on first axes
    %     hpanel=buttonarray(gcf,[.01 .87 .2 .08],ngath,ones(1,ngath));
    %user data of hpanel is an 3-by-ngath matrix with the first row giving
    %the values of the radio buttons, the second row giving the handles of
    %the buttons, and the third row giving the handles of the text labels
    xnow=.01;ynow=.85;width=.15;height=.1;
    huipan1=uipanel('position',[xnow ynow width height]);
    xnow=.01;ynow=.4;width=.98;height=.58;
    hchlbl=uicontrol(huipan1,'style','text','string',{'Chose snapshot';...
        '(All snaps are shown in';'true relative amplitude.)'},...
        'value',1,'units','normalized','position',...
        [xnow,ynow,width,height],'userdata',axlims);
    xnow=.01;ynow=0;width=.98;height=.35;
    hchoose=uicontrol(huipan1,'style','popupmenu','string',titles,...
        'value',1,'units','normalized','position',[xnow,ynow,width,height],...
        'callback','plotsnaps(''choose'');',...
        'tooltipstring','Select snapshot','userdata',[1 ha]);
    %user data of hchoose is the index of the displayed snapshot followed by
    %the vector of all axes handles
    
    %make a uipanel to put the slider and its increment control in
    xnow=.01; ynow=.77;width=.15;height=.07;
    huipan2=uipanel('position',[xnow ynow width height]);
    %     xnow=.01;ynow=ynow-height-.005;width=.15;height=.05;
    s1=1/(ngath);s2=2*s1;%slider steps
    hslider=uicontrol(huipan2,'style','slider','min',1,'max',ngath+1,'value',1,...
        'units','normalized','position',[.01,.6,.98,.35],...
        'callback','plotsnaps(''slide'');','sliderstep',sort([s1 s2]),...
        'tooltipstring','step through snaps','userdata',1);
    %userdata of hslider is the previous position of the slider. This
    %allows implementing slider wraparopund.
    %the above does not work because the Matlab slider does not execute the
    %callback when the slider is asked to move beyond the maximum or
    %minimum. So forget you ever read this comment.
    %     xnow=.01+width;ynow=ynow-height;width=.03;height=.05;
    hinc=uicontrol(huipan2,'style','popupmenu','value',1,...
        'string',{'1';'2';'3';'4';'5';'6'},...
        'units','normalized','position',[.01,.1,.22,.4],...
        'callback','plotsnaps(''step'');','userdata',[s1 s2],...
        'tooltipstring','set slider step');
    hinclbl=uicontrol(huipan2,'style','text','string','Slider increment',...
        'units','normalized','position',[.24 .03 .7 ,.4],...
        'horizontalalignment','left');
    
    xnow=.01;ynow=.67;width=.15;height=.09;
    huipan3=uipanel('position',[xnow,ynow,width,height]);
    levels=[500 100 40 30 20 15 10 8 6 4 2 1 .5 .25 .1 .05 .01 .005 .001];
    ilevel=find(levels==Ak);
    levels(ilevel)=-levels(ilevel);%this serves as a flag to indicate the current level
    hcliplbl=uicontrol(huipan3,'style','text','string','Amplitude Level',...
        'units','normalized','position',[.01,.4,.45,.58],...
        'userdata',levels);

    levels=cellstr(num2str(abs(levels)'));
    %     xnow=.01;ynow=ynow-height-.005;width=.15;height=.05;
    hclip=uicontrol(huipan3,'style','popupmenu','string',levels,...
        'value',ilevel,'units','normalized','position',[.01,0,.45,.4],...
        'callback','plotsnaps(''clip'');',...
        'tooltipstring','Specify Amplitude Level','userdata',adat);
    haxelbl=uicontrol(huipan3,'style','text','string','Set Axes Style',...
        'units','normalized','position',[.51,.4,.45,.58]);
    haxe=uicontrol(huipan3,'style','popupmenu','string','Normal|Equal',...
        'value',1,'units','normalized','position',[.51,0,.45,.4],...
        'callback','plotsnaps(''axesstyle'');',...
        'tooltipstring','Specify axes style');
%     wigmax=[0 50 30 20 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1];
%     thiswig=16;
%     wigglemax=wigmax(thiswig);
%     hwiglbl=uicontrol(huipan3,'style','text','string','Max trace excursion',...
%         'units','normalized','position',[.51,.4,.45,.58],...
%         'userdata',wigmax);
%     wigmax=cellstr(num2str(wigmax'));
%     wigmax{1}='none';
%     %     xnow=.01;ynow=ynow-height-.005;width=.15;height=.05;
%     hwig=uicontrol(huipan3,'style','popupmenu','string',wigmax,...
%         'value',thiswig+1,'units','normalized','position',[.51,0,.45,.4],...
%         'callback','plotsnaps(''wiggle'');',...
%         'tooltipstring','Specify trace excursion','userdata',wigglemax);
%     xnow=.01;ynow=.56;width=.15;height=.1;
%     hbg=uibuttongroup('position',[xnow ynow width height],'userdata',1,...
%         'selectionchangefcn','plotsnaps(''wt'');');
%     uicontrol('parent',hbg,'style','radiobutton','string','Wiggle traces off',...
%         'value',1,'units','normalized','position',[0,2/3,1,1/3],'min',0,...
%         'max',1,'tag','nowt');
%     uicontrol('parent',hbg,'style','radiobutton','string','Wiggle traces',...
%         'value',0,'units','normalized','position',[0,1/3,1,1/3],'min',0,...
%         'max',1,'tag','wt');
%     uicontrol('parent',hbg,'style','radiobutton','string','Wiggle traces variable area',...
%         'value',0,'units','normalized','position',[0,0,1,1/3],'min',0,...
%         'max',1,'tag','wtva');
    xnow=.01;ynow=ynow-.06;width=.04;height=.05;
    hmark=uicontrol('style','pushbutton','string','Mark','Units','Normalized',...
        'position',[xnow ynow width height],'callback','plotsnaps(''mark'');',...
        'tag','mark','userdata',zeros(1,ngath+1),'tooltipstring','mark for toggling');
    xnow=xnow+width+.01;width=.04;height=.05;
    hunmark=uicontrol('style','pushbutton','string','unMark','Units','Normalized',...
        'position',[xnow ynow width height],'callback','plotsnaps(''unmark'');',...
        'tag','unmark','userdata',hmark,'tooltipstring','unmark for toggling');
    xnow=xnow+width+.01;width=.05;height=.05;
    hunmarkall=uicontrol('style','pushbutton','string','unMarkAll','Units','Normalized',...
        'position',[xnow ynow width height],'callback','plotsnaps(''unmark'');',...
        'tag','unmarkall','userdata',hmark,'tooltipstring','disable toggling');
    xnow=.01;ynow=ynow-height;width=.15;height=.05;
    htoggle=uicontrol('style','pushbutton','string','Toggle bewteen marked snaps','Units','Normalized',...
        'position',[xnow ynow width height],'callback','plotsnaps(''toggle'');',...
        'tag','toggle','visible','on','tooltipstring','cycle through marked snaps',...
        'enable','off');
    
    xnow=.01;ynow=ynow-.06;width=.07;height=.05;
    hpolarity=uicontrol('style','text','string',{' ';'Normal polarity'},'Units','Normalized',...
        'position',[xnow ynow width height],'backgroundcolor','c',...
        'userdata',ones(1,ngath+1),'tooltipstring','polarity of current snapshot');
    xnow=xnow+.01+width;width=.07;height=.05;
    hflip=uicontrol('style','pushbutton','string','Flip polarity','Units','Normalized',...
        'position',[xnow ynow width height],'callback','plotsnaps(''flip'');',...
        'tag','flip','userdata',zeros(1,ngath+1),'tooltipstring','flip polarity');
    %make a copy button
    if(ispc)
        buttit='Copy current image to clipboard';
        callback='plotsnaps(''clipboard'')';
    else
        buttit='Write current image to TIFF file';
        callback='plotsnaps(''clipboard'')';
    end
    xnow=.01;width=.15;height=.05;ynow=ynow-.06;
    hcopybutton=uicontrol('style','pushbutton','string',buttit,'units','normalized',...
        'position',[xnow,ynow,width,height],'callback',callback);
    
    %make movie button and message
    xnow=.01;width=.15;height=.05;ynow=ynow-.07;
    hmoviebutton=uicontrol('style','pushbutton','string','Make movie','units','normalized',...
        'position',[xnow,ynow,width,height],'callback','plotsnaps(''makemovie'');',...
        'userdata',cmap);
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
    
    handles={hchlbl hchoose hcliplbl hclip hslider nan hmark hunmark ...
        hunmarkall htoggle hpolarity hflip hinc hinclbl haxelbl haxe ...
        hcopybutton hmoviebutton};
    set(gcf,'userdata',{handles;xs;zs;xlabels;zlabels;titles;ha;hi;secondarytitle});
    %enlarge to 1/4 screen size
    screen=get(0,'screensize');
    f1=1/4;f2=1/2;
    position=[f1*screen(3) f1*screen(4) f2*screen(3) f2*screen(4)];
    if(~isempty(BIGFIG_X))
        bigfig;
    else
        set(gcf,'Units','Pixels','position',position);
    end
    bigfont(gcf,1.6,1);whitefig;
    simplezoom(1,'plotsnaps(''zoom'');');
elseif(strcmp(action,'choose'))
    figdat=get(gcf,'userdata');
    handles=figdat{1};
    hchoose=handles{2};
    hslider=handles{5};
    ichoice=get(hchoose,'value');
    udat=get(hchoose,'userdata');
    icurrent=udat{1};
    hachoice=udat{ichoice+1};
    hacurrent=udat{icurrent+1};
    visaxes(hacurrent);
    visaxes(hachoice);
    udat{1}=ichoice;
    set(hchoose,'userdata',udat);
    set(hslider,'value',ichoice);
    hpolarity=handles{11};
    polarities=get(hpolarity,'userdata');
    if(polarities(ichoice)==1)
        set(hpolarity,'string',{' ';'Normal Polarity'},'backgroundcolor','c');
    else
        set(hpolarity,'string','Reverse Polarity','backgroundcolor',[1 .6 .6]);
    end
elseif(strcmp(action,'clip'))
    figdat=get(gcf,'userdata');%figure data
    handles=figdat{1};
    xs=figdat{2};
    zs=figdat{3};
    xlabels=figdat{4};
    zlabels=figdat{5};
    titles=figdat{6};
    secondarytitle=figdat{9};
    hi=figdat{8};
    hchoose=handles{2};
    hclip=handles{4};
    hcliplbl=handles{3};
    hpolarity=handles{11};
    haxe=handles{16};
    axesstyle=get(haxe,'value');
    hmoviebutton=handles{18};
%     wigglemax=get(hwig,'userdata');
    polarities=get(hpolarity,'userdata');
    ilevel=get(hclip,'value');%cliplevel flag
    levels=get(hcliplbl,'userdata');
    ind=find(levels<0);
    Akprev=abs(levels(ind));%previous amplitude level
    levels(ind)=-levels(ind);%unflag
    Ak=levels(ilevel);%new amplitude level
    levels(ilevel)=-levels(ilevel);%set new flag
    set(hcliplbl,'userdata',levels);
    udat=get(hchoose,'userdata');%axes data
    adat=get(hclip,'userdata');%amp data
    cmap=get(hmoviebutton,'userdata');
%     hbg=handles(6);
%     wtflag=get(hbg,'userdata');
    ha=udat(2:end);
    % the data is stored only in the images themselves. Since each image is
    % a combination of the velocity model and the snapshot, the only way to
    % change the amplitude of the snapshot is to decompose the image data
    % into velocity and snapshot, then rescale, and recompose. The velocity
    % and snap are combined into the image data via the formula
    % id=vel.*(1+(A*Ak/amax)*snap) where A is an empirical constant, Ak is
    % the current amplidue level, amax is the maximum absolutie value of
    % the data. Then, given vel, we can reconstruct the snap via
    % snap=(amax/(A*Ak))*(id./vel-1)
    %
    % vel is the image data in the first axis. We do not rescale it
    
    vel=get(hi{1},'cdata');
    amax=adat(2);
    A=adat(5);
    for k=2:length(ha)
        imagedata=polarities(k)*get(hi{k},'cdata');
        snap=(amax/(A*Akprev))*(imagedata./vel-1);
        hi{k}=plotgath(ha{k},polarities(k)*snap+1i*vel,xs{k},zs{k},xlabels{k},zlabels{k},...
            titles{k},adat,Ak,cmap,secondarytitle);
        if(axesstyle==1)
            axis normal
        else
            axis equal
        end
        xlim([xs{k}(1) xs{k}(end)])
        ylim([zs{k}(1) zs{k}(end)])
    end
    figdat{8}=hi;
    set(gcf,'userdata',figdat);
    if verLessThan('matlab','8.4')
        bigfont(gcf,1,1);
    else
        bigfont(gcf,1.6,1);
        bigfont(ha{1},.625,1);
    end
elseif(strcmp(action,'axesstyle'))
    figdat=get(gcf,'userdata');%figure data
    handles=figdat{1};
    xs=figdat{2};
    zs=figdat{3};
    haxe=handles{16};
    axesflag=get(haxe,'value');
    hchoose=handles{2};
    udat=get(hchoose,'userdata');%axes data
%     hbg=handles(6);
%     wtflag=get(hbg,'userdata');
    ha=udat(2:end);
    for k=1:length(ha)
        vis=get(ha{k},'visible');
        set(gcf,'currentaxes',ha{k});
        if(axesflag==1)
            axis normal
        else
            axis equal
        end
        x=xs{k};
        z=zs{k};
        xlim([x(1) x(end)])
        ylim([z(1) z(end)])
        if(strcmp(vis,'on'))
            set(ha{k},'visible','off');
        else
            set(ha{k},'visible','on');
        end
        visaxes(ha{k});
    end
    
% elseif(strcmp(action,'wiggle'))
%     figdat=get(gcf,'userdata');
%     handles=figdat{1};
%     hwig=handles(15);
%     hwiglbl=handles(16);
%     thiswig=get(hwig,'value');
%     wigmax=get(hwiglbl,'userdata');
%     if(thiswig==1)
%         wigglemax=inf;
%     else
%         wigglemax=wigmax(thiswig-1);
%     end
%     set(hwig,'userdata',wigglemax);
%     plotsnaps('clip');
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
    plotsnaps('choose');
elseif(strcmp(action,'step'))
    figdat=get(gcf,'userdata');
    handles=figdat{1};
    hslider=handles{5};
    hinc=handles{13};
    factor=get(hinc,'value');
    step=get(hinc,'userdata');
    set(hslider,'sliderstep',[factor*step(1) factor*step(2)]);
% elseif(strcmp(action,'wt'))
%     figdat=get(gcf,'userdata');
%     handles=figdat{1};
%     hbg=handles(6);
%     hk=get(hbg,'children');
%     for k=1:length(hk)
%         tag=get(hk(k),'tag');
%         if(strcmp(tag,'nowt'))
%             v=get(hk(k),'value');
%             if(v==1)
%                 set(hbg,'userdata',1);
%             end
%         elseif(strcmp(tag,'wt'))
%             v=get(hk(k),'value');
%             if(v==1)
%                 set(hbg,'userdata',2);
%             end
%         elseif(strcmp(tag,'wtva'))
%             v=get(hk(k),'value');
%             if(v==1)
%                 set(hbg,'userdata',3);
%             end
%         end
%     end
%     plotsnaps('clip');
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
    hchoose=handles{2};
    hslider=handles{5};
    hmark=handles{7};
    chosen=get(hmark,'userdata');
    imarked=find(chosen==1);%these are the snaps that we toggle between,
    %there should always be at least one of these or the toggle button will
    %be disabled
    
    ichoice=get(hchoose,'value');%the current snapshot
    ind=find(ichoice==imarked);%Determine if the displayed snapshot is a marked one
    if(isempty(ind))
        %so here, there is no marked snapshot displayed
        ichoice=imarked(1);%so we go to the first marked one
    elseif(ind==length(imarked))
        %here, the displayed snapshot is the last marked snapshot
        ichoice=imarked(1);%cycle to the first snapshot
    else
        %if here, then the displayed snapshot is either the first or not the
        %last marked snapshot
        ichoice=imarked(ind+1);%toggle to the next one
    end
    udat=get(hchoose,'userdata');
    icurrent=udat{1};%this is the presently displayed snapshot
    hachoice=udat(ichoice+1);%the axes of the snapshot to be displayed
    hacurrent=udat(icurrent+1);%the axes of the current snapshot
    visaxes(hacurrent{1});%toggle the current snapshot off
    visaxes(hachoice{1});%toggle the chose snapshot on
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
    haxes=handles{16};
    axesstyle=get(haxes,'value');
    polarities=get(hpolarity,'userdata');
    hchoose=handles{2};
    udat=get(hchoose,'userdata');
    icurrent=udat{1};%the index of the current snapshot
    if(icurrent==1)
        msgbox('You cannot change the polarity of the velocity model','Terribly Sorry...')
        return
    end
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
    ilevel=get(hclip,'value');%cliplevel flag
    levels=get(hcliplbl,'userdata');
    Ak=abs(levels(ilevel));
%     if(ilevel==1)
%         clip=inf;
%     else
%         clip=levels(ilevel);
%     end
%     hwig=handles(15);
    hmoviebutton=handles{18};
%     wigglemax=get(hwig,'userdata');
    udat=get(hchoose,'userdata');%axes data
    adat=get(hclip,'userdata');%amp data
    amax=adat(2);
    A=adat(5);
%     hbg=handles(6);
%     wtflag=get(hbg,'userdata');%are we plotting wiggles??
    ha=udat(2:end);%axis handles
    hi=figdat{8};%get the image handles
%     imagedata=oldpolarity*get(hi(icurrent),'cdata');%recover the data from the current image
    imagedata=get(hi{icurrent},'cdata');
    vel=get(hi{1},'cdata');%velocity model
    snap=oldpolarity*(amax/(A*Ak))*(imagedata./vel-1);
    xs=figdat{2};
    zs=figdat{3};
    xlabels=figdat{4};
    zlabels=figdat{5};
    titles=figdat{6};
    secondarytitle=figdat{9};
    cmap=get(hmoviebutton,'userdata');
    hi{icurrent}=plotgath(ha{icurrent},newpolarity*snap+1i*vel,xs{icurrent},...
        zs{icurrent},xlabels{icurrent},zlabels{icurrent},...
        titles{icurrent},adat,Ak,cmap,secondarytitle);
    if(axesstyle==1)
        axis normal
    else
        axis equal
    end
    xlim([xs{icurrent}(1) xs{icurrent}(end)])
    ylim([zs{icurrent}(1) zs{icurrent}(end)])
    figdat{8}=hi;
    set(gcf,'userdata',figdat);
    if verLessThan('matlab','8.4')
        bigfont(ha{icurrent},1,1);
    else
        bigfont(ha{icurrent},1.6,1);
    end
elseif(strcmp(action,'zoom'))
    figdat=get(gcf,'userdata');
    handles=figdat{1};
    hchlabel=handles{1};
    axlims=get(hchlabel,'userdata');
    haxes=figdat{7};
    %get the limits on the current axis
    xl=get(gca,'xlim');
    yl=get(gca,'ylim');
    %get the original (nominal limits)
    icurrent=findaxes(gca,haxes);
    xlnom=axlims(icurrent,1:2);
    ylnom=axlims(icurrent,3:4);
    if(diff(xlnom)<diff(xl))
        xl=xlnom;
        
    end
    if(diff(xl)==1 || sum(xl)==1)
        xl=xlnom;
    end
    if(diff(ylnom)<diff(yl))
        yl=ylnom;
        
    end
    if(diff(yl)==1 || sum(yl)==1)
        yl=ylnom;
    end
    if(~isempty(xl)&& ~isempty(yl))
        for k=1:length(haxes)
            set(haxes{k},'xlim',xl,'ylim',yl);
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
    ha=udat(2:end);%vector of axes handles
    icurrent=udat{1};%this is the index of the starting axes
    step=get(hinc,'value');%this is the slider increment
    %put up confirmation dialog
    q=char('Starting axes:','Ending axes:','Axes increment:','Frame rate (frames/sec):','Output file name:');
    a1=cell2char(get(hchoose,'string'));
    a2=cell2char(get(hinc,'string'));
    a3='1';
    a=char(a1,a1,a2,a3,fullfilename);
    flags=[icurrent length(ha) step 1 1];
    askthingsinit('plotsnaps(''makemovie2'');',q,a,flags,'Please verify this Movie information');
elseif(strcmp(action,'makemovie2'))
    figdat=get(gcf,'userdata');
    handles=figdat{1};
    hslider=handles{5};
    hchoose=handles{2};
    udat=get(hchoose,'userdata');
    ha=udat(2:end);%vector of axes handles
    
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
        msgbox('Movie length is only one frame, movie aborted')
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
        plotsnaps('choose');
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
    plotsnaps('choose');
    unhideui;%restore the user interface controls
    msgbox(['Movie file ' fullfilename ' created containing ' int2str(nframes) ' frames'],'Movie Success');
end
function visaxes(ha)
%toggles the visible state of the axes ha. If it is on, then it is set to
%off, if off then it becomes on. This is done to the axis and to all its
%children (image, traces, titles, labels, etc)
state=get(ha,'visible');
hk=get(ha,'children');
if(strcmp(state,'on'))
    state='off';
else
    state='on';
end
for k=1:length(hk)
    set(hk(k),'visible',state);%children
end
set(ha,'visible',state);%axis
function hi=plotgath(hax,csnapshot,x,y,xlbl,ylbl,titl,a,Ak,klrmap,secondarytitle)
%
% hax    ... axis handle to plot in
% csnapshot ... complex matrix, real part is the snapshot, imaginary part is the
%               velocity model
% x      ... column coordinate of snapshot
% y      ... row coordinate of snapshot
% xlbl   ... string for x axis label
% ylbl   ... string for y axis label
% titl   ... string for titles
% a      ... 5 length vector to determine scaling [amin amax amean stddev A]
% A      ... empirical scalar
% Ak   ... current amplitude level
% klrmap ... string naming colormap to be used
% secondarytitle ... second title line
% hi     ... handle of image plot
vis=get(hax,'visible');
set(gcf,'currentaxes',hax);
%decompose the complex snapshot
snapshot=real(csnapshot);
vel=imag(csnapshot);
if(sum(abs(vel(:)))==0)
    %this means that we are plotting the first snapshot which is the
    %velocity model
    vel=snapshot;
    snapshot=zeros(size(vel));
end

%set colormap limits
cmin=min(vel(:));
cmax=max(vel(:));
amax=max(a(2));
A=a(5);

%image plot
hi=imagesc(x,y,vel.*(1+(A*Ak/amax)*snapshot),[cmin cmax]);
% colormap(klrmap);
eval(['colormap(' klrmap ')'])
brighten(.4);
xlabel(xlbl);ylabel(ylbl);
if(isempty(secondarytitle))
    title(titl);
else
    title({titl,secondarytitle})
end
% if(wtflag>1)
%     dx=mean(diff(x));
%     ntraces=size(snapshot,2);
%     if(isinf(clip))
%         amax=max(abs(a(1:2)));
%     else
%         amax=clip*a(4);%so amax is clip times the stddev
%     end
%     if(ntraces<=nwiggles)
%         for k=1:ntraces
%             s=snapshot(:,k);%trace
%             ss=2*s*dx/amax;%s==amax converts to ss=2*dx which is two trace spacings
%             ind=find(abs(ss)>dx*wigglemax);
%             if(~isempty(ind))
%                 ss(ind)=sign(ss(ind))*dx*wigglemax;
%             end
%             if(wtflag==2)
%                 line(ss+x(k),y,'color',wtcolor)
%             else
%                 wtva(ss+x(k),y,wtcolor,x(k),1,1,1);
%             end
%         end
%     else
%         xnew=linspace(x(1),x(end),nwiggles);
%         inew=round((xnew-x(1))/dx)+1;
%         xnew=(inew-1)*dx;
%         dxnew=mean(diff(xnew));
%         ntraces=length(xnew);
%         for k=1:ntraces
%             s=snapshot(:,inew(k));%trace
%             ss=2*s*dxnew/amax;
%             ind=find(abs(ss)>dx*wigglemax);
%             if(~isempty(ind))
%                 ss(ind)=sign(ss(ind))*dx*wigglemax;
%             end
%             if(wtflag==2)
%                 line(ss+xnew(k),y,'color',wtcolor)
%             else
%                 wtva(ss+xnew(k),y,wtcolor,xnew(k),1,1,1);
%             end
%         end
%     end
% end
if(strcmp(vis,'on'))
    set(hax,'visible','off');
else
    set(hax,'visible','on');
end
visaxes(hax);