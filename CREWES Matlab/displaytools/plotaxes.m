function ha=plotaxes(arg1,arg2)
%PLOTAXESS ... plot a set of similar axes for easy conparison
%
% plotaxes(naxes)
%
% Creates a set of full-sized axes stacked on top of one another with only
% the topmost axes visible at any time. User interface controls are
% provided to step through the axes and to toggle back and forth between
% axes of interest, copy to the clipboard (for pasting into Word or
% PowerPoint) and for mavin a movie (AVI file). Plotaxes is called once to
% initialize it for a given number of axes. It returns the vector of axes
% handles as ha. Then to plot in each axes, it is called with the syntax
% plotaxes('currentaxes',ha(k)) in a loop over the number of axes where k
% is the loop index. To see this in action, run the example below.
% 
% naxes ... number of axes desired
%
% ha ... vector of length naxes of handles of the axes.
%
% Example of use
% show a series of vibroseis sweeps of progressivly higher fmax
% tmax=3;%sweep length
% fmin=2;
% fmaxs=5:5:200;
% 
% ha=plotaxes(length(fmaxs));
% 
% for k=1:length(fmaxs)
%     plotaxes('currentaxes',ha(k));
%     [s,ts]=sweep(fmin,fmaxs(k),.001,tmax);
%     plot(ts,s)
%     title(['Sweep from ' int2str(fmin) ' to ' int2str(fmaxs(k)) ' Hz']);
%     xlabel('time (sec)')
% end
% boldlines;
% bigfont;
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
if(ischar(arg1))
    action=arg1;
else
    action='init';
    naxes=arg1;
end

if(strcmp(action,'init'))
    figure;
    %create the axes
    pos=[0.250    0.1100    0.7250    0.750];
    ha=gobjects(1,naxes);
    titles=cell(size(ha));
    for k=1:naxes
        ha(k)=axes('position',pos);
        xlabel(['Axes ', num2str(k)])
        titles{k}=['Axes number ' num2str(k)];
        visaxes(ha(k));%turn axes off
    end
    axlims=[];
    visaxes(ha(1));%turn on first axes
%     hpanel=buttonarray(gcf,[.01 .87 .2 .08],naxes,ones(1,naxes));
    %user data of hpanel is an 3-by-naxes matrix with the first row giving
    %the values of the radio buttons, the second row giving the handles of
    %the buttons, and the third row giving the handles of the text labels
    xnow=.01;ynow=.85;width=.15;height=.1;
    huipan1=uipanel('position',[xnow ynow width height]);
    xnow=.01;ynow=.4;width=.98;height=.58;
    hchlbl=uicontrol(huipan1,'style','text','string','Chose axes',...
            'value',1,'units','normalized','position',...
            [xnow,ynow,width,height],'userdata',axlims);
    xnow=.01;ynow=0;width=.98;height=.35;
    hchoose=uicontrol(huipan1,'style','popupmenu','string',titles,...
            'value',1,'units','normalized','position',[xnow,ynow,width,height],...
            'callback','plotaxes(''choose'');',...
            'tooltipstring','Select gather','userdata',[1 ha]);
    %user data of hchoose is the index of the displayed gather followed by
    %the vector of all axes handles
    
    %make a uipanel to put the slider and its increment control in
    xnow=.01; ynow=.77;width=.15;height=.07;
    huipan2=uipanel('position',[xnow ynow width height]);
%     xnow=.01;ynow=ynow-height-.005;width=.15;height=.05;
    s1=1/(naxes-1);s2=2*s1;%slider steps
    hslider=uicontrol(huipan2,'style','slider','min',1,'max',naxes,'value',1,...
        'units','normalized','position',[.01,.6,.98,.35],...
        'callback','plotaxes(''slide'');','sliderstep',sort([s1 s2]),...
        'tooltipstring','step through axes','userdata',1);
    %userdata of hslider is the previous position of the slider. This
    %allows implementing slider wraparopund.
    %the above does not work because the Matlab slider does not execute the
    %callback when the slider is asked to move beyond the maximum or
    %minimum. So forget you ever read this comment. 
%     xnow=.01+width;ynow=ynow-height;width=.03;height=.05;
    hinc=uicontrol(huipan2,'style','popupmenu','value',1,...
        'string',{'1';'2';'3';'4';'5';'6'},...
        'units','normalized','position',[.01,.1,.22,.4],...
        'callback','plotaxes(''step'');','userdata',[s1 s2],...
        'tooltipstring','set slider step');
    hinclbl=uicontrol(huipan2,'style','text','string','Slider increment',...
        'units','normalized','position',[.24 .03 .7 ,.4],...
        'horizontalalignment','left');
        
    
    xnow=.01;ynow=.67;width=.04;height=.05;
    hmark=uicontrol('style','pushbutton','string','Mark','Units','Normalized',...
        'position',[xnow ynow width height],'callback','plotaxes(''mark'');',...
        'tag','mark','userdata',zeros(1,naxes),'tooltipstring','mark for toggling');
    xnow=xnow+width+.01;width=.04;height=.05;
    hunmark=uicontrol('style','pushbutton','string','unMark','Units','Normalized',...
        'position',[xnow ynow width height],'callback','plotaxes(''unmark'');',...
        'tag','unmark','userdata',hmark,'tooltipstring','unmark for toggling');
    xnow=xnow+width+.01;width=.05;height=.05;
    hunmarkall=uicontrol('style','pushbutton','string','unMarkAll','Units','Normalized',...
        'position',[xnow ynow width height],'callback','plotaxes(''unmark'');',...
        'tag','unmarkall','userdata',hmark,'tooltipstring','disable toggling');
    xnow=.01;ynow=ynow-height;width=.15;height=.05;
    htoggle=uicontrol('style','pushbutton','string','Toggle bewteen marked axes','Units','Normalized',...
        'position',[xnow ynow width height],'callback','plotaxes(''toggle'');',...
        'tag','toggle','visible','on','tooltipstring','cycle through marked axes',...
        'enable','off');
    
    %make a copy button
    if(ispc)
        buttit='Copy current image to clipboard';
        callback='plotaxes(''clipboard'');';
    else
        buttit='Write current image to TIFF file';
        callback='plotaxes(''clipboard'');';
    end
    xnow=.01;width=.15;height=.05;ynow=ynow-.06;
    hcopybutton=uicontrol('style','pushbutton','string',buttit,'units','normalized',...
        'position',[xnow,ynow,width,height],'callback',callback);
    
    %make movie button and message
    xnow=.01;width=.15;height=.05;ynow=ynow-.07;
    hmoviebutton=uicontrol('style','pushbutton','string','Make movie','units','normalized',...
        'position',[xnow,ynow,width,height],'callback','plotaxes(''makemovie'');');
    msg{1}='Display starting axes for movie.';
    msg{2}='Slider increment is frame step.';
    xnow=.01;width=.15;height=.05;ynow=ynow-.06;
    uicontrol('style','text','string',msg,'Units','Normalized',...
        'position',[xnow ynow width height]);
    
    handles={hchlbl hchoose nan nan hslider nan hmark hunmark ...
        hunmarkall htoggle nan nan hinc hinclbl nan nan ...
        hcopybutton hmoviebutton};
    set(gcf,'userdata',{handles;nan;nan;nan;nan;titles;ha;});
    %enlarge to 1/4 screen size
    screen=get(0,'screensize');
    f1=1/4;f2=1/2;
    position=[f1*screen(3) f1*screen(4) f2*screen(3) f2*screen(4)];
    if(~isempty(BIGFIG_X))
        bigfig;
    else
        set(gcf,'Units','Pixels','position',position);
    end
%     bigfont;
    whitefig;
%     simplezoom(1,'plotaxes(''zoom'');');
elseif(strcmp(action,'currentaxes'))
    hnew=arg2;%we want to make this axis current
    vis=get(hnew,'visible');    
    if(strcmp(vis,'off'))
        visaxes(hnew);
    end
    set(gcf,'currentaxes',hnew);
    
    figdat=get(gcf,'userdata');
    handles=figdat{1};
    ha=figdat{7};%the axes handles
    inew=find(ha==hnew);
    if(isempty(inew))
        error('invalid axes handle')
    end
    hchoose=handles{2};
    %ichoice=get(hchoose,'value');
    set(hchoose,'value',inew);
    plotaxes('choose');
%     udat=get(hchoose,'userdata');
%     icurrent=udat(1);
%     hachoice=udat(ichoice+1);
%     hacurrent=udat(icurrent+1);
%     visaxes(hacurrent);
%     visaxes(hachoice);
%     udat(1)=ichoice;
%     set(hchoose,'userdata',udat);
%     set(hslider,'value',ichoice);
    
elseif(strcmp(action,'choose'))
    figdat=get(gcf,'userdata');
    handles=figdat{1};
    hchoose=handles{2};
    hslider=handles{5};
    ichoice=get(hchoose,'value');
    udat=get(hchoose,'userdata');
    icurrent=udat(1);
    hachoice=udat(ichoice+1);
    hacurrent=udat(icurrent+1);
    visaxes(hacurrent);
    visaxes(hachoice);
    udat(1)=ichoice;
    set(hchoose,'userdata',udat);
    set(hslider,'value',ichoice);

elseif(strcmp(action,'slide'))
    figdat=get(gcf,'userdata');
    handles=figdat{1};
    hchoose=handles{2};
    hslider=handles{5};
    newchoice=round(get(hslider,'value'));
    prevchoice=get(hslider,'userdata');
    if(newchoice==prevchoice)
        %indicates slider wraparound; however, this does not work
        naxes=get(hslider,'max');
        if(newchoice==1)
            newchoice=naxes;
        else
            newchoice=naxes;
        end
    end
    set(hchoose,'value',newchoice);
    set(hslider,'userdata',newchoice);
    plotaxes('choose'); 
elseif(strcmp(action,'step'))
    figdat=get(gcf,'userdata');
    handles=figdat{1};
    hslider=handles{5};
    hinc=handles{13};
    factor=get(hinc,'value');
    step=get(hinc,'userdata');
    set(hslider,'sliderstep',sort([factor*step(1) factor*step(2)]));   

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
    imarked=find(chosen==1);%these are the axes that we toggle between, 
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
    icurrent=udat(1);%this is the presently displayed gather
    hachoice=udat(ichoice+1);%the axes of the gather to be displayed
    hacurrent=udat(icurrent+1);%the axes of the current gather
    visaxes(hacurrent);%toggle the current gather off
    visaxes(hachoice);%toggle the chose gather on
    udat(1)=ichoice;%update the user data
    set(hchoose,'userdata',udat);
    set(hchoose,'value',ichoice);
    set(hslider,'value',ichoice);%set the slider
        
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
    if(diff(xl)==1)
        xl=xlnom;
    end
    if(diff(ylnom)<diff(yl))
        yl=ylnom;
        
    end
    if(diff(yl)==1)
        yl=ylnom;
    end
    for k=1:length(haxes)
        set(haxes(k),'xlim',xl,'ylim',yl);
    end
elseif(strcmp(action,'clipboard'))
    hideui; %hide any user interface controls
    %expand the current axis
    ud=get(gcf,'userdata');
    handles=ud{1};
    haxes=ud{7};
    hchoose=handles{2};
    iaxis=get(hchoose,'value');
    pos=get(haxes(iaxis),'position');
%     set(haxes(iaxis),'position',[.11, pos(2) .85 pos(4)])    
%     legend off
    if(ispc)
        print -dbitmap %copy the figure to the clipboard (windows only)
    else
        print -dtiff
    end
    set(haxes(iaxis),'position',pos);
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
    icurrent=udat(1);%this is the index of the starting axes
    step=get(hinc,'value');%this is the slider increment
    
    %put up confirmation dialog
    q=char('Starting axes:','Ending axes:','Axes increment:','Frame rate (frames/sec):','Output file name:');
    a1=cell2char(get(hchoose,'string'));
    a2=cell2char(get(hinc,'string'));
    a3='1';
    a=char(a1,a1,a2,a3,fullfilename);
    flags=[icurrent length(ha) step 1 1];
    askthingsinit('plotaxes(''makemovie2'');',q,a,flags,'Please verify this Movie information');
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
        telluserinit('Movie length is only one frame, movie aborted')
        return
    end
    
    if(icurrent>iend); step=-step; end
    
    video_obj=VideoWriter(fullfilename);
    video_obj.FrameRate=framerate;%movie will play at 1 second per frame by default
    open(video_obj)
    
    kframes=icurrent:step:iend;%these are the axes that will play in the movie
    nframes=length(kframes);
    hideui; %hide any user interface controls    
    for k=1:nframes;
        %display the next axes
        set(hchoose,'value',kframes(k));
        set(hslider,'userdata',kframes(k));
        plotaxes('choose');
        %expand the current axis to fill the figure
        handles=figdat{1};
        haxes=figdat{7};
        iaxis=get(hchoose,'value');
        pos=get(haxes(iaxis),'position');
        set(haxes(iaxis),'position',[.11, pos(2) .85 pos(4)])
        %now take the frame
        frame=getframe(gcf);
        writeVideo(video_obj,frame);
        %restore the axes to the original size
        set(haxes(iaxis),'position',pos);
    end
    %return to starting frame
    set(hchoose,'value',kframes(1));
    set(hslider,'userdata',kframes(1));
    plotaxes('choose');
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