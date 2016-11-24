function seiscompare(seis1,seis2,t,x,title1,title2,attrflag)
% SEISCOMPARE: compare two seismic sections (same geometry)
%
% seiscompare(seis1,seis2,t,x,title1,title2,attrflag)
%
% seis1 ... firstseismic section
% seis2 ... second seismic section
% t ... time coordinate for seis1 and seis2
% x ... space coordinate for seis1 and seis2
% title1 ... title for seis1 (should be as descriptive as possible)
% title2 ... title for seis2 (should be as descriptive as possible)
% attrflag ... produce attribute axes showing cross correlation, lag, phase
%       rotation, amplitude scalar, and dominant frequency. 1 meand produce
%       the axes, 0 means don't do it.
% ********* default =0 *********** Turning this on can make the tool very slow
% 
%
%
% G.F. Margrave, CREWES Project, August 2016
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

if(ischar(seis1))
    action=seis1;
else
    action='init';
end

if(strcmp(action,'init'))
    
    if(nargin<7)
        attrflag=0;
    end
    
    figure
    tmax=max(t);

    if(attrflag)
        htnom=.35;%height of seismic
        hta=.09;%ht of attribute axes
    else
        htnom=.39;%height of seismic
        hta=0;%ht of attribute axes
    end
    widthnom=.8;%width of seismic
    sepa=.05;%separation between attribute and seismic axes
    x0nom=.05;%nominal horizontal starting position
    y0nom=.1;%nominal vertical starting position
    horsep=.04;%horizontal separtion between seismic and most controls
    
    %define clip levels
    cliplevels={'64','32','24','16','12','8','4','2','1','.5','.25','.125','.05','.01','.001'};
    icliplvl=7;%index of default clip level
    clip=str2double(cliplevels{icliplvl});
    
    hax1=subplot('position',[x0nom,y0nom+htnom+1.1*hta+sepa,widthnom,htnom]);
    A1=mean(seis1(:));
    sigma1=std(seis1(:));
    hi1=imagesc(x,t,seis1,[A1-clip*sigma1 A1+clip*sigma1]);
    set(hax1,'tag','seis1','userdata',hi1)
    if(tmax<20)
        ylabel('time (sec)')
    else
        ylabel('depth (m)')
    end
    set(hax1,'xticklabel',[])
    title(['seis1: ' title1])
    
    %amplitude control radio buttons
    bgwidth=.12;bght=.02;
    hbg1=uibuttongroup(gcf,'position',[x0nom+widthnom-bgwidth,y0nom+2*htnom+1.1*hta+sepa,...
        bgwidth,bght],'selectionchangedfcn','seiscompare(''rescale'')','tag','bg1');
    hr1=uicontrol(hbg1,'style','radiobutton','units','normalized','position',[0,0,1/4,1],...
        'string','Master','tag','m1');
    hr2=uicontrol(hbg1,'style','radiobutton','units','normalized','position',[5/16,0,1/4,1],...
        'string','Slave','tag','s1');
    hr3=uicontrol(hbg1,'style','radiobutton','units','normalized','position',[5/8,0,3/8,1],...
        'string','Independent','tag','i1');
    set(hbg1,'userdata',[hr1 hr2 hr3])
    %clip level control
    cwidth=.03;cht=.02;sep=.005;
    hclip=uicontrol(gcf,'style','popupmenu','units','normalized',...
        'position',[x0nom+widthnom-bgwidth-cwidth,y0nom+2*htnom+1.1*hta+sepa+sep,...
        cwidth,cht],'string',cliplevels,'callback','seiscompare(''rescale'')',...
        'tag','clip','value',icliplvl);
    uicontrol(gcf,'style','text','units','normalized',...
        'position',[x0nom+widthnom-bgwidth-cwidth,y0nom+2*htnom+1.1*hta+sepa++sep+cht,...
        cwidth,cht],'string','Clip level')
    
    
    if(attrflag)
        %make 4 attribute axes in the same spot, only 1 visible at a time
        attributes={'ccmax','cclag','phase','scalar','domfreq'};
        hattr=zeros(size(attributes));
        ynot=y0nom+htnom+sepa;
        for k=1:length(attributes)
            if(k==1)
                vis='on';
            else
                vis='off';
            end
            hattr(k)=axes('position',[x0nom,ynot,widthnom,hta],'visible',vis,'tag',attributes{k});
        end
        width=.05;ht=.03;
        uicontrol(gcf,'style','popupmenu','string',...
            attributes,'units','normalized',...
            'position',[x0nom+widthnom+horsep,ynot+.3*hta,width,ht],...
            'callback','seiscompare(''switchattr'')','value',1,'tooltipstring',...
            'Cross correlation mximum','userdata',hattr,'tag','attrpopup');
    end
    
    hax2=subplot('position',[x0nom,y0nom,widthnom,htnom]);
    A2=mean(seis2(:));%measure and save for later
    sigma2=std(seis2(:));
    hi2=imagesc(x,t,seis2,[A1-clip*sigma1 A1+clip*sigma1]);%initially seis2 is a slave
    set(hax2,'tag','seis2','userdata',hi2)
    if(tmax<20)
        ylabel('time (sec)')
    else
        ylabel('depth (m)')
    end
    title(['seis2: ' title2])
    xlabel('distance (m)')
    hbg2=uibuttongroup(gcf,'position',[x0nom+widthnom-bgwidth,y0nom+htnom,...
        bgwidth,bght],'selectionchangedfcn','seiscompare(''rescale'')','tag','bg2');
    hr1=uicontrol(hbg2,'style','radiobutton','units','normalized','position',[0,0,1/4,1],...
        'string','Master','tag','m2');
    hr2=uicontrol(hbg2,'style','radiobutton','units','normalized','position',[5/16,0,1/4,1],...
        'string','Slave','tag','s2');
    hr3=uicontrol(hbg2,'style','radiobutton','units','normalized','position',[5/8,0,3/8,1],...
        'string','Independent','tag','i2');
    set(hbg2,'userdata',[hr1 hr2 hr3])
    set(hr2,'value',1)
    
    set(hclip,'userdata',[A1 sigma1 A2 sigma2]);
    
    colormap(seisclrs)
    bigfig
    whitefig
    bigfont(gcf,1.5,1);
    
   %the equalize zoom stuff 
   xnow=x0nom+widthnom+horsep; ynow=y0nom+2*htnom+sepa+.5*hta;
   width=.09;height=.04;
   uicontrol(gcf,'style','text','string',...
       'Zoom the upper plot and then click below to equalize all plots',...
       'units','normalized','position',[xnow,ynow,width,height])
   ynow=ynow-height;
   uicontrol(gcf,'style','pushbutton','string','equalize axes','units',...
       'normalized','position',[xnow,ynow,width,height],...
       'callback','seiscompare(''equalize'')');
   
   %define zone of interest
   sepy=.01;
   ynow=ynow-height-2*sepy;
   xnow=x0nom+widthnom+horsep;
   width=.09;height=.02;
   uicontrol(gcf,'style','text','string',...
       'Time Zone of Interest (ZOI)',...
       'units','normalized','position',[xnow,ynow,width,height])
   ynow=ynow-height-.5*sepy;
   width=.02;height=.02;
   uicontrol(gcf,'style','text','string',...
       'Start',...
       'units','normalized','position',[xnow,ynow,width,height])
   xnow=xnow+width;
   width=.02;
   
   times=t(1):.1:t(end);
   if(length(times)<10)
       times=t(1):.01:t(end);
   end
   tvals=cell(size(times));
   for k=1:length(times)
       tvals{k}=num2str(times(k));
   end
   
   uicontrol(gcf,'style','popupmenu','string',...
       tvals,'units','normalized','position',[xnow,ynow,2*width,height],...
       'value',1,'tag','start')
   %ynow=ynow-height-.5*sepy;
   %xnow=x0nom+widthnom+horsep;
   xnow=x0nom+widthnom+horsep;
   ynow=ynow-height-sepy;
   width=.02;height=.02;
   uicontrol(gcf,'style','text','string',...
       'End',...
       'units','normalized','position',[xnow,ynow,width,height])
   xnow=xnow+width;
   width=.02;
   uicontrol(gcf,'style','popupmenu','string',...
       tvals,'units','normalized','position',[xnow,ynow,2*width,height],...
       'value',length(tvals),'tag','end')
   
   %space zone of interest
   ynow=ynow-height-sepy;
   xnow=x0nom+widthnom+horsep;
   width=.09;height=.02;
   uicontrol(gcf,'style','text','string',...
       'Space Zone of Interest (ZOI)',...
       'units','normalized','position',[xnow,ynow,width,height])
   ynow=ynow-height-.5*sepy;
   width=.02;height=.02;
   uicontrol(gcf,'style','text','string',...
       'Start',...
       'units','normalized','position',[xnow,ynow,width,height])
   xnow=xnow+width;
   width=.02;
   
   xs=sigfig(linspace(x(1),x(end),20),2);

   xvals=cell(size(xs));
   for k=1:length(xs)
       xvals{k}=num2str(xs(k));
   end
   
   uicontrol(gcf,'style','popupmenu','string',...
       xvals,'units','normalized','position',[xnow,ynow,2*width,height],...
       'value',1,'tag','startx')
   %ynow=ynow-height-.5*sepy;
   %xnow=x0nom+widthnom+horsep;
   xnow=x0nom+widthnom+horsep;
   ynow=ynow-height-sepy;
   width=.02;height=.02;
   uicontrol(gcf,'style','text','string',...
       'End',...
       'units','normalized','position',[xnow,ynow,width,height])
   xnow=xnow+width;
   width=.02;
   uicontrol(gcf,'style','popupmenu','string',...
       xvals,'units','normalized','position',[xnow,ynow,2*width,height],...
       'value',length(xvals),'tag','endx')
   
   width=.09;height=.04;
   ynow=ynow-height-.5*sepy;
   xnow=x0nom+widthnom+horsep;
   uicontrol(gcf,'style','pushbutton','string','Zoom to ZOI','units',...
       'normalized','position',[xnow,ynow,width,height],...
       'callback','seiscompare(''ZOI'')');
   
   %clipping controls for each section NOT DONE
   %minisep=.005;
   %xnow=x0nom+widthnom+
   
   
   set(gcf,'name',['compare ' title1 ' & ' title2]);
    
   seiscompare('recompute')
   
elseif(strcmp(action,'switchattr'))
    hpop=findobj(gcf,'tag','attrpopup');
    hattr=get(hpop,'userdata');
    choice=get(hpop,'value');
    for k=1:length(hattr)
        if(k==choice)
            set(hattr(k),'visible','on')
            hl=get(hattr(k),'children');
            set(hl,'visible','on');
            switch k
                case 1
                    set(gco,'tooltipstring','Cross correlation mximum');
                case 2
                    set(gco,'tooltipstring','Cross correlation lag (sec)');
                case 3
                    set(gco,'tooltipstring','Relative phase (deg)');
                case 4
                    set(gco,'tooltipstring','Log relative amplitude scalar (log10)');
                case 5
                    set(gco,'tooltipstring','Dominant frequency (Hz)');
            end
        else
            set(hattr(k),'visible','off')
            hl=get(hattr(k),'children');
            set(hl,'visible','off');
        end
        
    end
elseif(strcmp(action,'equalize'))
    haxes=findobj(gcf,'type','axes');
    hseis1=findobj(gcf,'tag','seis1');
    xl=get(hseis1,'xlim');
    yl=get(hseis1,'ylim');
    for k=1:length(haxes)
        if(haxes(k)~=hseis1)
            set(haxes(k),'xlim',xl)
            tag=get(haxes(k),'tag');
            if(strcmp(tag,'seis2'))
                set(haxes(k),'ylim',yl);
            end
        end
    end
    
    seiscompare('recompute');
    
elseif(strcmp(action,'recompute'))
%     hpop=findobj(gcf,'tag','attrpopup');
%     choice=get(hpop,'value');
%     attrnames=get(hpop,'string');
    haxes=findobj(gcf,'type','axes');
    if(length(haxes)==2)%means no attributes
        return;
    end
    hseis1=findobj(gcf,'tag','seis1');
    hseis2=findobj(gcf,'tag','seis2');
    %get the time zone
    yl=get(hseis1,'ylim');
    hi1=findobj(hseis1,'type','image');
    hi2=findobj(hseis2,'type','image');
    seis1=get(hi1,'cdata');
    seis2=get(hi2,'cdata');
    t=get(hi1,'ydata');
    x=get(hi1,'xdata');
    cc1=zeros(size(x));
    cc2=cc1;
    phase=cc1;
    scalar=cc1;
    domfreq1=cc1;
    domfreq2=cc2;
    small=.00000000000001;
    dt=t(2)-t(1);
    izone=near(t,yl(1),yl(2));
    mw=mwindow(length(izone),10);
    for k=1:length(x)
        s1=seis1(izone,k).*mw;s2=seis2(izone,k).*mw;
        test1=sum(abs(s1));test2=sum(abs(s2));
        if(test1*test2>small)
            cc=maxcorr(s1,s2,100);
            cc1(k)=cc(1);
            cc2(k)=dt*cc(2);
            % done in this way, s2 will need stat(s2,t,cc2(k)) to
            % align with s1.
            phase(k)=constphase(s2,s1);
            % so s2 will need phsrot(s2,phase(k))
            scalar(k)=norm(s1)/norm(s2);
            %so s2 will need s2*scalar(k)
            domfreq1(k)=domfreq(s1,t(izone));
            domfreq2(k)=domfreq(s2,t(izone));
        end
    end
    for k=1:length(haxes)
        tag=get(haxes(k),'tag');
        if(strcmp(tag,'ccmax'))
            axes(haxes(k));
            hccmax=plot(x,cc1,'r');
            set(hccmax,'visible','off');
            set(haxes(k),'xticklabel',[],'yaxislocation','right','ygrid','on')
        elseif(strcmp(tag,'cclag'))
            axes(haxes(k));
            hcclag=plot(x,cc2,'r');
            set(hcclag,'visible','off');
            set(haxes(k),'xticklabel',[],'yaxislocation','right','ygrid','on')
        elseif(strcmp(tag,'phase'))
            axes(haxes(k));
            hphase=plot(x,phase,'r');
            set(hphase,'visible','off');
            set(haxes(k),'xticklabel',[],'yaxislocation','right','ygrid','on')
        elseif(strcmp(tag,'scalar'))
            axes(haxes(k));
            hscalar=plot(x,log10(scalar),'r');
            set(hscalar,'visible','off');
            set(haxes(k),'xticklabel',[],'yaxislocation','right','ygrid','on')
        elseif(strcmp(tag,'domfreq'))
            axes(haxes(k));
            fnyq=.5/(t(2)-t(1));
            hdom=plot(x,domfreq1,'r',x,domfreq2,'b');
            ylim([0 .5*fnyq])
            legend(hdom,'seis1','seis2')
            set(hdom,'visible','off');
            set(haxes(k),'xticklabel',[],'yaxislocation','right','ygrid','on')
        end
        
    end
    
    seiscompare('switchattr');
elseif(strcmp(action,'ZOI'))
    hstart=findobj(gcf,'tag','start');
    hend=findobj(gcf,'tag','end');
    hstartx=findobj(gcf,'tag','startx');
    hendx=findobj(gcf,'tag','endx');
    hseis1=findobj(gcf,'tag','seis1');
    %hi1=findobj(hseis1,'type','image');

    start_times=get(hstart,'string');
    istart=get(hstart,'value');
    t1=str2double(start_times{istart});
    end_times=get(hend,'string');
    iend=get(hend,'value');
    t2=str2double(end_times{iend});
    if(t2<=t1)
        msgbox('ZOI end time must be greater than start time');
        return;
    end
    
    start_xs=get(hstartx,'string');
    istart=get(hstartx,'value');
    x1=str2double(start_xs{istart});
    end_xs=get(hendx,'string');
    iend=get(hendx,'value');
    x2=str2double(end_xs{iend});
    if(x2<=x1)
        msgbox('ZOI end x must be greater than start x');
        return;
    end
    
    set(hseis1,'ylim',[t1 t2],'xlim',[x1 x2]);
    
    seiscompare('equalize')
    
elseif(strcmp(action,'rescale'))
    hbg1=findobj(gcf,'tag','bg1');
    hbg2=findobj(gcf,'tag','bg2');
    hclip=findobj(gcf,'tag','clip');
    ampdata=get(hclip,'userdata');%means and sigmas of the seismics
%     hr1=get(hbg1,'children');
%     hr2=get(hbg1,'children');
    %determine if call was from clip or a radio button
    callid=get(gco,'tag');
    if(~strcmp(callid,'clip'))
        %ok, a radio button was clicked
        hbg=get(gco,'parent');%this is the button group clicked
        if(hbg==hbg1);
            hbga=hbg2;%alternate button group
        else
            hbga=hbg1;
        end
        hr=flipud(get(hbg,'children'));
        hra=flipud(get(hbga,'children'));
        %determine master, slave , or independent and make consistent
        %setting
        for k=1:3
            val=get(hr(k),'value');
            if(val==1)
                if(k==1)%means clicked group is master
                    set(hra(2),'value',1);%force the alternate to slave
                elseif(k==2)%means clicked group is slave
                    set(hra(1),'value',1);%force the alternate to master
                else %clicked group is independent
                    set(hra(3),'value',1)%force the alternate to independent
                end
                if(hbg==hbg1)
                    %aflag is the amplitude flag for seis1. The flag for
                    %seis2 is determined by that for seis1. If 1 is master
                    %then 2 must be slave and vice versa. If 1 is
                    %independent then 2 must be also
                    aflag=k;
                else
                    if(k==1)
                        aflag=2;
                    elseif(k==2)
                        aflag=1;
                    else
                        aflag=k;
                    end
                end
                        
                    
            end
        end
    else
        %determine amp flag
        hr=flipud(get(hbg1,'children'));
        for k=1:3
            val=get(hr(k),'value');
            if(val==1)
                aflag=k;
            end
        end
    end
    %determine the clip value
    cliplevels=get(hclip,'string');
    iclip=get(hclip,'value');
    clip=str2double(cliplevels{iclip});

    %get the seismic
    hseis1=findobj(gcf,'tag','seis1');
    hi1=findobj(hseis1,'type','image');
    seis1=get(hi1,'cdata');
    x1=get(hi1,'xdata');
    y1=get(hi1,'ydata');
    hseis2=findobj(gcf,'tag','seis2');
    hi2=findobj(hseis2,'type','image');
    seis2=get(hi2,'cdata');
    x2=get(hi2,'xdata');
    y2=get(hi2,'ydata');
    
    %determine A and sigma based on aflag
    if(aflag==1)
        A1=ampdata(1);
        sigma1=ampdata(2);
        A2=A1;
        sigma2=sigma1;
    elseif(aflag==2)
        A1=ampdata(3);
        sigma1=ampdata(4);
        A2=A1;
        sigma2=sigma1;
    else
        A1=ampdata(1);
        sigma1=ampdata(2);
        A2=ampdata(3);
        sigma2=ampdata(4);
    end
    
    %now refresh the plots
    axes(hseis1)
    tit=get(get(gca,'title'),'string');
    xl=get(gca,'xlim');
    yl=get(gca,'ylim');
    imagesc(x1,y1,seis1,[A1-clip*sigma1, A1+clip*sigma1])
    title(tit)
    xlim(xl);ylim(yl);
    set(gca,'tag','seis1','xticklabel',[]);
    
    axes(hseis2)
    tit=get(get(gca,'title'),'string');
    xl=get(gca,'xlim');
    yl=get(gca,'ylim');
    imagesc(x2,y2,seis2,[A2-clip*sigma2, A2+clip*sigma2])
    title(tit)
    xlim(xl);ylim(yl);
    set(gca,'tag','seis2');
    
    bigfont(gcf,1.5,1)
end
