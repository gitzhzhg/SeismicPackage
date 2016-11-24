function theodisplay(theo,t,rr,w,tw,phs,hhors,thors,tscale,xscale,...
    title,plotmode,nreps,tint,ievery,kolor,lasheader)

% theodisplay(theo,t,rr,wavelet,tw,phs,hhors,thors,tscale,xscale,title,
%             plotmode,nreps,tint,ievery,kolor,lasheader)
%
% THEODISPLAY is called by LOGSEC to create a display of a theogram showing
% the wavelet and reflectivity (in time) that it was compute from, the
% theogram, and an arbitrary number of phase rotated copies of the theogram.
% Also shown are any named horizons which intersect the
% theogram. THEODISPLAY then controls the figure and provides facilities for
% zooming, grid, more phase rotations, altering the title, and hardcopy.
% Once launched, the figure has no further communication with LOGSEC.
%
%	theo = a column vector containing the theogram
%	t= column vector of times for theo and r
%	rr = a 2 column matrix. First column is a vector of the reflectivity samples
%           while the secone is the times for the reflectivities
%	w = a column vector with the wavelet for theo.
%			(theo was computed by convolution of r and w)
%	tw = column vector of times for w
%	phs = vector of phase angles (in degrees). A phase rotated copy of theo
%		will be generated for each phs
%	hhors = vector of handles of the horizons which intersect theo. At the
%		time of the creation of this display, these horizons must be
%		drawn in an active figure.
%	thors = vector of times at which the above horizons intersect theo
%	tscale = default time scale (seconds per inch) for hardcopy
%	xscale = default horizontal scale (traces per inch) for hardcopy
%	title = title
%   plotmode = flag ... 1-> wiggle trace
%                       2-> wiggle trace variable area
%   nreps =  number of times to repeat the plot of each phase rotation
%   **** default=1 ****
%   tint = interval between timing lines.
%          Must be one of: .5 .25 .1 .05 .025 .01
%          **** default = .1 ****
%  ievery = timing line labeling frequency. The first timing line will be
%           labeled and every ievery'th line after that.
%           Must be one of: 1 2 4 5 10
%           **** default =1 ****
%  kolor = rgb triplet defining color of the theogram traces
%          **** default = [0 0 1] (blue) ****
%  lasheader =  string matrix containing an LAS header appropriate for the
%               theogram. If provided then the 'writelas' menu will be
%               active and allow the theogram, impedance, rcs, and wavelet
%               to be written as an LAS log in time.
%               (Log mnemonic matrix in MAKELASHEADER should be:
%		['THEO';'IMPD';'RCS ';'WLET']
%		NULLS value should be -999.25, and units should be 'S')
%               **** default = '' (no write capability) ****
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


if( ~ischar(theo) )
    action='init';
else
    action=theo;
end

if( strcmp(action,'init') )
    if(nargin<17)
        lasheader='';
    end
    if(nargin<16)
        kolor=[0 0 1];
    end
    
    if(nargin < 15)
        ievery=1;
    end
    
    if(nargin<14)
        tint=.1;
    end
    
    if(nargin<13)
        nreps=1;
    end
    
    it=find( [1 2 4 5 10]==ievery, 1 );
    if(isempty(it))
        error('Invalid value for ''ievery'' ');
    end
    
    it=find( [.5 .25 .1 .05 .025 .01]==tint, 1);
    if(isempty(it))
        error('Invalid value for ''tint'' ');
    end
    
    % make a new figure
    hfig=figure('visible','off','menubar','none','numbertitle','off','name','Theogram display');
    scrsize=get(0,'screensize');
%     pos=get(hfig,'position');
    swd=scrsize(3);sht=scrsize(4);
    set(hfig,'units','pixels','position',[.1*swd .1*sht .4*swd .7*sht]);
    
    % make sure everything is a column vector
    theo=theo(:);
    t=t(:);
    w=w(:);
    tw=tw(:);
    hhors=hhors(:);
    thors=thors(:);
    
    r=rr(:,1);
    tr=rr(:,2);
    
    % make a menu
    hoptions=uimenu(gcf,'label','Options');
    
    % turn grid on and off... default to on
    hgrid=uimenu(hoptions,'label','Timing Lines');
    hgridon=uimenu(hgrid,'label','Show Timing Lines','checked','on','callback',...
        'theodisplay(''grid'')');
    htimeint=uimenu(hgrid,'label','Line Interval');
    if(tint==.5); chk='on';  else chk='off';  end
    htime500=uimenu(htimeint,'label','500 ms','callback','theodisplay(''timeint'')',...
        'userdata',.5,'checked',chk);
    
    if(tint==.25); chk='on';  else chk='off'; end
    htime250=uimenu(htimeint,'label','250 ms','callback','theodisplay(''timeint'')',...
        'userdata',.250,'checked',chk);
    
    if(tint==.1); chk='on';  else chk='off';  end
    htime100=uimenu(htimeint,'label','100 ms','callback','theodisplay(''timeint'')',...
        'userdata',.1,'checked',chk);
    
    if(tint==.05); chk='on'; else chk='off'; end
    htime50=uimenu(htimeint,'label','50 ms','callback','theodisplay(''timeint'')',...
        'userdata',.050,'checked',chk);
    
    if(tint==.025); chk='on'; else chk='off'; end
    htime25=uimenu(htimeint,'label','25 ms','callback','theodisplay(''timeint'')',...
        'userdata',.025,'checked',chk);
    
    if(tint==.01); chk='on';  else chk='off'; end
    htime10=uimenu(htimeint,'label','10 ms','callback','theodisplay(''timeint'')',...
        'userdata',.010,'checked',chk);
    set(htimeint,'userdata',{htime500 htime250 htime100 htime50 htime25 htime10 tint});
    
    htimelabel=uimenu(hgrid,'label','Line Labels');
    if(ievery==1); chk='on'; else chk='off'; end
    hlabel1=uimenu(htimelabel,'label','Every Line','callback',...
        'theodisplay(''label'')','userdata',1,'checked',chk);
    
    if(ievery==2); chk='on'; else chk='off'; end
    hlabel2=uimenu(htimelabel,'label','Every Other Line','callback',...
        'theodisplay(''label'')','userdata',2,'checked',chk);
    
    if(ievery==4); chk='on'; else chk='off'; end
    hlabel4=uimenu(htimelabel,'label','Every Fourth Line','callback',...
        'theodisplay(''label'')','userdata',4,'checked',chk);
    
    if(ievery==5); chk='on'; else chk='off'; end
    hlabel5=uimenu(htimelabel,'label','Every Fifth Line','callback',...
        'theodisplay(''label'')','userdata',5,'checked',chk);
    
    if(ievery==10); chk='on'; else chk='off'; end
    hlabel10=uimenu(htimelabel,'label','Every Tenth Line','callback',...
        'theodisplay(''label'')','userdata',10,'checked',chk);
    
    set(htimelabel,'userdata',{hlabel1 hlabel2 hlabel4 hlabel5 hlabel10 ievery});
    
    % a new phase rotation
    hphase=uimenu(hoptions,'label','New Phase','callback',...
        'theodisplay(''newphs'')');
    
    % a locate action
    hlocate=uimenu(hoptions,'label','Locate','callback','theodisplay(''locate'')');
    
    % hardcopy
    hhard=uimenu(hoptions,'label','Hardcopy','callback',...
        'theodisplay(''hardcopy'')');
    
    %writelas
    vis='on';
    if(strcmp(lasheader,'')); vis='off'; end
    hwritelas=uimenu(hoptions,'label','Write LAS','callback',...
        'theodisplay(''writelas'')','enable',vis);
    % quit
    hquit=uimenu(hoptions,'label','Close','callback','theodisplay(''quit'')');
    
    % a message panel
    
    hmsg=uicontrol('style','text','string','MB3 drag -> Zoom ... MB3 click -> unZoom',...
        'units','normalized','position',[0 0 1 .03]);
    
    % compute the phase rotated copies of theo
    theophs=zeros(length(theo),length(phs));
    for k=1:length(phs)
        theophs(:,k)=phsrot(theo,phs(k));
    end
    
    %make a storage bucket
    hstore1=uicontrol('style','text','visible','off');
    
    % store things
    
    set(gcf,'userdata',{hmsg,hoptions,hgrid,hgridon,htimeint,hphase,hhard,hquit,...
        htimelabel,hlocate,hwritelas,hstore1});
    
    %integrate the reflectivity to get impedance
    imp=integrate(r);
    
    % make the impedance zero mean
    imp=imp-mean(imp);
    
    %userdata assignments:
    % h=get(gcf,'userdata') =>[hmsg,hoptions,hgrid,hgridon,htimeint,hphase,hhard,hquit,
    %				htimelabel,hlocate,hwritelas]
    % hmsg = h(1) ... vectors of theogram, time, impedance, rcs:  [theo t imp r]
    % hoptions = h(2) ... [w tw] wavelet and wavelet times
    % hgrid = h(3) ... [hhors thors] vector of horizon handles in the master figure,
    %				and vector of horizon times
    % hgridon = h(4) ... [plotmode nreps abs(title)] plotmode flag, number of phase reps
    %				and title
    % htimeint = h(5) ... vector of submenu handles
    % hphase = h(6) ... vector of theogram phases
    % hhard = h(7) ... x and time scale for hardcopy
    % hquit = h(8) ... matrix of phase rotated theograms
    % htimelabel = h(9) ... vector of submenu handles
    % hlocate = h(10) ... color to plot the theograms
    % hwritelas = h(11) ... string matrix with las header
    % hstore1 = h(12) ... handles of the lines and labels of the tops:[ntops hlines hlbls]
    %
    %
    
    set(hmsg,'userdata',{theo t imp r tr});
    set(hoptions,'userdata',[w tw]);
    set(hgrid,'userdata',{hhors thors});
    set(hgridon,'userdata',[plotmode nreps abs(title)]);
    set(hphase,'userdata',phs);
    set(hhard,'userdata',[xscale tscale]);
    set(hquit,'userdata',theophs);
    set(hlocate,'userdata',kolor);
    set(hwritelas,'userdata',lasheader);
    
    %plot things
    
    theodisplay('plot');
    set(gca,'box','off');
    set(gca,'position',[.13 .11 .7 .8]);
    
    %turn on zooming with button 3
    simplezoom(3,'theodisplay(''repositops'')');
    
    set(hfig,'visible','on');
    
    return;
    
end

if(strcmp(action,'plot'))
    h=get(gcf,'userdata');
    hmsg=h{1};
    hoptions=h{2};
    hgrid=h{3};
    hgridon=h{4};
    htimeint=h{5};
    hphase=h{6};
%     hhard=h{7};
    hquit=h{8};
    htimelabel=h{9};
    hlocate=h{10};
    hstore1=h{12};
    
    dat=get(hgridon,'userdata');
    plotmode=dat(1);
    nreps=dat(2);
    title=char(dat(3:length(dat)));
    
    labelangle=45;
    
    % get the trace etc
    trcs=get(hmsg,'userdata');
    theo=trcs{1};
    t=trcs{2};
    imp=trcs{3};
    rc=trcs{4};
    tr=trcs{5};
    
    trcs=get(hoptions,'userdata');
    w=trcs(:,1);
    tw=trcs(:,2);
    
    trcs=get(hquit,'userdata');
    
    horstuff=get(hgrid,'userdata');
    if(~isempty(horstuff))
        hors=horstuff{1};
        thors=horstuff{2};
    else
        hors=[];
        thors=[];
    end
    
    phs=get(hphase,'userdata');
    
    %clear the axes
    cla;
    
    % plot the wavelet
    %first truncate the wavelet if it is longer than the theogram.
    if(length(w)>length(theo))
        ind=near(tw,0);
        i1=round(max([1 ind-length(theo)/2]));
        w=w(i1:i1+length(theo)-1);
        tw=tw(i1:i1+length(theo)-1);
    else
        w=pad_trace(w,theo,1);
        tw=t;
    end
    x=1;
    wm=max(abs(w));
    %make an appropriate time axis for w
    tw2=xcoord(t(1),tw(2)-tw(1),w);
    if(plotmode==1)
        plot(x+w/wm,tw2,'r');
    else
        wtva(x+w/wm,tw2,'r',x,1,1);
    end
    
    set(gca,'ydir','reverse');
    ytext=t(1)-.01*(t(length(t))-t(1));
    text('string','Wavelet','units','data','rotation',labelangle,'position',...
        [x ytext]);
    
    %plot the impedance
    bigger=2;
    x=x+2;
    im=max(abs(imp));
    line(x+bigger*imp/im,tr,'color','r');
    text('string','Impedance','units','data','rotation',labelangle,'position',...
        [x ytext]);
    
    
    %plot the rcs
    x=x+2;
    ilive= ~isnan(rc);
    rm=max(abs(rc(ilive)));
    %if( plotmode==1 )
    line(x+bigger*rc/rm,tr,'color','k');
    %else
    %wtva(x+rc(ilive)/rm,tr(ilive),'r',x,1,1);
    %end
    text('string','RCs','units','data','rotation',labelangle,'position',...
        [x ytext]);
    
    kolor=get(hlocate,'userdata');
    
    %plot the theogram
    thmax=max(abs(theo));
    x=x+1;
    ilabel=floor((nreps+1)/2);
    for k=1:nreps
        x=x+1;
        if( plotmode== 1)
            line(x+theo/thmax,t,'color',kolor);
        else
            wtva(x+theo/thmax,t,kolor,x,1,1);
        end
        if(k==ilabel)
            text('string','Theogram','units','data','rotation',labelangle,'position',...
                [x ytext]);
        end
    end
    
    %plot the phase rotations
    for k=1:length(phs)
        x=x+1;
        for kk=1:nreps
            x=x+1;
            if( plotmode== 1)
                line(x+trcs(:,k)/thmax,t,'color',kolor);
            else
                wtva(x+trcs(:,k)/thmax,t,kolor,x,1,1);
            end
            if(kk==ilabel)
                text('string',[num2str(phs(k)) ' Phs'],'units','data','rotation',labelangle,...
                    'position',[x ytext]);
            end
        end
    end
    
    %plot the horizons
    nhors=length(hors);
    hlines=nan*zeros(1,nhors);
    hlbls=nan*zeros(1,nhors);
    xlim=get(gca,'xlim');
    x=max(xlim);
    for k=1:nhors
        if(~isnan(thors(k)))
            %get the color
            %kol=get(hors(k),'color');
            kol='k';
            %get the name
            dat=get(hors(k),'userdata');
            it=strfind(dat,':');
            it2=strfind(dat,'depth');
            if(isempty(it2))
                it2=length(dat);
            else
                it2=it2-2;
            end
            name=dat(it+2:it2);
            if( ~strcmp(name(1:2),'__') )
                
                %plot
                hlines(k)=line(xlim,[thors(k) thors(k)],'color',kol,'linestyle','-.');
                hlbls(k)=text('string',name,'units','data',...
                    'position',[x thors(k)],'color',kol,'fontsize',9);
            end
        end
    end
    ind=find(isnan(hlines));
    if(~isempty(ind))
        hlines(ind)=[];
        hlbls(ind)=[];
        nhors=nhors-length(ind);
    end
    set(hstore1,'userdata',[nhors hlines hlbls]);
    
    %surpress x ticks
    set(gca,'xtick',[]);
    
    %determine the yticks
    %determine the tick interval
    dat=get(htimeint,'userdata');
    dt=dat{end};
    
    %determine the labeling interval
    dat=get(htimelabel,'userdata');
    ievery=dat{end};
    
    %first tick at the first even interval smaller than t(1)
    tnot=floor(t(1)/dt)*dt;
    if( abs((tnot-t(1)))>.2*dt )
        tnot=ceil(t(1)/dt)*dt;
    end
    
    ticks=tnot:dt:t(length(t));
    %generate tick labels
    lblmat=[];
    for k=1:length(ticks)
        if( rem(k-1,ievery)== 0)
            lblmat=strmat(lblmat,num2str(ticks(k)));
        else
            lblmat=strmat(lblmat,' ');
        end
    end
    ind=find(lblmat==1);
    if(~isempty(ind))
        lblmat(ind)=32;
    end
    set(gca,'ytick',ticks,'yticklabel',lblmat);
    
    %turn on grid if needed
    if(strcmp(get(hgridon,'checked'),'on'))
        grid on;
    end
    
    %put the title at the bottom
    if(~isempty(title))
        xlim=get(gca,'xlim');
        ylim=get(gca,'ylim');
        xtit=mean(xlim);
        ytit=max(ylim)+.05*abs(diff(ylim));
        htit=text(xtit,ytit,title);
        ext=get(htit,'extent');
        xtit=xtit-.5*ext(3);
        set(htit,'position',[xtit,ytit]);
    end
    
    %yaxis label
    ylabel('Time in Seconds');
    
    return;
    
end

%
%	*******REPOSITION THE TOPS LABELS*********
%
if(strcmp(action,'repositops'))
    h=get(gcf,'userdata');
    hstore1=h{12};
    hlocate=h{10};
    info=get(hstore1,'userdata');
    if(~isempty(info))
        xlim=get(gca,'xlim');
        ntops=info(1);
        if(ntops)
            hline=info(2:ntops+1);
            htext=info(ntops+2:2*ntops+1);
            
            for k=1:ntops
                pos=get(htext(k),'position');
                set(htext(k),'position',[xlim(2) pos(2)]);
                set(hline(k),'xdata',xlim);
            end
        end
    end
    
    %if locate was on, turn it back on
    if( strcmp(get(hlocate,'checked'),'on'))
        set(hlocate,'checked','off');
        theodisplay('locate');
    end
    
    return;
end


%
% manipulate the grid
%
if(strcmp(action,'grid'))
    hmenu=gcbo;
    flag=get(hmenu,'checked');
    
    if(strcmp(flag,'on'))%turn it off
        set(hmenu,'checked','off');
        grid off
    else
        set(hmenu,'checked','on');
        grid on
    end
    
    return;
end

% change timing line interval
if(strcmp(action,'timeint'))
    h=get(gcf,'userdata');
    hmsg=h{1};
    hmenu=gcbo;
    htimeint=get(hmenu,'parent');
    htimelabel=h{9};
    
    dat=get(htimeint,'userdata');

    tint_old=dat{end};%Previous time interval
    htint=dat(1:end-1);%array of tint menus
    
    %turn off previous, turn on current
    for k=1:length(htint)
        tintk=get(htint{k},'userdata');
        if(tint_old==tintk)
            set(htint{k},'checked','off');
        end
        if(hmenu==htint{k})
            set(htint{k},'checked','on');
            tint_new=get(htint{k},'userdata');
        end
    end
    dat{end}=tint_new;
    
    set(htimeint,'userdata',dat);
    
    %determine the yticks
    % get the trace etc
    trcs=get(hmsg,'userdata');
    t=trcs{2};
    
    %determine the labeling interval
    dat=get(htimelabel,'userdata');
    ievery=dat{end};
    
    %first tick at the first even interval smaller than t(1)
    tnot=floor(t(1)/tint_new)*tint_new;
    if( abs((tnot-t(1)))>.2*tint_new )
        tnot=ceil(t(1)/tint_new)*tint_new;
    end
    
    ticks=tnot:tint_new:t(length(t));
    %generate tick labels
    lblmat=[];
    for k=1:length(ticks)
        if( rem(k-1,ievery)== 0)
            lblmat=strmat(lblmat,num2str(ticks(k)));
        else
            lblmat=strmat(lblmat,' ');
        end
    end
    ind=find(lblmat==1);
    if(~isempty(ind))
        lblmat(ind)=32;
    end
    set(gca,'ytick',ticks,'yticklabel',lblmat);
    
    
end

%change the labeling interval
if(strcmp(action,'label'))
    h=get(gcf,'userdata');
    hmsg=h{1};
    htimeint=h{5};
    hmenu=gcbo;
    htimelabel=get(hmenu,'parent');
    
    dat=get(htimelabel,'userdata');
    ievery_old=dat{end};
    hievery=dat(1:end-1);

    %turn off previous, turn on current
    for k=1:length(hievery)
        ieveryk=get(hievery{k},'userdata');
        if(ievery_old==ieveryk)
            set(hievery{k},'checked','off');
        end
        if(hmenu==hievery{k})
            set(hievery{k},'checked','on');
            ievery_new=get(hievery{k},'userdata');
        end
    end
    dat{end}=ievery_new;
    
    set(htimelabel,'userdata',dat);
    
    %determine the yticks
    % get the trace etc
    trcs=get(hmsg,'userdata');
    t=trcs{2};
    
    %determine the tick interval
    dat=get(htimeint,'userdata');
    dt=dat{end};
    
    %first tick at the first even interval smaller than t(1)
    tnot=floor(t(1)/dt)*dt;
    if( abs((tnot-t(1)))>.2*dt )
        tnot=ceil(t(1)/dt)*dt;
    end
    
    ticks=tnot:dt:t(length(t));
    %generate tick labels
    lblmat=[];
    for k=1:length(ticks)
        if( rem(k-1,ievery_new)== 0)
            lblmat=strmat(lblmat,num2str(ticks(k)));
        else
            lblmat=strmat(lblmat,' ');
        end
    end
    ind=find(lblmat==1);
    if(~isempty(ind))
        lblmat(ind)=32;
    end
    set(gca,'ytick',ticks,'yticklabel',lblmat);
    
end


if(strcmp(action,'quit'))
    close(gcf);
    return;
end

%compute a new phase rotation
if(strcmp(action,'newphs'))
    % ask for the phase angle
    askthingsinit('theodisplay(''newphs2'')','Phase angle (degrees):');
    return;
end

if(strcmp(action,'newphs2'))
    h=get(gcf,'userdata');
    hmsg=h{1};
%     hoptions=h(2);
%     hgrid=h(3);
    hphase=h{6};
%     hhard=h(7);
    hquit=h{8};
    
    %get the angle
    a=askthingsfini;
    if(a==-1) %test for a cancel
        return;
    end
    
    ang=sscanf(a,'%f');
    if( isempty(ang) || ang<-180 || ang>180 )
        set(hmsg,'string','Angle must be between -180 and 180');
        askthingsinit('theodisplay(''newphs2'')','Phase angle (degrees):',a);
        return;
    end
    
    % get the trace etc
    trcs=get(hmsg,'userdata');
    theo=trcs{1};
    
    trcs=get(hquit,'userdata');
    
    phs=get(hphase,'userdata');
    
    %rotate
    newtrc=phsrot(theo,ang);
    
    %insert in the proper place
    ind=surround(phs,ang);
    if(isempty(ind))
        if(ang<phs(1))
            trcs=[newtrc trcs];
            phs=[ang phs];
        elseif( ang>phs(length(phs)))
            trcs=[trcs newtrc];
            phs=[phs ang];
        end
    else
        trcs=[trcs(:,1:ind) newtrc trcs(:,ind+1:length(phs))];
        phs=[phs(1:ind) ang phs(ind+1:length(phs))];
    end
    
    %set the userdata
    set(hquit,'userdata',trcs);
    set(hphase,'userdata',phs);
    
    %plot
    
    theodisplay('plot');
    
    return;
    
end

% hardcopy
if(strcmp(action,'hardcopy') )
    h=get(gcf,'userdata');
    hhard=h{7};
    
    scales=get(hhard,'userdata');
    
    shardcopy(gcf,'theodisplay(''hardcopy2'')',scales(1),scales(2),...
        'temp.ps');
    return;
end

if(strcmp(action,'hardcopy2') )
    h=get(gcf,'userdata');
    hhard=h{7};
    hmessage = h{1};
    
    % get the dialog answers and test for reasonableness
    [ps_scale,xlength,ylength,xscale,yscale]=shardcopyfini;
    
    if( ps_scale== -999.) %test for a cancel
        set(hmessage,'string','Plot cancelled');
        return;
    end
    
    %put out a message
    set(hmessage,'string',...
        ['plotsize is ' num2str(xlength) ' by ' num2str(ylength) ...
        ' inches. use ps_scale = ' num2str(ps_scale) ' in CHVSUB']);
    
    %remember the scales
    set(hhard,'userdata',[xscale yscale]);
    
    return
end

%turn on locate info
if(strcmp(action,'locate'))
    h=get(gcf,'userdata');
    hmsg=h{1};
    
    hlocate=h{10};
    
    flag=get(hlocate,'checked');
    
    if(strcmp(flag,'off'))
        set(hlocate,'checked','on');
        
        set(gcf,'windowbuttonmotionfcn','theodisplay(''showloc'')');
        
        set(hmsg,'string','Move mouse to display location');
        
    else
        set(hlocate,'checked','off');
        set(gcf,'windowbuttonmotionfcn','');
        set(hmsg,'string','Locate turned off');
    end
    
    return;
    
end

if(strcmp(action,'showloc'))
    h=get(gcf,'userdata');
    %make sure we have a theogram display window
    doit=0;
    if(length(h)==12)
        % if( get(h(3),'type','uimenu') )
        
        if(strcmp(get(h{3},'label'),'Timing Lines'))
            doit=1;
        end
        % end
    end
    if(doit)
        hmsg=h{1};
        
        pt=get(gca,'currentpoint');
        
        set(hmsg,'string',['Time = ' num2str(pt(1,2))]);
    end
    
    return;
    
end

if(strcmp(action,'writelas'))
    h=get(gcf,'userdata');
    hmsg=h{1};
%     hoptions=h{2};
    hphase=h{6};
    hquit=h{8};
    hwritelas=h{11};
    % hphase = h(6) ... vector of theogram phases
    % hquit = h(8) ... matrix of phase rotated theograms
    
    %put up a file dialog
    [filename,path]=uiputfile('*','Select LAS Output File');
    
    if(isempty(filename)||~ischar(filename))
        set(hmsg,'string','Output aborted,no file name given');
        return;
    end
    
    fullfilename=[path filename];
    
    %set the watch cursor
    set(gcf,'pointer','watch');
    
    %get the lasheader
    lasheader=get(hwritelas,'userdata');
    
    %get the theogram
    trcmat=get(hmsg,'userdata');
    theo=trcmat{1};
    t=trcmat{2};
    
    %get the phase rotated traces
    phstrcs=get(hquit,'userdata');
    
    %get the phases
    phs=get(hphase,'userdata');
    
    %generate names for the lasheader
    nphs=length(phs);
    phsnames=32*ones(nphs,4);
    phsdesc='Phase rotated theo';
    phsdesc=ones(nphs,1)*phsdesc;
    for k=1:nphs
        sphs=int2str(abs(phs(k)));
        bb=blanks(3-length(sphs));
        if(phs(k)>0)
            phsnames(k,:)=['+' sphs bb];
        else
            phsnames(k,:)=['-' sphs bb];
        end
    end
    %modify the header
    u=char(32*ones(nphs,1));
    lasheader=lashcurves(lasheader,char(phsnames),u,u,phsdesc);
    
    %get the wavelet
    % 	w=get(hoptions,'userdata');
    % 	w=w(:,1);
    % 	%pad wavelet with -999.25 which las will interprete as nan's
    % 	[ns,m]=size(trcmat);
    % 	nw=length(w);
    % 	if(ns>nw)
    % 		w=[w;-999.25*ones(ns-nw,1)];
    % 	elseif(nw>ns)
    % 		t=trcmat(:,1);
    % 		t=xcoord(t(1),t(2)-t(1),nw);
    % 		trcmat=[t [trcmat(2:4);-999.25*ones(nw-ns,3)]];
    % 		phstrcs=[phstrcs; -999.25*ones(nw-ns,nphs)];
    % 	end
    
    
    %reorder the trcmat
    %trcmat=trcmat(:,[2 1 3 4]);
    
    %attach the the phases
    lasmat=[t theo phstrcs];
    
    %write out
    writelas(fullfilename, lasheader, lasmat);
    
    set(hmsg,'string',['Output successful to ' fullfilename]);
    
    set(gcf,'pointer','arrow');
    
    return;
end