function phaser(arg)
% PHASER: An interactive tool to test your ability to guess phase
%
% Just type phaser at the command line
%

if(nargin<1)
    action='init';
else
    action=arg;
end

switch action
    case 'init'
        figure
        x0=.1;y0=.1;
        xwid=.2;
        axht=.8;
        axwd=.8-xwid;
        axes('position',[x0+xwid,y0,axwd,axht],'tag','axes');
        dt=.001;
        tlen=.2;
        fdom=30;
        [w,tw]=ricker(dt,fdom,tlen);
        w=w/max(abs(w));
        wenv=env(w);
        hh=plot(tw,w,tw,wenv,'r:',tw,-wenv,'r:');
        set(hh(1),'color',.75*[1 1 1],'linewidth',1.5);
        legend('original wavelet','envelope')
        grid
        prepfiga
        title('Guess the phase rotation')
        
        ht=.05;
        sep=.01;
        wid=.7*xwid;
        xnow=x0;
        ynow=y0+axht-.6*ht;
        uicontrol(gcf,'style','text','string','Mode:','tag','modelbl',...
            'units','normalized','position',[xnow,ynow,.5*wid,.5*ht]);
        ynow=ynow-.4*ht;
        uicontrol(gcf,'style','popupmenu','string',{'wavelet','trace'},...
            'tag','mode','units','normalized','position',[xnow+.5*wid+sep,ynow,.5*wid-sep,ht],...
            'callback','phaser(''mode'')');
        ynow=ynow-.6*ht;
        uicontrol(gcf,'style','text','string','Wavelet:','tag','wavelbl',...
            'units','normalized','position',[xnow,ynow,.5*wid,.5*ht]);
        ynow=ynow-.4*ht;
        uicontrol(gcf,'style','popupmenu','string',{'Ricker','min phase'},...
            'tag','wavelet','units','normalized',...
            'position',[xnow+.5*wid+sep,ynow,.5*wid-sep,ht]);
        ynow=ynow-.6*ht;
        uicontrol(gcf,'style','text','string','fdom:','tag','fdomlbl',...
            'units','normalized','position',[xnow,ynow,.5*wid,.5*ht]);
        ynow=ynow-.4*ht;
        fdoms=20:10:80;
        idom=near(fdoms,fdom);
        strfdom=cell(size(fdoms));
        for k=1:length(fdoms)
            strfdom{k}=[int2str(fdoms(k)) ' Hz'];
        end
        uicontrol(gcf,'style','popupmenu','string',strfdom,...
            'tag','fdom','units','normalized','position',[xnow+.5*wid+sep,ynow,.5*wid-sep,ht],...
            'value',idom,'userdata',fdoms);
        ynow=ynow-ht;
        uicontrol(gcf,'style','pushbutton','string','New Phase',...
            'tag','newphase','units','normalized','position',[xnow,ynow,wid,ht],...
            'callback','phaser(''newphase'')','userdata',{w,tw,hh(1),hh(2),hh(3)});
        ynow=ynow-.75*ht-sep;
        uicontrol(gcf,'style','text','string','Enter phase:','tag','phlbl',...
            'units','normalized','position',[xnow,ynow,.5*wid,.5*ht]);
        ynow=ynow-.25*ht;
        uicontrol(gcf,'style','edit','string','??','tag','phase',...
            'units','normalized','position',[xnow+.5*wid+sep,ynow,.5*wid-sep,ht],...
            'callback','phaser(''guess'')','userdata',[0 0]);%UD will be the number of the guess and the phase
        ynow=ynow-ht-sep;
        uicontrol(gcf,'style','pushbutton','string','Clear phases',...
            'tag','clear','units','normalized','position',[xnow,ynow,wid,ht],...
            'callback','phaser(''clear'')');
        ynow=ynow-ht-sep;
        uicontrol(gcf,'style','pushbutton','string','Example',...
            'tag','example','units','normalized','position',[xnow,ynow,wid,ht],...
            'callback','phaser(''example'')');
        ynow=ynow-ht-sep;
        uicontrol(gcf,'style','text','string','Average error: 0',...
            'tag','error','units','normalized','position',[xnow,ynow,wid,.5*ht],...
            'userdata',0);
        
        phaser('newphase')
        
    case 'newphase'
        ph=randi([-180 180],1);
        hnewp=findobj(gcf,'tag','newphase');
        udat1=get(hnewp,'userdata');
        w=udat1{1};
        tw=udat1{2};
        hphase=findobj(gcf,'tag','phase');
        udat2=get(hphase,'userdata');
        N=udat2(1)+1;
        set(hphase,'userdata',[N ph],'string','');
        kols=get(gca,'colororder');
        nkols=size(kols,1);
        ikol=rem(N,nkols);
        if(ikol==0); ikol=nkols; end
        wp=phsrot(w,ph);
        hh=line(tw,wp,'color',kols(ikol,:),'linewidth',2);
        legend([udat1{3} udat1{4} hh],'original wavelet','envelope','unknown phase');
        nn=length(udat1);
        udat1{nn+1}=hh;
        set(hnewp,'userdata',udat1);
        
    case 'guess'
        hguess=findobj(gcf,'tag','phase');
        str=get(hguess,'string');
        phg=str2double(str);
        if(isnan(phg) || isempty(phg))
            msgbox('I don''t recognized that as a number...','Oops!');
            return;
        end
        if(phg<-180 || phg>180)
            msgbox('Your guess must be a number between -180 and 180','Oops!')
        end
        %get the answer
        hphase=findobj(gcf,'tag','phase');
        udat=get(hphase,'userdata');
        ph=udat(2);
        err=phaseerr(ph,phg);
        title(['Actual phase: ' int2str(ph) ', guessed phase: ' int2str(phg) ', error: ' int2str(err)])
        %update average error
        hphase=findobj(gcf,'tag','phase');
        udat2=get(hphase,'userdata');
        N=udat2(1);
        herror=findobj(gcf,'tag','error');
        averr=get(herror,'userdata');
        averr=(err+(N-1)*averr)/N;
        set(herror,'string',['Average error: ' int2str(averr)]);
        set(herror,'userdata',averr);
    case 'clear'
        hnewp=findobj(gcf,'tag','newphase');
        udat=get(hnewp,'userdata');
        nn=length(udat);
        for k=6:nn
            delete(udat{k})
        end
        legend([udat{3} udat{4}],'original wavelet','envelope');
        set(hnewp,'userdata',udat(1:5));
        hphase=findobj(gcf,'tag','phase');
        %udat2=get(hphase,'userdata');
        set(hphase,'userdata',[0 0],'string','');
        herror=findobj(gcf,'tag','error');
        set(herror,'string','Average error: 0');
        title('Click ''New Phase'' to try again')
        
    case 'mode'
        phaser('clear');
        hmode=findobj(gcf,'tag','mode');
        str=get(hmode,'string');
        ival=get(hmode,'value');
        mode=str{ival};
        hwave=findobj(gcf,'tag','wavelet');
        str=get(hwave,'string');
        ival=get(hwave,'value');
        wavelet=str{ival};
        hfdom=findobj(gcf,'tag','fdom');
        fdoms=get(hfdom,'userdata');
        ival=get(hfdom,'value');
        fdom=fdoms(ival);
        tlen=6/fdom;
        tmax=.5;
        if(2.5*tlen>tmax)
            tmax=1;
        end
        dt=.001;
        %make the wavelet
        switch wavelet
            case 'Ricker'
                [w,tw]=ricker(dt,fdom,tlen);
            case 'min phase'
                [w,tw]=wavemin(dt,fdom,tlen);
                %extend it a bit in negative time
                npad=round(.5*tlen/dt);
                w=[zeros(npad,1);w];
                tw=[dt*(-npad:1:-1)';tw];
        end
        switch mode
            case 'trace'
                [r,t]=reflec(tmax,dt);
                switch wavelet
                    case 'Ricker'
                        s=convz(r,w);
                    case 'min phase'
                        s=convm(r,w);
                end
                lw=1;
            case 'wavelet'
                s=w;
                t=tw;
                lw=1.5;
        end
        senv=env(s);
        hnewp=findobj(gcf,'tag','newphase');
        udat=get(hnewp,'userdata');
        udat{1}=s;
        udat{2}=t;
        %plot s
        h1=line(t,s);
        kol=get(udat{3},'color');
        set(h1,'color',kol,'linewidth',lw);
        delete(udat{3});
        udat{3}=h1;
        %plot env
        h2=line(t,senv);
        h3=line(t,-senv);
        kol=get(udat{4},'color');lw=get(udat{4},'linewidth');ls=get(udat{4},'linestyle');
        set([h2 h3],'color',kol,'linewidth',lw,'linestyle',ls);
        delete([udat{4} udat{5}]);
        udat{4}=h2;udat{5}=h3;
        set(hnewp,'userdata',udat)
        legend([h1 h2],['original ' mode],'envelope')
        title('Guess the phase rotation')
        phaser('newphase')
    case 'example'
        hwave=findobj(gcf,'tag','wavelet');
        str=get(hwave,'string');
        ival=get(hwave,'value');
        wavelet=str{ival};
        hfdom=findobj(gcf,'tag','fdom');
        fdoms=get(hfdom,'userdata');
        ival=get(hfdom,'value');
        fdom=fdoms(ival);
        tlen=6/fdom;
        dt=.001;
        
        figure
        phases=-90:45:90;
        switch wavelet
            case 'Ricker'
                [w,tw]=ricker(dt,fdom,tlen);
            case 'min phase'
                [w,tw]=wavemin(dt,fdom,tlen);
                %extend it a bit in negative time
                npad=round(.5*tlen/dt);
                w=[zeros(npad,1);w];
                tw=[dt*(-npad:1:-1)';tw];
        end
        ws=zeros(length(w),length(phases));
        names=cell(1,length(phases));
        for k=1:length(phases)
            ws(:,k)=phsrot(w,phases(k));
            names{k}=[int2str(phases(k)) 'deg'];
        end
        hh=plot(tw,ws);
        ind=near(phases,0);
        set(hh(ind),'linewidth',1,'color',.7*ones(1,3));
        legend(names)
        grid
        prepfiga
        
        
end
        