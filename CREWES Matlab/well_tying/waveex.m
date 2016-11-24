function waveex(action,vars)
%
% Main data structure:
% wavestruc.s ... the seismic trace
% wavestruc.sp ... the modelled trace: conv(w,r), same length as r
% wavestruc.r ... the reflectivity
% wavestruc.rp ... the estimated reflectivity conv(w^-1,s), same length as s
% wavestruc.t ... time coordinate for sw
% wavestruc.tr ... time coordinate for r
% wavestruc.tvphase ... initial time-variant phase
% wavestruc.tvdelay ... initial time-variant delay
% wavestruc.name ... name of this experiment (string)
% wavestruc.defparams ... default parameters as cell array of name-value pairs
% wavestruc.results ... cell array of results structures
% wavestruc.iresult ... number of current result
% wavestruc.fmin ... default min frequency (for filter)
% wavestruc.fmax ... default max frequency (for filter)
%
% Results structure
% result.w ... estimated wavelet
% result.tw ... time coordinate for w
% result.tw1 ... start of estimation time zone
% result.tw2 ... end of estimation time zone
% result.tvphase ... estimated time variant phase 
% result.tvdelay ... estimated time variant delay
% result.tvpep ... estimated time variant pep (proportion of energy predicted)
% result.tvprr ... estimated time variant prr (proportion of reflectivity resolved)
% result.params ... cell array of name-value pairs with extraction parameters
% result.tinc ... time increment used in time-variant estimations
% result.twin ... window size used in time-variant estimations
% result.fmin ... min frequency (for filter)
% result.fmax ... max frequency (for filter)
% result.name ... unique name for the result (used in legend)
% result.sp ... model trace: reflectivity convolved with wavelet
% result.rp ... reflectivity estimate: trace convolved with wavelet inverse (filter applied)
% result.rf ... filtered reflectivity
% result.icausal ... 1 for causal, 0 for noncausal
% result.ccs ... output from maxcorr for s versus sp
% result.pep ... pep fofr s versus sp
% result.ccr ... output from maxcorr for rf versus rp (r filtered)
% result.prr ... portion of reflectivity resolved for rf versus rp
% result.wavelabel ... text string summarizing the above four measures
%

global DRAGLINE_MOTION DRAGLINE_YLIMS DRAGLINE_SHOWPOSN DRAGLINE_CALLBACK
global ALIGN_REF_RNEW ALIGN_REF_TRNEW

if(strcmp(action,'build'))
    %on this call, vars should be a structure with fields: s,r,t,tr,name,w,tw
    figure;
    whitefig;

    
    figsize(.9,.7);
    
    x0=.1;y0=.1;
    htwave=.3;%height of wavelet axes
    httr=.4;%height of traces axes;
    wwave=.4;
    wspec=.4;
    wtr=.4;
    wtvphs=.1;
    wtvdel=.1;
    wtvpep=.1;
    wcntl=.15;
    sep=.1;%big separation
    smsep=.01;%small separation
    %trace axes
    fs=12;
    fs2=10;
    xnow=x0;ynow=y0+htwave+sep;
    axes('position',[xnow,ynow,wtr,httr],'tag','traces','fontsize',fs);
    title('traces');
    %legend toggle button
    cht=.05;
    uicontrol(gcf,'style','pushbutton','string','Legend off','units','normalized',...
       'position',[xnow,ynow-.5*cht,.15*wtr,.5*cht],'callback','waveex(''legendtoggle'')',...
       'tag','legendtoggle','userdata',1);%1 for on 0 for off
    %wavelet axes
    xnow=x0;ynow=y0;
    axes('position',[xnow,ynow,wwave,htwave],'tag','wavelets','fontsize',fs);
    title('wavelets');
    %spectra axes
    xnow=xnow+.5*sep+wwave;
    axes('position',[xnow,ynow,wspec,htwave],'tag','spectra','fontsize',fs);
    title('wavelet spectra');
    %tvphs axes
    xnow=x0+wtr;
    ynow=y0+htwave+sep;
    axes('position',[xnow,ynow,wtvphs,httr],'tag','tvphase','fontsize',fs2);
    title('phase')
    %tvdelay axes
    xnow=xnow+wtvphs;
    axes('position',[xnow,ynow,wtvdel,httr],'tag','tvdelay','fontsize',fs2);
    title('delay')
    %tvpep axes
    xnow=xnow+wtvdel;
    axes('position',[xnow,ynow,wtvpep,httr],'tag','tvpep','ytick',[],'xtick',[],'fontsize',fs2);
    title({'PEP','& PRR'})
    
    %build the control panel
    xnow=xnow+smsep+wtvpep;
    ynow=ynow-2*smsep;
    hcntl=uipanel(gcf,'title','Wavelet control panel','position',[xnow,ynow,wcntl,httr+smsep],...
        'tag','control','backgroundcolor',[1 1 1]);
    
    %build the tvcontrolpanel
    ynow=ynow+httr+2*smsep;
    htvcntl=uipanel(gcf,'title','Time-variant analysis controls','position',[xnow,ynow,wcntl,.2*httr],...
        'tag','tvcontrol','backgroundcolor',[1 1 1]);
    ht=1/3;
    sep=.5*ht;
    wlbl=.6;
    wtxt=.3;
    wsep=.05;
    xxnow=wsep;
    yynow=1-ht-sep;
    fs=11;
    uicontrol(htvcntl,'style','text','string','Time window size:','units','normalized',...
        'position',[xxnow,yynow,wlbl,ht],...
        'tooltipstring','Half width of Gaussian window (sec)','backgroundcolor',[1 1 1],...
        'fontsize',fs);
    xxnow=xxnow+wlbl+wsep;
    %ynow=ynow+.25*ht;
    uicontrol(htvcntl,'style','edit','string',num2str(.3),'tag','twin',...
        'units','normalized','position',[xxnow,yynow,wtxt,ht],...
        'tooltipstring','Longer windows mean lower resolution but greater reliability',...
        'backgroundcolor',[1 1 1],'fontsize',fs);
    
    xxnow=wsep;
    yynow=yynow-ht-sep;
    uicontrol(htvcntl,'style','text','string','Window increment:','units','normalized',...
        'position',[xxnow,yynow,wlbl,ht],...
        'tooltipstring','Time separation between adjacent windows (sec)','backgroundcolor',[1 1 1],...
        'fontsize',fs);
    xxnow=xxnow+wlbl+wsep;
    %ynow=ynow+.25*ht;
    uicontrol(htvcntl,'style','edit','string',num2str(.1),'tag','tinc',...
        'units','normalized','position',[xxnow,yynow,wtxt,ht],...
        'tooltipstring','This should be smaller than the window size','backgroundcolor',[1 1 1],...
        'fontsize',fs);
    
    %bigfont;
    
    
    %build the wavestruc and store in control panel
    wavestruc.s=vars.s;
    wavestruc.sp=zeros(size(vars.r));
    %wavestruc.sp=vars.s;
    wavestruc.r=vars.r;
    wavestruc.rp=zeros(size(vars.s));
    wavestruc.t=vars.t;
    wavestruc.tr=vars.tr;
    wavestruc.iresult=0;
    wavestruc.results=[];
    wavestruc.twin=.3;%make sure this is consistent with the value seeded in the GUI
    wavestruc.tinc=.05;%make sure this is consistent with the value seeded in the GUI
    if(isfield(vars,'name'))
        wavestruc.name=vars.name;
    else
        wavestruc.name=[];
    end
    if(isfield(vars,'w'))
        wavestruc.w=vars.w;
    else
        wavestruc.w=nan;
    end
    if(isfield(vars,'tw'))
        wavestruc.tw=vars.tw;
    else
        wavestruc.tw=nan;
    end
    
    set(hcntl,'userdata',wavestruc);
    
elseif(strcmp(action,'legendtoggle'))
    haxe=findobj(gcf,'tag','traces');
    hbutton=gco;
    val=get(hbutton,'userdata');
    %set(gcf,'currentaxes',haxe);
    if(val==0)
        %turn legend on
        legend(haxe,'show');
        set(hbutton,'string','Legend off','userdata',1);
    else
        %turn legend off
        legend(haxe,'hide')
        set(hbutton,'string','Legend on','userdata',0);
    end
    

    
    
elseif(strcmp(action,'traceplot'))
    hcntl=findobj(gcf,'tag','control');
    wavestruc=get(hcntl,'userdata');
    s=wavestruc.s;
    r=wavestruc.r;
    sp=wavestruc.sp;
    rp=wavestruc.rp;
    t=wavestruc.t;
    tr=wavestruc.tr;
    %name=wavestruc.name;
    htraxe=findobj(gcf,'tag','traces');
    set(gcf,'currentaxes',htraxe);
    trplot({t tr tr t},{s sp r rp},'direction','v','normalize',1);
    legend('trace','model trace','reflectivity','estimated r',...
        'location','northwest');
    title('traces')
    set(htraxe,'tag','traces');
    %determine legend status
    hlegendbutton=findobj(gcf,'tag','legendtoggle');
    val=get(hlegendbutton,'userdata');
    if(val==0)
        legend(htraxe,'hide');
    end
    %draw wavelet gate
    [tw1,tw2]=getwaveletgate;
    xl=xlim;
    hl=line(xl,[tw1 tw1],[1 1],'linestyle',':','color','g','buttondownfcn',...
        'waveex(''dragline'')','tag','tw1','linewidth',2);
    set(hl,'zdata',[10 10]);
    hl=line(xl,[tw2 tw2],[1 1],'linestyle',':','color','g','buttondownfcn',...
        'waveex(''dragline'')','tag','tw2','linewidth',2);
    set(hl,'zdata',[10 10]);
    bigfont(gca,1.5,1)
    
    hcntx=uicontextmenu;
    uimenu(hcntx,'label','Alignment tool','callback',@alignment);
    set(gca,'uicontextmenu',hcntx);
    
elseif(strcmp(action,'dragline'))
    hline=gco;
    wavestruc=getwavestruc;
    tr=wavestruc.tr;
    lineid=get(hline,'tag');
    [tw1,tw2]=getwaveletgate;
    factor=.1;
    if(strcmp(lineid,'tw1'))
        del=(tw2-tw1)*factor;
        ylims=[tr(1) tw2-del];
    elseif(strcmp(lineid,'tw2'))
        del=(tw2-tw1)*factor;
        ylims=[tw1+del tr(end)];
    else
        return;
    end
    DRAGLINE_MOTION='yonly';
    DRAGLINE_YLIMS=ylims;
    DRAGLINE_SHOWPOSN='on';
    DRAGLINE_CALLBACK='waveex(''waveletgatechangeline'')';
    dragline('click');
elseif(strcmp(action,'waveletgatechangeline'))
    id=get(gco,'tag');
    y=get(gco,'ydata');
    hcntl=findobj(gcf,'tag','control');
    hbox=findobj(hcntl,'tag',id);
    set(hbox,'string',num2str(y(1),4));
elseif(strcmp(action,'waveletgatechangetext'))
    [tw1,tw2]=getwaveletgate;
    hbox=gco;
    id=get(hbox,'tag');
    val=get(hbox,'string');
    y=str2double(val);
    wavestruc=getwavestruc;
    tr=wavestruc.tr;
    if(~isnumeric(y)||isnan(y))
        error(['bad value in ' id])
    end
    del=tw2-tw1;
    factor=.1;
    htraces=findobj(gcf,'tag','traces');
    hline=findobj(htraces,'tag',id);
    switch id
        case 'tw1'
            if(y<tr(1))
                y=tr(1);
            end
            if(y>=tw2)
                y=tw2-factor*del;
            end
            set(hline,'ydata',[y y]);
            
        case 'tw2'
            if(y>tr(end))
                y=tr(end);
            end
            if(y<=tw1)
                y=tw1+factor*del;
            end
            set(hline,'ydata',[y y]);
    end

    set(hbox,'string',num2str(y,4));
% elseif(strcmp(action,'plotresult'))
%     wavestruc=getwavestruc;
%     iresult=wavestruc.iresult;
    
elseif(strcmp(action,'tvphsdelay'))
    %measure time-variant phase and delay
    wavestruc=getwavestruc;
    s=wavestruc.s;
    r=wavestruc.r;
    sp=wavestruc.sp;
    rp=wavestruc.rp;
    t=wavestruc.t;
    tr=wavestruc.tr;
    %see if we need to measure the before phase and delay
    if(~isfield(wavestruc,'tvphase'))
        twin=wavestruc.twin;
        tinc=wavestruc.tinc;
        flag=1;
%         s=s/max(s);
%         r=r/max(r);
        [stie,sreftie,tvdelay,tvphase]=welltie(s,t,r,tr,twin,tinc,flag);
        clear stie sreftie
        wavestruc.tvdelay=tvdelay;
        wavestruc.tvphase=tvphase;
    end
    setwavestruc(wavestruc);
    thisresult=getcurrentresult;
    %measure on current result
    if(~isempty(thisresult))
        twinr=thisresult.twin;
        tincr=thisresult.tinc;
        [twin,tinc]=gettvparms;
        %measure time-variant phase and delay
        doit=1;
        if(isfield(thisresult,'tvdelay'))
            doit=0;
        end
        if((doit)||(twin~=twinr)||(tinc~=tincr))
            %recompute if twin or tinc have changed
            flag=1;
            [stie,sreftie,tvdelay,tvphase]=welltie(rp,t,r,tr,twin,tinc,flag);
            thisresult.tvdelay=tvdelay;
            thisresult.tvphase=tvphase;
        else
            tvdelay=thisresult.tvdelay;
            tvphase=thisresult.tvphase;
        end
        %measure pep
        %measure time-variant pep and prr
        doit=1;
        if(isfield(thisresult,'tvpep'))
            doit=0;
        end
        if((doit)||(twin~=twinr)||(tinc~=tincr))
            %recompute if twin or tinc have changed
            ind=near(t,tr(1),tr(end));
            pep=tvpep(s(ind),sp,tr,twin,tinc);
            rf=butterband(r,tr,thisresult.fmin,thisresult.fmax,4,0);
            prr=tvprr(rf,rp(ind),tr,twin,tinc);
            thisresult.tvpep=pep;
            thisresult.tvprr=prr;
        end
        
        thisresult.twin=twin;
        thisresult.tinc=tinc;
        setcurrentresult(thisresult);
    end
    %plot the stuff
    htvphs=findobj(gcf,'tag','tvphase');
    htvdelay=findobj(gcf,'tag','tvdelay');
    htvpep=findobj(gcf,'tag','tvpep');
    set(gcf,'currentaxes',htvphs);
    if(~isempty(thisresult))
        plot(wavestruc.tvphase,t,tvphase,t)
    else
        plot(wavestruc.tvphase,t)
    end
    grid
    xlim([-180 180])
    set(htvphs,'tag','tvphase','yticklabel',[],'ydir','reverse')
    title('phase')
    xlabel('degrees')
    
    set(gcf,'currentaxes',htvdelay);
    if(~isempty(thisresult))
        plot(wavestruc.tvdelay,t,tvdelay,t)
        legend('before','after');
    else
        plot(wavestruc.tvdelay,t)
    end
    grid
    set(htvdelay,'tag','tvdelay','yticklabel',[],'ydir','reverse')
    xl=get(gca,'xlim');
    xl=max(abs(xl));
    if(xl<.1);xl=.1;end
    xlim([-xl xl])
    xtick([-xl/2 0 xl/2])
    title('delay')
    xlabel('sec')
    
    set(gcf,'currentaxes',htvpep);
    if(~isempty(thisresult))
        plot(thisresult.tvpep,tr,thisresult.tvprr,tr)
        legend('pep','prr');
        grid
        ylim([t(1) t(end)])
        set(htvpep,'tag','tvpep','yticklabel',[],'ydir','reverse')
%         xl=get(gca,'xlim');
%         xl=max(abs(xl));
%         if(xl<.1);xl=.1;end
%         xlim([-xl xl])
        title({'PEP','& PRR'})
        xlim([0 1])
    end
    
elseif(strcmp(action,'waveletplot'))
    hfig=gcf;
    wavestruc=getwavestruc;
    iresult=wavestruc.iresult;
    nresults=length(wavestruc.results);
    hwaveaxe=findobj(gcf,'tag','wavelets');
    hspecaxe=findobj(gcf,'tag','spectra');
    if(nresults==0)
        haxe=get(hfig,'currentaxes');
        set(hfig,'currentaxes',hwaveaxe);
        cla;
        set(hwaveaxe,'tag','wavelets');
        set(hfig,'currentaxes',hspecaxe);
        cla
        set(hspecaxe,'tag','spectra');
        set(hfig,'currentaxes',haxe);
        return;
    end
    wavelets=cell(1,nresults);
    tws=wavelets;
    names=wavelets;
    windowflags=ones(1,nresults+1);%used in dbspec
    labels=cell(1,nresults);
    for k=1:nresults
        wavelets{k}=wavestruc.results{k}.w;
        tws{k}=wavestruc.results{k}.tw;
        names{k}=wavestruc.results{k}.name;
        windowflags(k)=wavestruc.results{k}.icausal+1;
        labels{k}=wavestruc.results{k}.wavelabel;
    end
    set(hfig,'currentaxes',hwaveaxe);
    hh=trplot(tws,wavelets,'order','d','zerolines','y','names',labels,...
        'namesalign','left','nameslocation','middle','nameshift',-.3,'fontsize',7);
    lw=get(hh{iresult},'linewidth');
    set(hh{iresult},'linewidth',3*lw);
    legend(cell2mat(hh),names,'interpreter','none','location','northwest')
    set(hwaveaxe,'tag','wavelets');
    title('wavelets (thick line is selected wavelet)')
    bigfont(gca,1.5,1);
    %set context menu's and tags on each wavelet
    for k=1:length(hh)
       hc=uicontextmenu;
       set(hh{k},'uicontextmenu',hc,'tag',int2str(k));
       uimenu(hc,'label','select','callback','waveex(''selectwavelet'')');
       uimenu(hc,'label','delete','callback','waveex(''deletewavelet'')');
    end
    
    set(hfig,'currentaxes',hspecaxe);
    s=wavestruc.s;
    t=wavestruc.t;
    [tw1,tw2]=getwaveletgate;
    ind=near(t,tw1,tw2);
    hh=dbspec(tws{1},[wavelets s(ind)],'windowflags',windowflags,'normoption',1);
    %hh=dbspec(tws{1},[wavelets s(ind)],'windowflags',ones(1,nresults+1));
    ylim([-100 0])
    lw=get(hh{iresult},'linewidth');
    xd=get(hh{iresult},'xdata');
    set(hh{iresult},'linewidth',3*lw,'zdata',(length(wavelets)+1)*ones(size(xd)));
    xd=get(hh{end},'xdata');
    set(hh{end},'zdata',-1*ones(size(xd)),'color',[.7 .7 .7])
    set(hspecaxe,'tag','spectra');
    title('wavelet spectra (gray line is trace spectrum in gate)')
    bigfont(gca,1.5,1);
    
elseif(strcmp(action,'deletewavelet'))
    %determine which wavelet
    tag=get(gco,'tag');
    ikill=str2double(tag);
    if(isnan(ikill))
        return;
    end
    wavestruc=getwavestruc;
    wavestruc.results(ikill)=[];
    iresult=wavestruc.iresult;
    if(iresult>ikill)
        iresult=iresult-1;
    end
    if(iresult==ikill)
        if(ikill==1)
            iresult=1;
        else
            iresult=iresult-1;
        end
    end
    wavestruc.iresult=iresult;
    setwavestruc(wavestruc);
    waveex('traceplot');
    waveex('tvphsdelay');
    waveex('waveletplot');
elseif(strcmp(action,'selectwavelet'))
    %determine which wavelet
    tag=get(gco,'tag');
    iselect=str2double(tag);
    if(isnan(iselect))
        return;
    end
    wavestruc=getwavestruc;
    wavestruc.iresult=iselect;
    wavestruc.rp=wavestruc.results{iselect}.rp;
    wavestruc.sp=wavestruc.results{iselect}.sp;
    setwavestruc(wavestruc);
    
    fillcontrolpanel(wavestruc.results{iselect}.params);
    setwaveletgate(wavestruc.results{iselect}.tw1,wavestruc.results{iselect}.tw2);
    %settvparms(wavestruc.results{iselect}.twin,wavestruc.results{iselect}.tinc);
    waveex('traceplot');
    waveex('tvphsdelay');
    waveex('waveletplot');
    
elseif(strcmp(action,'receivenewref'))
    rnew=ALIGN_REF_RNEW;
    trnew=ALIGN_REF_TRNEW;
    wavestruc=getwavestruc;
    wavestruc.r=rnew;
    wavestruc.tr=trnew;
    wavestruc.rp=zeros(size(wavestruc.s));
    wavestruc.sp=zeros(size(rnew));
    wavestruc=rmfield(wavestruc,{'tvphase','tvdelay'});
    setwavestruc(wavestruc);
    waveex('traceplot')
    
    waveex('tvphsdelay')
elseif(strcmp(action,'help'))
    xxx=which('waveex');
    ind=strfind(xxx,'waveex.m');
    yyy=[xxx(1:ind-1) 'Waveex_Help.pdf'];
    winopen(yyy);
end

end

function thisresult=getcurrentresult
wavestruc=getwavestruc;
if(~isempty(wavestruc.results))
    ir=wavestruc.iresult;
    thisresult=wavestruc.results{ir};
else
    thisresult=[];
end
end

function setcurrentresult(thisresult)
wavestruc=getwavestruc;
ir=wavestruc.iresult;
if(ir>0)
    wavestruc.results{ir}=thisresult;
    setwavestruc(wavestruc);
end
end

function wavestruc=getwavestruc
hcntl=findobj(gcf,'tag','control');
wavestruc=get(hcntl,'userdata');
end

function setwavestruc(wavestruc)
hcntl=findobj(gcf,'tag','control');
set(hcntl,'userdata',wavestruc);
end

function [tw1,tw2]=getwaveletgate
wavestruc=getwavestruc;
htw1=findobj(gca,'tag','tw1');
if(isempty(htw1))
    %go to boxes
    hcntl=findobj(gcf,'tag','control');
    hbox=findobj(hcntl,'tag','tw1');
    if(isempty(hbox))
        tr=wavestruc.tr;
        tw1=tr(1);
        tw2=tr(end);
    else
        val=get(hbox,'string');
        tw1=str2double(val);
        hbox=findobj(hcntl,'tag','tw2');
        val=get(hbox,'string');
        tw2=str2double(val);
    end

else
    htw2=findobj(gca,'tag','tw2');
    y=get(htw1,'ydata');
    tw1=y(1);
    y=get(htw2,'ydata');
    tw2=y(1);
end
end

function fillcontrolpanel(parms)
hcntl=findobj(gcf,'tag','control');
for k=1:2:length(parms)
    tag=parms{k};
    hobj=findobj(hcntl,'tag',tag);
    if(~isempty(hobj))
        if(strcmp(get(hobj,'style'),'edit'))
            %so fill in the edit text box
            set(hobj,'string',num2str(parms{k+1}));
        else
            %this will mean a popupmenu
            choices=get(hobj,'string');
            thischoice=parms{k+1};
            for kk=1:length(choices)
                if(strcmp(choices{kk},thischoice))
                    val=kk;
                end
            end
            set(hobj,'value',val);
        end
    end
end
end

function setwaveletgate(tw1,tw2)
hcntl=findobj(gcf,'tag','control');
htw=findobj(hcntl,'tag','tw1');
set(htw,'string',num2str(tw1));
htw=findobj(hcntl,'tag','tw2');
set(htw,'string',num2str(tw2));
%now change the green lines
htraces=findobj(gcf,'tag','traces');
hline=findobj(htraces,'tag','tw1');
if(isempty(hline))
    return
end
set(hline,'ydata',[tw1 tw1]);
hline=findobj(htraces,'tag','tw2');
set(hline,'ydata',[tw2 tw2]);
end

function [twin,tinc]=gettvparms
wavestruc=getwavestruc;
t=wavestruc.t;
htvcntl=findobj(gcf,'tag','tvcontrol');
hobj=findobj(htvcntl,'tag','twin');
val=get(hobj,'string');
twin=str2double(val);
if(isnan(twin)||twin>.5*(t(end)-t(1))||twin<0)
    msgbox('Bad value for time window, cannot proceed');
    twin=nan;
    tinc=nan;
    return;
end
hobj=findobj(htvcntl,'tag','tinc');
val=get(hobj,'string');
tinc=str2double(val);
if(isnan(tinc)||tinc>twin||tinc<0)
    msgbox('Bad value for window increment, cannot proceed');
    twin=nan;
    tinc=nan;
    return;
end
end

function settvparms(twin,tinc)
htvcntl=findobj(gcf,'tag','tvcontrol');
hobj=findobj(htvcntl,'tag','twin');
set(hobj,'string',time2str(twin));
hobj=findobj(htvcntl,'tag','tinc');
set(hobj,'string',time2str(tinc));
end

function alignment(source,callbackdata)
wavestruc=getwavestruc;
s=wavestruc.s;
r=wavestruc.r;
t=wavestruc.t;
tr=wavestruc.tr;
cb='waveex(''receivenewref'')';
wfact=.5;
hfact=.8;
name1=get(gcf,'name');%name of wavelet explorer window
align_ref(r,tr,s,t,cb,wfact,hfact);
name2=get(gcf,'name');%name of alignment tool window
set(gcf,'name',[name2 ' (' name1 ')'])
end
        
