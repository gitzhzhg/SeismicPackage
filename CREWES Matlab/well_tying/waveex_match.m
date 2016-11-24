function waveex_match(s,t,r,tr,name,w,tw)
% waveex_match: wavelet explorer for match filter method
%
% waveex_match(s,t,r,tr,name,w,tw)
%
% extraction method found in: extract_wavelets_match
% 
% s ... seismic trace
% t ... time coordinate for s
% r ... reflectivity
% tr ... time coordinate for r
% name ... title string
% ******** default '' *********
% w ... wavelet known to be embedded (if s is a synthetic with known
%           wavelet)
% ******** default nan (i.e. no known wavelet) *******
% tw ... time coordinate for s
% Must be provided if w is provided
%

if(~ischar(s))
    action='init';
else
    action=s;
end

if(strcmp(action,'init'))
    if(length(s)~=length(t))
        error('s and t must have the same length');
    end
    if(length(r)~=length(tr))
        error('r and tr must have the same length');
    end
    ind=near(t,tr(1),tr(end));
    if(length(ind)~=length(r))
        error('tr must lie within bounds of t')
    end
    if(nargin<5)
        name=[];
    end
    if(nargin<6)
        w=nan;
        tw=nan;
    end
    
    %build the figure
    vars.s=s;
    vars.r=r;
    vars.t=t;
    vars.tr=tr;
    vars.name=name;
    vars.w=w;
    vars.tw=tw;
    
    waveex('build',vars);
    hfig=gcf;
    
    set(hfig,'name','Wavelet Explorer: Match Filter')
    
    %set title
    xnow=.02;ynow=.93;
    width=.2;ht=.05;
    fs=12;
    uicontrol(gcf,'style','text','string','Wavelet Explorer: Match Filter','tag','title',...
        'units','normalized','position',[xnow,ynow,width,ht],'fontsize',fs,'fontweight','bold',...
        'horizontalalignment','left','backgroundcolor',[1 1 1]);
    
    %info button
    xnow=xnow+1.1*width;
    ynow=ynow+.02;
    width=.05;fs=12;ht=.03;
    uicontrol(gcf,'style','pushbutton','string','Info?','tag','info','units','normalized',...
        'position',[xnow,ynow,width,ht],'callback','waveex(''help'')',...
        'backgroundcolor',.95*[1 1 0],'fontsize',fs);
    
    %name object
    xnow=xnow+2*width;
%     ynow=ynow-.02;
    width=.4;fs=12;ht=.03;
    uicontrol(gcf,'style','text','string',vars.name,'tag','info','units','normalized',...
        'position',[xnow,ynow,width,ht],...
        'backgroundcolor',.95*[1 1 0],'fontsize',fs,'fontweight','bold',...
        'horizontalalignment','left','backgroundcolor',[1 1 1]);
    
    %set default parameters
    hcntl=findobj(hfig,'tag','control');
    wavestruc=get(hcntl,'userdata');
    parms={'timezero',50,'wlen',.4,'mu',1,'stab',.01};
    wavestruc.defparms=parms;
    wavestruc.fmin=5;
    wavestruc.fmax=.25/(t(2)-t(1));
    set(hcntl,'userdata',wavestruc);
    
    
    
    %install controls
    ht=1/16;
    sep=.5*ht;
    wlbl=.5;
    wtxt=.3;
    wsep=.05;
    xnow=wsep;
    ynow=1-ht-sep;
    fs=11;
    uicontrol(hcntl,'style','text','string','Top gate:','units','normalized',...
        'position',[xnow,ynow,wlbl,ht],...
        'tooltipstring','Top of wavelet extraction gate (sec)',...
        'backgroundcolor',[1 1 1],...
        'fontsize',fs);
    xnow=xnow+wlbl+wsep;
    %ynow=ynow+.25*ht;
    uicontrol(hcntl,'style','edit','string',num2str(tr(1),4),'tag','tw1',...
        'units','normalized','position',[xnow,ynow,wtxt,ht],...
        'callback','waveex(''waveletgatechangetext'')',...
        'tooltipstring','You can choose this by dragging the dotted green lines',...
        'fontsize',fs);
    %
    xnow=wsep;
    ynow=ynow-ht-sep;
    uicontrol(hcntl,'style','text','string','Bottom gate:','units','normalized',...
        'position',[xnow,ynow,wlbl,ht],...
        'tooltipstring','Bottom of wavelet extraction gate (sec)',...
        'backgroundcolor',[1 1 1],...
        'fontsize',fs);
    xnow=xnow+wlbl+wsep;
    uicontrol(hcntl,'style','edit','string',num2str(tr(end),4),'tag','tw2',...
        'units','normalized','position',[xnow,ynow,wtxt,ht],...
        'callback','waveex(''waveletgatechangetext'')',...
        'tooltipstring','You can choose this by dragging the dotted green lines',...
        'fontsize',fs);
    %
    xnow=wsep;
    ynow=ynow-ht-sep;
    tzpct=findparm(parms,'timezero');
    %     uicontrol(hcntl,'style','popupmenu','string',{'Symmetric','Not symmetric'},...
    %         'tag','causal','units','normalized','position',[xnow,ynow,1.5*wlbl,ht],...
    %         'value',icausal+1,'tooltipstring','Symmetic means t=0 is in the middle. The wavelet will not be symmetric.',...
    %         'fontsize',fs);
    uicontrol(hcntl,'style','text','string','Pct time zero:','units','normalized',...
        'position',[xnow,ynow,wlbl,ht],...
        'tooltipstring','Percent of wavelet before t=0.',...
        'backgroundcolor',[1 1 1],...
        'fontsize',fs);
    xnow=xnow+wlbl+wsep;
    %ynow=ynow+.25*ht;
    uicontrol(hcntl,'style','edit','string',num2str(tzpct),'tag','timezero',...
        'units','normalized','position',[xnow,ynow,wtxt,ht],...
        'tooltipstring','A number between 0 and 100.',...
        'fontsize',fs);
    %

    xnow=wsep;
    ynow=ynow-ht-sep;
    val=findparm(parms,'wlen');
    uicontrol(hcntl,'style','text','string','Wavelet length:','units','normalized',...
        'position',[xnow,ynow,wlbl,ht],...
        'tooltipstring','Wavelet length as a fraction of gate size',...
        'backgroundcolor',[1 1 1],...
        'fontsize',fs);
    xnow=xnow+wlbl+wsep;
    uicontrol(hcntl,'style','edit','string',num2str(val),'tag','wlen',...
        'units','normalized','position',[xnow,ynow,wtxt,ht],...
        'tooltipstring','Enter a value between 0 and 1',...
        'fontsize',fs);
    %
    xnow=wsep;
    ynow=ynow-ht-sep;
    val=findparm(parms,'mu');
    uicontrol(hcntl,'style','text','string','Smoothness:','units','normalized',...
        'position',[xnow,ynow,wlbl,ht],...
        'tooltipstring','Smootheness parameter, 0 is no smoothing, greater than 0 is smoothing)',...
        'backgroundcolor',[1 1 1],...
        'fontsize',fs);
    xnow=xnow+wlbl+wsep;
    uicontrol(hcntl,'style','edit','string',num2str(val),'tag','mu',...
        'units','normalized','position',[xnow,ynow,wtxt,ht],...
        'tooltipstring','Enter a non-negative number',...
        'backgroundcolor',[1 1 1],...
        'fontsize',fs);
    %
    xnow=wsep;
    ynow=ynow-ht-sep;
    val=findparm(parms,'stab');
    uicontrol(hcntl,'style','text','string','Stability:','units','normalized',...
        'position',[xnow,ynow,wlbl,ht],...
        'tooltipstring','Stability constant (used in wavelet inversion)',...
        'backgroundcolor',[1 1 1],...
        'fontsize',fs);
    xnow=xnow+wlbl+wsep;
    uicontrol(hcntl,'style','edit','string',num2str(val),'tag','stab',...
        'units','normalized','position',[xnow,ynow,wtxt,ht],...
        'tooltipstring','Enter a number between 0 and 1',...
        'fontsize',fs);
    %
    xnow=wsep;
    ynow=ynow-ht-sep;
    val=wavestruc.fmin;
    uicontrol(hcntl,'style','text','string','Min freq:','units','normalized',...
        'position',[xnow,ynow,wlbl,ht],...
        'tooltipstring','This is for a post-decon bandpass filter',...
        'backgroundcolor',[1 1 1],...
        'fontsize',fs);
    xnow=xnow+wlbl+wsep;
    uicontrol(hcntl,'style','edit','string',num2str(val),'tag','fmin',...
        'units','normalized','position',[xnow,ynow,wtxt,ht],...
        'tooltipstring','Enter a non negative number in HZ',...
        'fontsize',fs);
    %
    xnow=wsep;
    ynow=ynow-ht-sep;
    val=wavestruc.fmax;
    uicontrol(hcntl,'style','text','string','Max freq:','units','normalized',...
        'position',[xnow,ynow,wlbl,ht],...
        'tooltipstring','This is for a post-decon bandpass filter',...
        'backgroundcolor',[1 1 1],...
        'fontsize',fs);
    xnow=xnow+wlbl+wsep;
    uicontrol(hcntl,'style','edit','string',num2str(val),'tag','fmax',...
        'units','normalized','position',[xnow,ynow,wtxt,ht],...
        'tooltipstring','Enter a non negative number in HZ',...
        'fontsize',fs);
    %
    xnow=wsep;
    ynow=ynow-ht-sep;
    uicontrol(hcntl,'style','pushbutton','string','Apply','units','normalized',...
        'position',[xnow,ynow,1.2*wlbl,1.2*ht],'callback','waveex_match(''apply'')',...
        'tooltipstring','Push to extract a wavelet with the above parameters',...
        'backgroundcolor',.95*[1 1 0],...
        'fontsize',fs);
    

    
    waveex('traceplot')
    
    waveex('tvphsdelay')
    
elseif(strcmp(action,'apply'))
    parms=getparms;
    pcttimezero=findparm(parms,'timezero');
    if(isnan(pcttimezero) || pcttimezero<0 || pcttimezero>100)
        msgbox('bad value for Pct time zero');
        return;
    end
    
    tw1=findparm(parms,'tw1');
    tw2=findparm(parms,'tw2');
    wlen=findparm(parms,'wlen');
    stab=findparm(parms,'stab');
    mu=findparm(parms,'mu');
    fmin=findparm(parms,'fmin');
    fmax=findparm(parms,'fmax');

    hcntl=findobj(gcf,'tag','control');
    wavestruc=get(hcntl,'userdata');
    s=wavestruc.s;
    r=wavestruc.r;
    t=wavestruc.t;
    tr=wavestruc.tr;
    ind=near(t,tr(1),tr(end));
    wlen2=wlen*(tw2-tw1);
    nwlen=round(wlen2/(t(2)-t(1)));
    if(pcttimezero==50)
        icausal=0;
    else
        icausal=round(pcttimezero*nwlen/100)+1;%we want pcttimezero=0 to correspond to minimum phase which is icausal=1
    end
    [ww,tww]=extract_wavelets_match(s(ind),t(ind),r,.5*(tw1+tw2),tw2-tw1,wlen,mu,icausal);
    w=ww{1};
    tw=tww{1};
    
    w=butterband(w,tw,fmin,fmax,4,0);%applying filter to wavelet makes it less jittery
    rf=butterband(r,tr,fmin,fmax,4,0);%filtered reflectivity needed for diagnostics
    
%     if(icausal==0)
%         mname='noncausal';
%     else
%         mname='causal';
%     end
    
    name=[time2str(tw1) '-' time2str(tw2) '_' num2str(pcttimezero) '_' num2str(wlen) '_' ...
        num2str(mu) '_'  num2str(fmin) '-' num2str(fmax)];
    %compute the trace model and the reflectivity estimate
    izero=near(tw,0);
    sp=convz(r,w,izero);
    [d,twd]=toinv(w,stab,round(length(w)/2),icausal);
    rp=convz(s,d,twd);
%     if(icausal==0)
%         rp=convz(s,d);
%     else
%         rp=convm(s,d);
%     end
    rpf=butterband(rp,t,fmin,fmax,4,0);%always apply filter after inverse
    wavestruc.sp=sp;
    wavestruc.rp=rpf;
    
    %compute performance diagnostics
    %ind=near(t,tw1,tw2);%extraction window
    %ind points to entire range of r, not just extraction window
    result.ccs=maxcorr(s(ind),sp);
    result.pep=penpred(s(ind),sp);
    result.ccr=maxcorr(rf,rpf(ind));
    result.prr=penpred(rf,rpf(ind));
    result.wavelabel=['ccs=' num2str(sigfig(result.ccs(1),2)) ', pep=' ...
        num2str(sigfig(result.pep,2)) ', ccr=' num2str(sigfig(result.ccr(1),2))...
        ', prr=' num2str(sigfig(result.prr,2))];
    
    %get windowing parameters
    htv=findobj(gcf,'tag','tvcontrol');
    htwin=findobj(htv,'tag','twin');
    val=get(htwin,'string');
    twin=str2double(val);
    if(isnan(twin)||twin>t(end)||twin<0)
        msgbox('bad value for time window size');
        return;
    end
    htinc=findobj(htv,'tag','tinc');
    val=get(htinc,'string');
    tinc=str2double(val);
    if(isnan(tinc)||tinc>t(end)||tinc<0)
        msgbox('bad value for window increment');
        return;
    end
    
    %populate results structure
    result.rf=rf;
    result.w=w;
    result.tw=tw;
    result.tw1=tw1;
    result.tw2=tw2;
    result.twin=twin;
    result.tinc=tinc;
    result.params=parms;
    result.fmin=fmin;
    result.fmax=fmax;
    result.name=name;
    result.icausal=icausal;
%     result.twin=wavestruc.twin;
%     result.tinc=wavestruc.tinc;
    result.sp=sp;
    result.rp=rpf;
    nresults=length(wavestruc.results);
    iresult=nresults+1;
    wavestruc.results{iresult}=result;
    wavestruc.iresult=iresult;

    set(hcntl,'userdata',wavestruc);
    
    %waveex('plotresult');
    waveex('traceplot');
    waveex('tvphsdelay');
    waveex('waveletplot');
end
end


function parms=getparms
parms=cell(16,1);
hcntl=findobj(gcf,'tag','control');

hobj=findobj(hcntl,'tag','timezero');
parms{1}='timezero';
val=get(hobj,'string');
timezero=round(str2double(val));
parms{2}=timezero;

hobj=findobj(hcntl,'tag','tw1');
tw1=str2double(get(hobj,'string'));
parms{3}='tw1';
parms{4}=tw1;

hobj=findobj(hcntl,'tag','tw2');
tw2=str2double(get(hobj,'string'));
parms{5}='tw2';
parms{6}=tw2;

hobj=findobj(hcntl,'tag','wlen');
wlen=str2double(get(hobj,'string'));
parms{7}='wlen';
parms{8}=wlen;

hobj=findobj(hcntl,'tag','mu');
mu=str2double(get(hobj,'string'));
parms{9}='mu';
parms{10}=mu;


hobj=findobj(hcntl,'tag','stab');
stab=str2double(get(hobj,'string'));
parms{11}='stab';
parms{12}=stab;

hobj=findobj(hcntl,'tag','fmin');
fmin=str2double(get(hobj,'string'));
parms{13}='fmin';
parms{14}=fmin;

hobj=findobj(hcntl,'tag','fmax');
fmax=str2double(get(hobj,'string'));
parms{15}='fmax';
parms{16}=fmax;
end