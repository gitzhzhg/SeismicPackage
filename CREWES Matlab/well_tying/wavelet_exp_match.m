function wavelet_exp_match(s,t,r,tr,name,w,tw)
% WAVELET_EXP_MATCH: wavelet explorer for match filter method
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
    
    set(hfig,'name','Wavelet explorer: Match filter')
    
    %set default parameters
    hcntl=findobj(hfig,'tag','control');
    wavestruc=get(hcntl,'userdata');
    parms={'causal',0,'wlen',.4,'mu',1,'stab',.01};
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
    icausal=findparm(parms,'causal');
    uicontrol(hcntl,'style','popupmenu','string',{'Not Causal','Causal'},...
        'tag','causal','units','normalized','position',[xnow,ynow,1.5*wlbl,ht],...
        'value',icausal+1,'tooltipstring','Choose causal or not');
    %
    xnow=wsep;
    ynow=ynow-ht-1.5*sep;
    uicontrol(hcntl,'style','text','string','Top gate:','units','normalized',...
        'position',[xnow,ynow,wlbl,ht],...
        'tooltipstring','Top of wavelet extraction gate (sec)');
    xnow=xnow+wlbl+wsep;
    %ynow=ynow+.25*ht;
    uicontrol(hcntl,'style','edit','string',num2str(tr(1),4),'tag','tw1',...
        'units','normalized','position',[xnow,ynow,wtxt,ht],...
        'callback','waveex(''waveletgatechangetext'')',...
        'tooltipstring','You can choose this by dragging the dotted green lines');
    %
    xnow=wsep;
    ynow=ynow-ht-sep;
    uicontrol(hcntl,'style','text','string','Bottom gate:','units','normalized',...
        'position',[xnow,ynow,wlbl,ht],...
        'tooltipstring','Bottom of wavelet extraction gate (sec)');
    xnow=xnow+wlbl+wsep;
    uicontrol(hcntl,'style','edit','string',num2str(tr(end),4),'tag','tw2',...
        'units','normalized','position',[xnow,ynow,wtxt,ht],...
        'callback','waveex(''waveletgatechangetext'')',...
        'tooltipstring','You can choose this by dragging the dotted green lines');
    %
    xnow=wsep;
    ynow=ynow-ht-sep;
    val=findparm(parms,'wlen');
    uicontrol(hcntl,'style','text','string','Wavelet length:','units','normalized',...
        'position',[xnow,ynow,wlbl,ht],...
        'tooltipstring','Wavelet length as a fraction of gate size');
    xnow=xnow+wlbl+wsep;
    uicontrol(hcntl,'style','edit','string',num2str(val),'tag','wlen',...
        'units','normalized','position',[xnow,ynow,wtxt,ht],...
        'tooltipstring','Enter a value between 0 and 1');
    %
    xnow=wsep;
    ynow=ynow-ht-sep;
    val=findparm(parms,'mu');
    uicontrol(hcntl,'style','text','string','Smoothness:','units','normalized',...
        'position',[xnow,ynow,wlbl,ht],...
        'tooltipstring','Smootheness parameter, 0 is no smoothing, greater than 0 is smoothing)');
    xnow=xnow+wlbl+wsep;
    uicontrol(hcntl,'style','edit','string',num2str(val),'tag','mu',...
        'units','normalized','position',[xnow,ynow,wtxt,ht],...
        'tooltipstring','Enter a non-negative number');
    %
    xnow=wsep;
    ynow=ynow-ht-sep;
    val=findparm(parms,'stab');
    uicontrol(hcntl,'style','text','string','Stability:','units','normalized',...
        'position',[xnow,ynow,wlbl,ht],...
        'tooltipstring','Stability constant (used in wavelet inversion)');
    xnow=xnow+wlbl+wsep;
    uicontrol(hcntl,'style','edit','string',num2str(val),'tag','stab',...
        'units','normalized','position',[xnow,ynow,wtxt,ht],...
        'tooltipstring','Enter a number between 0 and 1');
    %
    xnow=wsep;
    ynow=ynow-ht-sep;
    val=wavestruc.fmin;
    uicontrol(hcntl,'style','text','string','Min frequency:','units','normalized',...
        'position',[xnow,ynow,wlbl,ht],...
        'tooltipstring','This is for a post-decon bandpass filter');
    xnow=xnow+wlbl+wsep;
    uicontrol(hcntl,'style','edit','string',num2str(val),'tag','fmin',...
        'units','normalized','position',[xnow,ynow,wtxt,ht],...
        'tooltipstring','Enter a non negative number in HZ');
    %
    xnow=wsep;
    ynow=ynow-ht-sep;
    val=wavestruc.fmax;
    uicontrol(hcntl,'style','text','string','Max frequency:','units','normalized',...
        'position',[xnow,ynow,wlbl,ht],...
        'tooltipstring','This is for a post-decon bandpass filter');
    xnow=xnow+wlbl+wsep;
    uicontrol(hcntl,'style','edit','string',num2str(val),'tag','fmax',...
        'units','normalized','position',[xnow,ynow,wtxt,ht],...
        'tooltipstring','Enter a non negative number in HZ');
    %
    xnow=wsep;
    ynow=ynow-ht-sep;
    uicontrol(hcntl,'style','pushbutton','string','Apply','units','normalized',...
        'position',[xnow,ynow,1.2*wlbl,ht],'callback','wavelet_exp_match(''apply'')',...
        'tooltipstring','Push to extract a wavelet with the above parameters');
    
    waveex('traceplot')
    
    waveex('tvphsdelay')
elseif(strcmp(action,'apply'))
    parms=getparms;
    causal=findparm(parms,'causal');
    if(strcmp(causal,'Causal'))
        icausal=1;
    else
        icausal=0;
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
    [ww,tww]=extract_wavelets_match(s(ind),t(ind),r,.5*(tw1+tw2),tw2-tw1,wlen,mu,icausal);
    w=ww{1};
    tw=tww{1};
    
    if(icausal==0)
        mname='noncausal';
    else
        mname='causal';
    end
    
    name=[mname '_' time2str(tw1) '-' time2str(tw2) '_' num2str(wlen) '_' ...
        num2str(mu) '_'  num2str(fmin) '-' num2str(fmax)];
    %compute the trace model and the reflectivity estimate
    izero=near(tw,0);
    sp=convz(r,w,izero);
    d=toinv(w,stab,round(length(w)/2),icausal);
    if(icausal==0)
        rp=convz(s,d);
    else
        rp=convm(s,d);
    end
    rpf=butterband(rp,t,fmin,fmax,4,0);
    wavestruc.sp=sp;
    wavestruc.rp=rpf;
    
    %make a results structure
    result.w=w;
    result.tw=tw;
    result.tw1=tw1;
    result.tw2=tw2;
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

hobj=findobj(hcntl,'tag','causal');
parms{1}='causal';
choices=get(hobj,'string');
ival=get(hobj,'value');
parms{2}=choices{ival};

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