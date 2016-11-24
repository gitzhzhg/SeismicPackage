function align_ref(r,tr,s,t,cb,wfact,htfact)
%
% r ... reflectivity
% tr ... time coordinate for r
% s ... trace
% ts ... time coordinate for s
% cb ... string giving callback to be executed when "return shifted reflectivity" button is pushed.
%   The adjusted reflectivity is placed in the global ALIGN_REF_RNEW and the corresponding time
%   coordinate is in the global ALIGN_REF_TRNEW .
% wfact ... width of figure as a fraction of calling figure
% htfact ... height of figure as a fraction of calling figure
%
% 
global DRAGLINE_MOTION DRAGLINE_YLIMS DRAGLINE_SHOWPOSN DRAGLINE_CALLBACK

if(~ischar(r))
    action='init';
else
    action=r;
end


if(strcmp(action,'init'))
   dt=t(2)-t(1);
   dtr=tr(2)-tr(1);
   small=.0000001;
   if((abs(dt-dtr)>small))
       error('log and seismic sample rates are different');
   end
   pos=get(gcf,'position');%calling figure
   hcallingfig=gcf;
   hfig=figure;
   h=pos(4)*htfact;
   w=pos(3)*wfact;
   set(hfig,'position',[pos(1)+.5*w,pos(2),w,h]);
   set(hfig,'name','Alignment tool');
   xnot=.1;ynot=.1;
   waxe=.55;
   htaxe=.8;
   htraxe=axes('position',[xnot ynot waxe htaxe],'tag','traces');
   
   
   sep=.1;
   sep2=.05;
   xnow=xnot+waxe+sep;
   waxe2=.2;
   htaxe2=.4;
   hccaxe=axes('position',[xnow,ynot+3*sep2,waxe2,htaxe2]);
   
   htpan=.2;
   wpan=.15;
   fs=10;
   xnow=xnot+waxe+.2*sep;
   ynow=ynot+htaxe-htpan;
   hpan1=uipanel(gcf,'title','Interactive shifting','fontsize',fs,...
       'position',[xnow,ynow,wpan,htpan]);
   xnow=xnow+wpan;
   hpan2=uipanel(gcf,'title','Automatic shifting','fontsize',fs,...
       'position',[xnow,ynow,wpan,htpan]);
   w=.45;
   h=.25;
   sep3=.05;
   xn=sep3;
   yn=1-h-sep3;
   uicontrol(hpan1,'style','text','string','Time shift (ms)','units','normalized',...
       'position',[xn,yn,w,h],'tag','timeshiftlabel',...
       'tooltipstring','A value in MILLISECONDS');
    xn=xn+w+sep3;
    shifts=dt*[.25 .5 1:10 15 20]*1000;
    sshifts=cell(size(shifts));
    for k=1:length(shifts)
        sshifts{k}=num2str(shifts(k));
    end
    ival=near(shifts,dt*1000);
    uicontrol(hpan1,'style','popupmenu','string',sshifts,'units','normalized',...
       'position',[xn,yn,w,h],'tag','timeshift','value',ival,...
       'tooltipstring','A value in MILLISECONDS');
   xn=sep3;
   yn=yn-h-sep3;
   w=.8;
   uicontrol(hpan1,'style','pushbutton','string','Shift DOWN','units','normalized',...
       'position',[xn,yn,w,h],'tag','shiftdown',...
       'tooltipstring','Shift reflectivity DOWN by the amount above',...
       'callback',@applyshift);
   xn=sep3;
   yn=yn-h-sep3;
   uicontrol(hpan1,'style','pushbutton','string','Shift UP','units','normalized',...
       'position',[xn,yn,w,h],'tag','shiftup',...
       'tooltipstring','Shift reflectivity UP by the amount above',...
       'callback',@applyshift,'userdata',[htraxe hccaxe]);
   
   xn=sep3;
   w=.8;
   h=.25;
   yn=1-h-sep3;
   uicontrol(hpan2,'style','popupmenu','string',{'Bulk shift','TV ccorr','DTW'},'units','normalized',...
       'position',[xn,yn,w,h],'tag','automethod','value',1,...
       'tooltipstring','Choose the automatic method');
   xn=sep3;
   yn=yn-h-sep3;
   uicontrol(hpan2,'style','pushbutton','string','Apply','units','normalized',...
       'position',[xn,yn,w,h],'tag','autoapply',...
       'tooltipstring','Apply the above method',...
       'callback',@applyauto);
   xn=.5*sep3;
   yn=yn-h-sep3;
   uicontrol(hpan2,'style','pushbutton','string','Undo','units','normalized',...
       'position',[xn,yn,.4*w,h],'tag','autoundo',...
       'tooltipstring','Undo the last auto application',...
       'callback',@undoauto,'enable','off');
   xn=xn+.4*w+.5*sep3;
   uicontrol(hpan2,'style','pushbutton','string','Show shifts','units','normalized',...
       'position',[xn,yn,.7*w,h],'tag','showshifts',...
       'tooltipstring','show the estimated time shifts',...
       'callback',@showshifts);
   
   xnow=xnot+waxe+sep;
   ynow=ynot;
   w=.1;
   h=.05;
   uicontrol(gcf,'style','pushbutton','string','Return shifted reflectivity','units','normalized',...
       'position',[xnow,ynow,2*w,h],'tag','returnref',...
       'tooltipstring','Return modified reflectivity to original program',...
       'callback',@returnref,'userdata',{cb, [tr(1) tr(end)], hcallingfig});
   
   %expand r to size of z
   ind=near(t,tr(1),tr(end));
   rnew=zeros(size(s));
   rnew(ind)=r;

   cc=maxcorr(rnew,s,100);
   
   dobj.s=s;
   dobj.r=r;
   dobj.tr=tr;
   dobj.t=t;
   dobj.rnew=rnew;%padded version of r to the same length as s
   dobj.cc=cc;
   dobj.tg1=tr(1);
   dobj.tg2=tr(end);
   %set(hdown,'userdata',dobj);
   setdataobject(dobj);
   
   plottraces(t,s,rnew);
elseif(strcmp(action,'dragline'))
    hline=gco;
    dobj=getdataobject;
    tr=dobj.tr;
    lineid=get(hline,'tag');
    [tg1,tg2]=getcorrelationgate;
    factor=.1;
    if(strcmp(lineid,'tg1'))
        del=(tg2-tg1)*factor;
        ylims=[tr(1) tg2-del];
    elseif(strcmp(lineid,'tg2'))
        del=(tg2-tg1)*factor;
        ylims=[tg1+del tr(end)];
    else
        return;
    end
    DRAGLINE_MOTION='yonly';
    DRAGLINE_YLIMS=ylims;
    DRAGLINE_SHOWPOSN='on';
    DRAGLINE_CALLBACK='align_ref(''corrgatechange'');';
    dragline('click');
elseif(strcmp(action,'corrgatechange'))
    dobj=getdataobject;
    h1=findobj(gca,'tag','tg1');
    h2=findobj(gca,'tag','tg2');
    y1=get(h1,'ydata');
    y2=get(h2,'ydata');
    dobj.tg1=y1(1);
    dobj.tg2=y2(1);
    setdataobject(dobj);
    plottraces(dobj.t,dobj.s,dobj.rnew);

end

end

function applyshift(source,cbdata)
% userdata of the reflectivity
% udat{1} ... cell array of graphics handles for the anchors
% udat{2} ... cell array of times for the anchors
% udat{3} ... a single graphics handle denoting the application point
% udat{4} ... the time of the application point
%

hrr=findobj(gcf,'tag','returnref');
urr=get(hrr,'userdata');
tr_lims=urr{2};%these are the limit times of r. We may need to adjust them here

sgn=+1;
if(strcmp(get(source,'tag'),'shiftup'))
    sgn=-1;
end
dobj=getdataobject;
s=dobj.s;
rnew=dobj.rnew;
t=dobj.t;
%get the handle of reflectivity
hup=findobj(gcf,'tag','shiftup');
haxes=get(hup,'userdata');
axes(haxes(1));
href=findobj(gca,'tag','ref');
% interrogate href user data for anchor info
uu=get(href,'userdata');
shifttype='bulk';
if(~isempty(uu))
    ha=uu{1};
    if(~isempty(ha))
        %ok we have anchors
        hpoint=uu{3};
        if(isempty(hpoint))
            msgbox('You must click on the reflectivity to define the application point')
            return;
        else
            shifttype='stretch';
            tpoint=uu{4};
            tanch=uu{2};
        end
    end
end
htshift=findobj(gcf,'tag','timeshift');
% val=get(htshift,'string');
% tshift=str2double(val)/1000;
sshifts=get(htshift,'string');
ival=get(htshift,'value');
tshift=str2double(sshifts{ival})/1000;
if(isnan(tshift) || abs(tshift)>.5*(t(end)-t(1)) || abs(tshift<.0001))
    msgbox('Bad time shift value');
    return;
end
if(strcmp(shifttype,'bulk'))
    rnew=stat(rnew,t,sgn*abs(tshift));
    tr_lims=tr_lims+sgn*abs(tshift);
else
    deltmp=zeros(1,length(tanch)+3);
    ttmp=sort([t(1) tanch tpoint t(end)]);
    ind=find(ttmp==tpoint);
    if(ind<=2)
        %tpoint is less than any anchor
        deltmp(1)=sgn*abs(tshift);
        deltmp(ind)=sgn*abs(tshift);
    elseif(ind>=length(ttmp)-1)
        %tpoint is greater than any anchor
        deltmp(ind)=sgn*abs(tshift);
        deltmp(end)=sgn*abs(tshift);
    else
        deltmp(ind)=sgn*abs(tshift);
    end
    delt=interp1(ttmp,deltmp,t);
    rnew=stretcht(rnew,t,delt);
    ind=near(t,tr_lims(1));
    tr_lims(1)=tr_lims(1)+delt(ind);
    ind=near(t,tr_lims(2));
    tr_lims(2)=tr_lims(2)+delt(ind);
end
urr{2}=tr_lims;
set(hrr,'userdata',urr);

plottraces(t,s,rnew)
dobj.rnew=rnew;
setdataobject(dobj);
end

function applyauto(source,cbdata)
% userdata of the reflectivity
% udat{1} ... cell array of graphics handles for the anchors
% udat{2} ... cell array of times for the anchors
% udat{3} ... a single graphics handle denoting the application point
% udat{4} ... the time of the application point
%

hrr=findobj(gcf,'tag','returnref');
urr=get(hrr,'userdata');
tr_lims=urr{2};%these are the limit times of r. We may need to adjust them here

hautometh=findobj(gcf,'tag','automethod');
methods=get(hautometh,'string');
imeth=get(hautometh,'value');
method=methods{imeth};
dobj=getdataobject;
s=dobj.s;
rnew=dobj.rnew;
t=dobj.t;
dt=t(2)-t(1);
hundo=findobj(gcf,'tag','autoundo');
hshowshifts=findobj(gcf,'tag','showshifts');
switch method
    case 'Bulk shift'
        [tg1,tg2]=getcorrelationgate;

        tshift=.1*(t(end)-t(1));
        answers=inputdlg({'start correlation gate (sec)','end correlation gate (sec)',...
            'max lag to search (sec)'},'Bulk shift',1,{time2str(tg1), time2str(tg2), time2str(tshift)});
        if(isempty(answers))
            return
        end
        tg1=str2double(answers{1});
        if(isnan(tg1) || tg1<t(1) || tg1>t(end))
            msgbox('bad value for correlation gate start');
            return;
        end
        tg2=str2double(answers{2});
        if(isnan(tg2) || tg2<tg1 || tg2>t(end))
            msgbox('bad value for correlation gate end');
            return;
        end
        tshift=str2double(answers{3});
        if(isnan(tshift) || tshift<0 || tshift>.5*(t(end)-t(1)))
            msgbox('bad value for correlation max lag');
            return;
        end
        %run maxcorr
        ind=near(t,tg1,tg2);
        n=round(tshift/dt);
        cc=maxcorr(s(ind),rnew(ind),n);
        tstat=cc(2)*dt;
        rnew2=stat(rnew,t,tstat);
        tr_lims=tr_lims+tstat;
        dobj.rnew=rnew2;
        set(hundo,'userdata',{rnew, [tstat tstat]},'enable','on');
        set(hshowshifts,'userdata',{tstat*ones(size(t)),rnew,rnew2,t,method})
    case 'TV ccorr'
        tr=dobj.tr;
        twin=.2;
        tinc=.05;
        tshift=.2*twin;
        answers=inputdlg({'Gaussian window half-width (sec)','Window increment (sec)',...
            'max lag to search (sec)'},'TV cc',1,{time2str(twin), time2str(tinc), time2str(tshift)});
        if(isempty(answers))
            return
        end
        twin=str2double(answers{1});
        if(isnan(twin) || twin<0 || twin>(t(end)-t(1))*0.5)
            msgbox('bad value for Gaussian window half-width');
            return;
        end
        tinc=str2double(answers{2});
        if(isnan(tinc) || tinc<0 || tinc>twin)
            msgbox('bad value for window increment');
            return;
        end
        tshift=str2double(answers{3});
        if(isnan(tshift) || tshift<0 || tshift>.5*twin)
            msgbox('bad value for correlation max lag');
            return;
        end
        ind=near(t,tr(1),tr(end));
        [cc,tcc]=tvmaxcorr(s(ind),rnew(ind),t(ind),twin,tinc,tshift);
        delt=cc(:,2)*dt;
        delt2=[delt(1);delt;delt(end)];
        tcc2=[t(1);tcc;t(end)];
        tstretch=interp1(tcc2,delt2,t);
        rnew2=stretcht(rnew,t,tstretch);
        i1=near(t,tr(1));
        i2=near(t,tr(end));
        tr_lims=tr_lims+[tstretch(i1) tstretch(i2)];
        dobj.rnew=rnew2;
        set(hundo,'userdata',{rnew, [tstretch(i1) tstretch(i2)]},'enable','on');
        set(hshowshifts,'userdata',{tstretch,rnew,rnew2,t,method})
    case 'DTW'
        tr=dobj.tr;
        tshift=.02;
        blocksize=10;%(samples)
        answers=inputdlg({'max lag to search (sec)','lag constraint delta(samples)/sample'},...
            'DTW',1,{time2str(tshift),num2str(1/blocksize)});
        if(isempty(answers))
            return
        end
        tshift=str2double(answers{1});
        if(isnan(tshift) || tshift<0 || tshift>.4*(tr(end)-tr(1)))
            msgbox('bad value for correlation max lag');
            return;
        end
        invb=str2double(answers{2});
        if(isnan(invb) || invb<0 || invb>1)
            msgbox('bad value for lag constraint');
            return;
        end
        ind=near(t,tr(1),tr(end));
        b=round(1/invb);
        L=round(tshift/dt);
        %impose the seismic band
        w=waveseis(s,t);
        rsband=convz(rnew,w);
        [e,d,u]=DTW(s(ind),rsband(ind),L,b);
        delt=u*dt;
        delt2=[delt(1);delt;delt(end)];
        tdtw=[t(1);tr;t(end)];
        tstretch=interp1(tdtw,delt2,t);
        rnew2=stretcht(rnew,t,tstretch);
        i1=near(t,tr(1));
        i2=near(t,tr(end));
        tr_lims=tr_lims+[tstretch(i1) tstretch(i2)];
        dobj.rnew=rnew2;
        set(hundo,'userdata',{rnew, [tstretch(i1) tstretch(i2)]},'enable','on');
        set(hshowshifts,'userdata',{tstretch,rnew,rnew2,t,method})
end

urr{2}=tr_lims;
set(hrr,'userdata',urr);

plottraces(t,s,dobj.rnew)
setdataobject(dobj);
end

function undoauto(source,cbdata)
hundo=source;
udat=get(hundo,'userdata');
if(isempty(udat))
    return;
end
rnew=udat{1};
tr_adj=udat{2};
dobj=getdataobject;
hrr=findobj(gcf,'tag','returnref');
urr=get(hrr,'userdata');
tr_lims=urr{2};%these are the limit times of r. We may need to adjust them here
urr{2}=tr_lims-tr_adj;
dobj.rnew=rnew;
set(hrr,'userdata',urr);
set(hundo,'userdata',[],'enable','off')
plottraces(dobj.t,dobj.s,dobj.rnew)
setdataobject(dobj);

end

function showshifts(source,cbdata)
hshowshifts=source;
udat=get(hshowshifts,'userdata');
if(isempty(udat))
    return;
end
tshifts=udat{1};
rnew=udat{2};
rnew_shifted=udat{3};
t=udat{4};
method=udat{5};
figure
subplot(1,2,1)
plot(rnew,t,rnew_shifted,t);
gridy;flipy
ylabel('time (sec)');
legend('original','shifted')
title('reflectivity before and after')
subplot(1,2,2)
plot(tshifts,t);
xlabel('seconds')
set(gca,'yticklabel',[])
gridy;flipy
tm=max(abs(tshifts));
tm=.01*ceil(tm/.01);
xlim([-tm tm])
title(['applied shifts by ' method])
end

function plottraces(t,s,rnew)
hup=findobj(gcf,'tag','shiftup');
haxes=get(hup,'userdata');
axes(haxes(1));
href=findobj(gca,'tag','ref');
uu=get(href,'userdata');
[tg1,tg2]=getcorrelationgate;
rnew=rnew/max(abs(rnew));
s=s/max(abs(s));

hh=plot([s rnew+1],t);flipy
ylabel('time(s)')
grid
set(gca,'xtick',[]);
icc=near(t,tg1,tg2);
cc=sum(rnew(icc).*s(icc))/sqrt(sum(rnew(icc).^2)*sum(s(icc).^2));
title(['Zero lag cc =' num2str(cc)])

%restore anchors
if(~isempty(uu))
    tanch=uu{2};
    tmotion=uu{4};
    [ha,hm]=plotanchors(t,tanch,tmotion);
    uu{1}=ha;
    uu{3}=hm;
    set(hh(2),'userdata',uu);
end

%set context menus
hcntx=uicontextmenu;
uimenu(hcntx,'label','Drop anchor','callback',@setanchor);
uimenu(hcntx,'label','Clear anchor','callback',@clearanchor);
set(hh(2),'buttondownfcn',@refclick);
set(hh(2),'uicontextmenu',hcntx,'tag','ref');

%draw wavelet gate
xl=xlim;
hl=line(xl,[tg1 tg1],[1 1],'linestyle',':','color','g','buttondownfcn',...
    'align_ref(''dragline'')','tag','tg1','linewidth',2);
set(hl,'zdata',[10 10]);
hl=line(xl,[tg2 tg2],[1 1],'linestyle',':','color','g','buttondownfcn',...
    'align_ref(''dragline'')','tag','tg2','linewidth',2);
set(hl,'zdata',[10 10]);

dobj=getdataobject;
ccm=dobj.cc;
cc=abs(ccm(1));
nlags=min([100,length(icc)]);
mw=mwindow(length(icc));
%mw=gwindow(length(icc));
%ccf=ccorr(rnew(icc).*mw,s(icc).*mw,nlags);
ccf=ccorr(s(icc).*mw,rnew(icc).*mw,nlags);
ccfe=env(ccf);
% cce=ccorr(env(rnew),env(s),nlags);
tau=(t(2)-t(1))*(-nlags:nlags)';
axes(haxes(2))
plot(ccf,tau,ccfe,tau);
xmax=sqrt(2)*cc;
if(max(ccfe)>xmax)
    while xmax<max(ccfe)
        xmax=xmax+.5;
    end
end
xlim([-xmax xmax])
xlabel('cc value');
ylabel('lag time (s)');
title('ccorr (blue) and env (red)')
grid
end

function [tg1,tg2]=getcorrelationgate
dobj=getdataobject;
tg1=dobj.tg1;
tg2=dobj.tg2;

end

function dobj=getdataobject
hbut=findobj(gcf,'tag','shiftdown');
dobj=get(hbut,'userdata');
end

function dobj=setdataobject(dobj)
hbut=findobj(gcf,'tag','shiftdown');
set(hbut,'userdata',dobj);
end

function setanchor(source,callbackdata)
href=findobj(gcf,'tag','ref');
pt=get(gca,'currentpoint');
tclick=pt(1,2);
t=get(href,'ydata');
%rs=get(href,'xdata');
% iclick=near(t,tclick);
% hh=line(1,t(iclick(1)),'linestyle','none','marker','o','color','k','markersize',10);
ha=plotanchors(t,tclick);
uu=get(href,'userdata');
if(isempty(uu))
    uu{1}=ha;
    uu{2}=tclick;
    uu{3}=[];
    uu{4}=[];
else
    uu{1}=[uu{1} ha];
    uu{2}=[uu{2} tclick];
end
set(href,'userdata',uu)
end

function clearanchor(source,callbackdata)
href=findobj(gcf,'tag','ref');
pt=get(gca,'currentpoint');
tclick=pt(1,2);
uu=get(href,'userdata');
if(isempty(uu))
    return
end
tanch=uu{2};
hh=uu{1};
ind=near(tanch,tclick);
delete(hh(ind));
hh(ind)=[];
tanch(ind)=[];
uu={hh tanch};
set(href,'userdata',uu)
end

function refclick(source,callbackdata)
button=get(gcf,'selectiontype');
if(~strcmp(button,'normal'))
    return;
end
%set the motion point
href=findobj(gcf,'tag','ref');
%rs=get(href,'xdata');
t=get(href,'ydata');
pt=get(gca,'currentpoint');
tclick=pt(1,2);
uu=get(href,'userdata');
if(~isempty(uu{3}))
    delete(uu{3});
end
[ha,hm]=plotanchors(t,[],tclick);
uu{3}=hm;
uu{4}=tclick;
set(href,'userdata',uu);
end

function [ha,hm]=plotanchors(t,tanch,tmotion)
ha=zeros(1,length(tanch));
for k=1:length(ha)
    ianch=near(t,tanch(k));
    ha(k)=line(1,t(ianch),'linestyle','none','marker','o','color','k','markersize',10);
end
if(nargin>2)
    im=near(t,tmotion);
    hm=line(1,t(im),'linestyle','none','marker','s','color','k','markersize',18);
end
end

function returnref(source,callbackdata)
global ALIGN_REF_RNEW ALIGN_REF_TRNEW
uu=get(source,'userdata');
cb=uu{1};
hcallingfig=uu{3};
tr_lims=uu{2};
dobj=getdataobject;
t=dobj.t;
rnewp=dobj.rnew;
ind=near(t,tr_lims(1),tr_lims(2));
ALIGN_REF_TRNEW=t(ind);
ALIGN_REF_RNEW=rnewp(ind);
figure(hcallingfig)
eval(cb);
return;
end