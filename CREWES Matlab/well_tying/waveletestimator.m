function [wlet tw]=waveletestimator (trcs,t,sonic,density,z)
% WAVELETESTIMATOR is a gui interface that will allow the user to estimate
% a wavelet from the seismic data in the frequency domain
%
% [wlet tw]=waveletestimator(trcs,t)
%
% Variables In
%  trcs - traces from the seismic centered at the well location
%  t    - time vector that matches seismic
% Variables out
%  wlet - wavelet
%  tw   - time vector for wavelet.
%
% H.J.E. Lloyd, December 2012
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
if nargin < 2
    errordlg('All Variables must be present');
end

if nargin <3
    warndlg('Constant Phase Rotations Will not be Accurate');
    phsrotflg=1;
    [rcs,tlog]=reflec(max(t),t(2)-t(1),max(trcs(:)));
else
    if nargin <5
        errordlg('Depth is Needed');
    end
    if nargin <4
        errordlg('Density is Needed');
    end
    [wlet tw]=ricker(t(2)-t(1),30);
 [sgram,tlog,rcs,pm,p]=seismo(sonic,density,z,0,0,wlet,tw);
 phsrotflg=0;
end
%% initiate gui

%h.Ffig=figure('units','pixels','position',[150 150 1000 700],'menubar','none','name','Wavelet Estimator','NumberTitle','off');
h.Ffig=figure('units','pixels','position',[50 50 1100 700],'menubar','none','name','Wavelet Estimator','NumberTitle','off','CloseRequestFcn',@close);
%h.Ffig=figure('units','normalized','position',[0 0 .82 1],'name','Wavelet Estimator','NumberTitle','off','CloseRequestFcn',@close);
%set(h.Ffig,'DefaultaxesFontsize',16,'DefaultuicontrolFontsize',14,'DefaultuipanelFontsize',14);
set(h.Ffig,'DefaultaxesFontsize',12,'DefaultuicontrolFontsize',10,'DefaultuipanelFontsize',10);
% trace display and picking buttons
h.ATaxis=subplot('position',[.05 .200 .15 .7175]);
h.Btopz=uicontrol('units','normalized','position',[.05 .1125 .10 .05],'string','Top of Zone','style','pushbutton','callback',@pick,'userdata','top');
h.Bbotz=uicontrol('units','normalized','position',[.05 .0625 .10 .05],'string','Bottom of Zone','style','pushbutton','callback',@pick,'userdata','bot');
h.Ttopz=uicontrol('units','normalized','position',[.15 .1125 .05 .05],'string',num2str(t(1)),'style','text','callback',@plottrc);
h.Tbotz=uicontrol('units','normalized','position',[.15 .0625 .05 .05],'string',num2str(t(end)),'style','text','callback',@plottrc);
% display axis
h.A1axis=subplot('position',[.60 .6875 .35 .22]);
h.A2axis=subplot('position',[.60 .3750 .35 .22]);
h.A3axis=subplot('position',[.60 .0625 .35 .22]);
% type Panels
h.Gtype=uibuttongroup('visible','on','units','normalized','position',[.25 .325 .3 .6125],'selectionChangeFcn',@calcwav);
h.Rspln=uicontrol('units','normalized','position',[.05 .95 .9 .05],'string','Spline','style','radio','parent',h.Gtype,'fontweight','bold','ForegroundColor',[0 0 .75]);
h.Rpoly=uicontrol('units','normalized','position',[.05 .65 .9 .05],'string','Polynominal','style','radio','parent',h.Gtype,'fontweight','bold','ForegroundColor',[0 .5 0]);
h.Rplog=uicontrol('units','normalized','position',[.05 .3125 .9 .05],'string','Polynominal with Log10(f)','style','radio','parent',h.Gtype,'fontweight','bold','ForegroundColor',[.75 0 0]);
% spline commands
h.Tspl1=uicontrol('units','normalized','position',[.27 .85 .12 .0375],'string','Percentage of Points:','style','text');
h.Espl1=uicontrol('units','normalized','position',[.39 .85 .12 .0375],'string','2','style','edit','callback',@calcwav);
h.Rspl2=uicontrol('units','normalized','position',[.27 .8125 .12 .0375],'string','Fit to Amplitude','style','Radio','callback',@ampdb,'value',1);
h.Rspl3=uicontrol('units','normalized','position',[.27 .775 .12 .0375],'string','Fit to Decibels','style','Radio','callback',@ampdb,'value',0);
h.Cspl4=uicontrol('units','normalized','position',[.39 .8125 .12 .0375],'string','Fit upto Half Nyquist','style','Checkbox','callback',@calcwav);
% polynomial commands
h.Tpol1=uicontrol('units','normalized','position',[.27 .6625 .12 .0375],'string','Order:','style','text');
h.Ppol1=uicontrol('units','normalized','position',[.39 .6625 .12 .0375],'string',{'1','2','3','4','5','6','7','8'},'style','popupmenu','callback',@calcwav,'value',4);
h.Rpol2=uicontrol('units','normalized','position',[.27 .625 .12 .0375],'string','Fit to Amplitude','style','Radio','callback',@ampdb,'value',1);
h.Rpol3=uicontrol('units','normalized','position',[.27 .5875 .12 .0375],'string','Fit to Decibels','style','Radio','callback',@ampdb,'value',0);
h.Cpol4=uicontrol('units','normalized','position',[.39 .625 .12 .0375],'string','Flat before Min f','style','Checkbox','callback',@calcwav);
h.Cpol5=uicontrol('units','normalized','position',[.39 .5875 .12 .0375],'string','Flat after Max f','style','Checkbox','callback',@calcwav);
h.Cpol6=uicontrol('units','normalized','position',[.39 .55 .12 .0375],'string','Fit upto Half Nyquist','style','Checkbox','callback',@calcwav);

% Log10 x polymoninal commands
h.Tlog1=uicontrol('units','normalized','position',[.27 .4625 .12 .0375],'string','Order:','style','text');
h.Plog1=uicontrol('units','normalized','position',[.39 .4625 .12 .0375],'string',{'1','2','3','4','5','6','7','8'},'style','popupmenu','callback',@calcwav,'value',4);
h.Rlog2=uicontrol('units','normalized','position',[.27 .425 .12 .0375],'string','Fit to Amplitude','style','Radio','callback',@ampdb,'value',1);
h.Rlog3=uicontrol('units','normalized','position',[.27 .3875 .12 .0375],'string','Fit to Decibels','style','Radio','callback',@ampdb,'value',0);
h.Clog4=uicontrol('units','normalized','position',[.39 .425 .12 .0375],'string','Flat before Min f','style','Checkbox','callback',@calcwav);
h.Clog5=uicontrol('units','normalized','position',[.39 .3875 .12 .0375],'string','Flat after Max f','style','Checkbox','callback',@calcwav);
h.Clog6=uicontrol('units','normalized','position',[.39 .35 .12 .0375],'string','Fit upto Half Nyquist','style','Checkbox','callback',@calcwav);

%Phase commands
h.Gphas=uibuttongroup('visible','on','units','normalized','position',[.25 .14 .3 .15],'selectionChangeFcn',@calcwav,'Title','Phase Options');
h.Rpmin=uicontrol('units','normalized','position',[.05 .65 .60 .30],'string','Minimum Phase','style','radio','parent',h.Gphas);
h.Rpzer=uicontrol('units','normalized','position',[.05 .35 .60 .30],'string','Zero Phase','style','radio','parent',h.Gphas);
h.Rpcon=uicontrol('units','normalized','position',[.05 .05 .60 .30],'string','Constant Phase','style','radio','parent',h.Gphas);
h.Bfin=uicontrol('units','normalized','position',[.25 .0625 .3 .05],'string','Finished','style','pushbutton','callback',@close);
h.Epcon=uicontrol('units','normalized','position',[.38 .15 .04 .0375],'string','10','style','edit','callback',@calcwav);
h.Bpcon=uicontrol('units','normalized','position',[.42 .15 .06 .0375],'string','Calculate','style','pushbutton','callback',@calcphs);
h.Bacon=uicontrol('units','normalized','position',[.48 .15 .06 .0375],'string','Analyze','style','pushbutton','callback',@analyzephs);
if phsrotflg
    set(h.Bpcon,'backgroundcolor',[1 1 0],'tooltip','Calculated Phase Rotations Will Be Inaccurate');
end
% Variable Assignments
h.Vtrc=mean(trcs,2);
h.Vtrc=h.Vtrc./max(abs(h.Vtrc));
h.Vtrca=h.Vtrc;
h.Vt=t;
[h.VFtrc h.Vf]=fftrl(h.Vtrca,h.Vt);
h.Vflog=log10(h.Vf);h.Vflog(1)=log10(t(2));
h.Vtype='spl';
h.Vtopz=t(1);
h.Vbotz=t(end);
h.Vwlet=[];
h.Vtw=[];
h.VFwlet=[];
h.VFwspl=[];
h.Vwspl=[];
h.Vtwspl=[];
h.VFwpol=[];
h.Vwpol=[];
h.Vtwpol=[];
h.Vwlog=[];
h.Vtwlog=[];
h.VFwlog=[];
h.Vref=zeros(size(t));
h.Vref(1:near(t,tlog(end)))=rcs(1:near(t,tlog(end)));
h.Vref=h.Vref(:);
set(h.Ffig,'currentaxes',h.ATaxis);
flipy;
guidata(h.Ffig,h);
plottrc(h.Ffig,h);
uiwait(h.Ffig);
h=guidata(h.Ffig);
wlet=h.Vwlet;
tw=h.Vtw;
delete(h.Ffig);
end

% calculate the wavelets
function calcwav(hobj,u)
h=guidata(hobj);
% establish the phase
if get(h.Gphas,'selectedobject')==h.Rpmin
    phs='min';
elseif get(h.Gphas,'selectedobject')==h.Rpzer
    phs='zer';
else
    phs=get(h.Epcon,'string');
    phs2=str2double(phs);
    if isnan(phs2) || abs(phs2)>180
        errordlg('Phase must be a number between -180 and 180 degrees');
        return
    end
end

% create the wavelet using a spline
if get(h.Rspl2,'value');
    VFtrc=abs(h.VFtrc);
else
    VFtrc=todb(abs(h.VFtrc));
end
per=str2double(get(h.Espl1,'string'));
if isnan(per) || abs(per)>100
    errordlg(['Percentage of Points must be a number between ',num2str(1/length(h.Vf)),' and 100 with no percentage sign']);
    return
end

if get(h.Cspl4,'value');
    lenh=near(h.Vf,max(h.Vf)/2);
    if isempty(lenh) ||length(lenh)<2
        errordlg(['Percentage of Points must be a number between ',num2str(1/length(h.Vf)),' and 100 with no percentage sign']);
        return
    end
    ind=round(linspace(1,lenh,round(per*lenh)/100));
    h.VFwspl=ppval(spline(h.Vf(ind),VFtrc(ind)),h.Vf);
    
    
else
    %df=round(abs((100-per))*length(h.Vf)/100);
    %         ind=1:df:length(h.Vf);
    lenh=length(h.Vf);
    if isempty(lenh) ||lenh<2
        errordlg(['Percentage of Points must be a number between ',num2str(1/length(h.Vf)),' and 100 with no percentage sign']);
        return
    end
    ind=round(linspace(1,lenh,round(per*lenh/100)));
    h.VFwspl=ppval(spline(h.Vf(ind),VFtrc(ind)),h.Vf);
    if get(h.Rspl3,'value');
        h.VFwspl=max(abs(h.VFtrc))*10.^(h.VFwspl/20);
        %h.VFwspl=10.^(h.VFwspl/20);
    end
    % create the wavelet using a polynominal
    ord=get(h.Ppol1,'value');
    if get(h.Rpol2,'value');
        VFtrc=abs(h.VFtrc);
    else
        VFtrc=todb(abs(h.VFtrc));
    end
    if get(h.Cpol6,'value');
        ind=near(h.Vf,max(h.Vf)/2);
        pp=polyfit(h.Vf(1:ind),VFtrc(1:ind),ord);
    else
        pp=polyfit(h.Vf,VFtrc,ord);
    end
    h.VFwpol=polyval(pp,h.Vf);
    dwlet=h.VFwpol(2:end)-h.VFwpol(1:end-1);
    ppr=polyfit(h.Vf(1:end-1),dwlet,ord-1);
    pr=roots(ppr);
    pr=pr(pr>=h.Vf(1));
    pr=pr(pr<=h.Vf(end));
    if get(h.Cpol4,'value');
        ind=near(h.Vf,min(real(pr)));
        h.VFwpol(1:ind)=h.VFwpol(ind);
    end
    if get(h.Cpol5,'value');
        ind=near(h.Vf,max(real(pr)));
        h.VFwpol(ind:end)=h.VFwpol(ind);
    end
    
    if get(h.Rpol3,'value');
        h.VFwpol=max(abs(h.VFtrc))*10.^(h.VFwpol/20);
    end
    
    % create the wavelet using a polynominal with log10(f)
    ord=get(h.Plog1,'value');
    if get(h.Rlog2,'value');
        VFtrc=abs(h.VFtrc);
    else
        VFtrc=todb(abs(h.VFtrc));
    end
    if get(h.Clog6,'value');
        ind=near(h.Vf,max(h.Vf)/2);
        pp=polyfit(h.Vflog(1:ind),VFtrc(1:ind),ord);
    else
        pp=polyfit(h.Vflog,VFtrc,ord);
    end
    h.VFwlog=polyval(pp,h.Vflog);
    dwlet=h.VFwlog(2:end)-h.VFwlog(1:end-1);
    ppr=polyfit(h.Vflog(1:end-1),dwlet,ord-1);
    pr=roots(ppr);
    pr=pr(pr>=h.Vflog(1));
    pr=pr(pr<=h.Vflog(end));
    if get(h.Clog4,'value');
        ind=near(h.Vflog,min(real(pr)));
        h.VFwlog(1:ind)=h.VFwlog(ind);
    end
    if get(h.Clog5,'value');
        ind=near(h.Vflog,max(real(pr)));
        h.VFwlog(ind:end)=h.VFwlog(ind);
    end
    
    if get(h.Rlog3,'value');
        h.VFwlog=max(abs(h.VFtrc))*10.^(h.VFwlog/20);
    end
    
    [h.Vwspl,h.Vtwspl]=ifftrl(h.VFwspl,h.Vf);
    [h.Vwpol,h.Vtwpol]=ifftrl(h.VFwpol,h.Vf);
    [h.Vwlog,h.Vtwlog]=ifftrl(h.VFwlog,h.Vf);
    h.Vwspl=ifftshift(h.Vwspl);
    h.Vwpol=ifftshift(h.Vwpol);
    h.Vwlog=ifftshift(h.Vwlog);
    
    if strcmp(phs,'min')
        [h.Vwspl]=tomin(h.Vwspl);
        [h.Vwpol]=tomin(h.Vwpol);
        [h.Vwlog]=tomin(h.Vwlog);

        
    elseif strcmp(phs,'zer')
        h.Vtwspl=(-1*max(h.Vtwspl)/2):h.Vt(2):(max(h.Vtwspl)/2);
        h.Vtwpol=(-1*max(h.Vtwpol)/2):h.Vt(2):(max(h.Vtwpol)/2);
        h.Vtwlog=(-1*max(h.Vtwlog)/2):h.Vt(2):(max(h.Vtwlog)/2);
    
    else % phase rotate each wavelet
        phs=str2double(phs);
        [h.Vwspl]=phsrot(h.Vwspl,phs);
        h.Vtwspl=(-1*max(h.Vtwspl)/2):h.Vt(2):(max(h.Vtwspl)/2);

        [h.Vwpol]=phsrot(h.Vwpol,phs);
        h.Vtwpol=(-1*max(h.Vtwpol)/2):h.Vt(2):(max(h.Vtwpol)/2);
        
        [h.Vwlog]=phsrot(h.Vwlog,phs);
        h.Vtwlog=(-1*max(h.Vtwlog)/2):h.Vt(2):(max(h.Vtwlog)/2);
    end
    
    if get(h.Gtype,'selectedobject')==h.Rspln
        h.Vwlet=h.Vwspl;
        h.Vtw=h.Vtwspl;
        h.VFwlet=h.VFwspl;
    elseif get(h.Gtype,'selectedobject')==h.Rpoly
        h.Vwlet= h.Vwpol;
        h.Vtw=h.Vtwpol;
        h.VFwlet=h.VFwpol;
    elseif get(h.Gtype,'selectedobject')==h.Rplog
        h.Vwlet=h.Vwlog;
        h.Vtw=h.Vtwlog;
        h.VFwlet=h.VFwlog;
    end
    
    
    guidata(hobj,h);
    plotwav(hobj,u);
end
end

%plot the traces
function plottrc(hobj,u)
h=guidata(hobj);
indt=near(h.Vt,h.Vtopz);
indb=near(h.Vt,h.Vbotz);
if indt>indb
    ind=indt;
    indt=indb;
    indb=ind;
end
w = mwindow(indt:indb);
win=zeros(size(h.Vt(:)));
win(indt:indb)=w;
win=win(:);
h.Vtrca=win.*h.Vtrc;
[h.VFtrc h.Vf]=fftrl(h.Vtrca,h.Vt);
set(h.Ffig,'currentaxes',h.ATaxis);
cla;hold on;
fill([-1 1 1 -1 -1],h.Vt([indt,indt,indb,indb,indt]),[1 1 .5]);
wtva(h.Vtrc,h.Vt,'k');
title('Trace with Highlighted Zone');
set(gca,'xlim',[-1 1]);
guidata(hobj,h);
calcwav(hobj,u);
end

% plot wavelets
function plotwav(hobj,u)
h=guidata(hobj);
if get(h.Gtype,'selectedobject')==h.Rplog
    plotwavlog(hobj,u);
    return
end

lwspl=':';
lwpol=':';
lwlog=':';
if get(h.Gtype,'selectedobject')==h.Rspln
    lwspl='-';
elseif get(h.Gtype,'selectedobject')==h.Rpoly
    lwpol='-';
elseif get(h.Gtype,'selectedobject')==h.Rplog
    lwlog='-';
end

lw=2.25;
set(h.Ffig,'currentaxes',h.A1axis);
cla;hold on;
plot(h.Vf,abs(h.VFtrc),'color',[1 .6 .8]);
plot(h.Vf,abs(h.VFwspl),lwspl,'color',[0 0 .75],'linewidth',lw)
plot(h.Vf,abs(h.VFwpol),lwpol,'color',[0 .5 0],'linewidth',lw)
plot(h.Vf,abs(h.VFwlog),lwlog,'color',[.75 0 0],'linewidth',lw)
xlabel('Frequency (Hz)');
ylabel('Amplitude');
title ('Amplitude Spectra');
legend('Seismic','Spline Estimate','Poly Estimate','Log_1_0 (f)Estimate','location','northeast');
x=round(linspace(h.Vf(1),h.Vf(end),6))';
set(gca,'xtick',x);
set(gca,'xticklabel',num2str(x(:)))
set(gca,'xlim',[x(1) x(end)]);

set(h.Ffig,'currentaxes',h.A2axis);
cla;hold on;
plot(h.Vf,todb(abs(h.VFtrc)),'color',[1 .6 .8]);
plot(h.Vf,todb(abs(h.VFwspl),max(abs(h.VFtrc))),lwspl,'color',[0 0 .75],'linewidth',lw)
plot(h.Vf,todb(abs(h.VFwpol),max(abs(h.VFtrc))),lwpol,'color',[0 .5 0],'linewidth',lw)
plot(h.Vf,todb(abs(h.VFwlog),max(abs(h.VFtrc))),lwlog,'color',[.75 0 0],'linewidth',lw)
xlabel('Frequency (Hz)');
ylabel('Decibels');
title ('Amplitude Spectra in Decibels');
x=round(linspace(h.Vf(1),h.Vf(end),6))';
set(gca,'xtick',x);
set(gca,'xticklabel',num2str(x(:)))
set(gca,'xlim',[x(1) x(end)]);


% get xlim
if get(h.Gphas,'selectedobject')==h.Rpmin
    indc=(find(cumsum(h.Vwlet.^2)>.9999*max(cumsum(h.Vwlet.^2)),1))+20;
    xmin=0;
    xmax=h.Vtw(indc);
else
    indtt=find(cumsum(h.Vwlet.^2)>.9999*max(cumsum(h.Vwlet.^2)),1);
        if indtt+20>length(h.Vtw)
            indtt=length(h.Vtw);
        else
            indtt=indtt+20;
        end
        indt=h.Vtw(indtt);
        xmin=h.Vtw(near(h.Vtw,-1*indt));
        xmax=h.Vtw(near(h.Vtw,indt));
end

set(h.Ffig,'currentaxes',h.A3axis);
cla;hold on;
plot(h.Vtwspl,h.Vwspl,lwspl,'color',[0 0 .75],'linewidth',2)
plot(h.Vtwpol,h.Vwpol,lwpol,'color',[0 .5 0],'linewidth',2)
plot(h.Vtwlog,h.Vwlog,lwlog,'color',[.75 0 0],'linewidth',2)
xlabel('Time (s)');
ylabel('Amplitude');
title('Wavelet in Time');
set(gca,'xlim',[xmin xmax]);
guidata(hobj,h);
end

% plot the wavelets on a log scale
function plotwavlog(hobj,u)
h=guidata(hobj);

lwspl=':';
lwpol=':';
lwlog=':';
if get(h.Gtype,'selectedobject')==h.Rspln
    lwspl='-';
elseif get(h.Gtype,'selectedobject')==h.Rpoly
    lwpol='-';
elseif get(h.Gtype,'selectedobject')==h.Rplog
    lwlog='-';
end

lw=2.25;
nyq=h.Vf(end);
set(h.Ffig,'currentaxes',h.A1axis);
cla;hold on;
plot(h.Vflog,abs(h.VFtrc),'color',[1 .6 .8]);
plot(h.Vflog,abs(h.VFwspl),lwspl,'color',[0 0 .75],'linewidth',lw)
plot(h.Vflog,abs(h.VFwpol),lwpol,'color',[0 .5 0],'linewidth',lw)
plot(h.Vflog,abs(h.VFwlog),lwlog,'color',[.75 0 0],'linewidth',lw)
xlabel('Frequency (Hz)');
ylabel('Amplitude');
x=[.1 1 10 .25*nyq nyq]';
set(gca,'xtick',log10(x));
set(gca,'xticklabel',num2str(round(x(:))))
set(gca,'xlim',[log10(x(1)) log10(x(end))]);
legend('Seismic','Spline Estimate','Poly Estimate','Log_1_0 (f)Estimate','location','northwest');

set(h.Ffig,'currentaxes',h.A2axis);
cla;hold on;
plot(h.Vflog,todb(abs(h.VFtrc)),'color',[1 .6 .8]);
plot(h.Vflog,todb(abs(h.VFwspl),max(abs(h.VFtrc))),lwspl,'color',[0 0 .75],'linewidth',lw)
plot(h.Vflog,todb(abs(h.VFwpol),max(abs(h.VFtrc))),lwpol,'color',[0 .5 0],'linewidth',lw)
plot(h.Vflog,todb(abs(h.VFwlog),max(abs(h.VFtrc))),lwlog,'color',[.75 0 0],'linewidth',lw)
xlabel('Frequency (Hz)');
ylabel('Decibels');
x=[.1 1 10 .25*nyq nyq]';
set(gca,'xtick',log10(x));
set(gca,'xticklabel',num2str(round(x(:))))
set(gca,'xlim',[log10(x(1)) log10(x(end))]);

% get xlim
if get(h.Gphas,'selectedobject')==h.Rpmin
    indc=(find(cumsum(h.Vwlet.^2)>.9999*max(cumsum(h.Vwlet.^2)),1))+20;
    xmin=0;
    xmax=h.Vtw(indc);
else
    indtt=find(cumsum(h.Vwlet.^2)>.9999*max(cumsum(h.Vwlet.^2)),1);
        if indtt+20>length(h.Vtw)
            indtt=length(h.Vtw);
        else
            indtt=indtt+20;
        end
        indt=h.Vtw(indtt);
        xmin=h.Vtw(near(h.Vtw,-1*indt));
        xmax=h.Vtw(near(h.Vtw,indt));
end
set(h.Ffig,'currentaxes',h.A3axis);
cla;hold on;
plot(h.Vtwspl,h.Vwspl,lwspl,'color',[0 0 .75],'linewidth',2)
plot(h.Vtwpol,h.Vwpol,lwpol,'color',[0 .5 0],'linewidth',2)
plot(h.Vtwlog,h.Vwlog,lwlog,'color',[.75 0 0],'linewidth',2)
xlabel('Time (s)');
ylabel('Amplitude');
title('Wavelet in Time');
set(gca,'xlim',[xmin, xmax]);
guidata(hobj,h);
end

% intialize pick the current point for the zone
function pick(hobj,u)
h=guidata(hobj);
if strcmp(get(hobj,'userdata'),'top')
    set(h.Btopz,'string','Finished Pick','callback',@fpick);
    set(h.Bbotz,'enable','off');
    set(h.ATaxis,'userdata',1);
end
if strcmp(get(hobj,'userdata'),'bot')
    set(h.Bbotz,'string','Finished Pick','callback',@fpick);
    set(h.Btopz,'enable','off');
    set(h.ATaxis,'userdata',2);
end
set(h.Ffig,'windowbuttondownfcn',@ppick,'pointer','fullcross');
guidata(hobj,h);
end

% Complete pick the current point for the zone
function ppick(hobj,u)
h=guidata(hobj);
if h.ATaxis==get(h.Ffig,'currentaxes')
    pp=get(h.ATaxis,'currentpoint');
    if get(h.ATaxis,'userdata')==1;
        pic=pp(1,2);
        h.Vtopz=h.Vt(near(h.Vt,pic));
        set(h.Ttopz,'string',num2str(h.Vtopz));
    end
    if get(h.ATaxis,'userdata')==2;
        pic=pp(1,2);
        h.Vbotz=h.Vt(near(h.Vt,pic));
        set(h.Tbotz,'string',num2str(h.Vbotz));
    end
end
guidata(hobj,h);
plottrc(hobj,h);
end

% switch between amplitude and decibel options
function ampdb(hobj,u)
h=guidata(hobj);
if hobj==h.Rpol2
    set(h.Rpol3,'value',0);
elseif hobj==h.Rpol3
    set(h.Rpol2,'value',0);
elseif hobj==h.Rlog2
    set(h.Rlog3,'value',0);
elseif hobj==h.Rlog3
    set(h.Rlog2,'value',0);
elseif hobj==h.Rspl2
    set(h.Rspl3,'value',0);
elseif hobj==h.Rspl3
    set(h.Rspl2,'value',0);
end;
guidata(hobj,h);
calcwav(hobj,u);
end

% pick the top and bottom of zone
function fpick(hobj,u)
h=guidata(hobj);

if strcmp(get(hobj,'userdata'),'top')
    set(h.Btopz,'string','Top of Zone','callback',@pick)
    set(h.Bbotz,'enable','on');
    set(h.ATaxis,'userdata',1);
end
if strcmp(get(hobj,'userdata'),'bot')
    set(h.Bbotz,'string','Bottom of Zone','callback',@pick)
    set(h.Btopz,'enable','on');
    set(h.ATaxis,'userdata',2);
end

set(h.ATaxis,'userdata',[]);
set(h.Ffig,'windowbuttondownfcn',[],'pointer','arrow');
guidata(hobj,h);
end

function calcphs(hobj,h)
h=guidata(hobj);
% determine current wavelet and create zero phase
if get(h.Gtype,'selectedobject')==h.Rspln
    [wave,tw]=ifftrl(h.VFwspl,h.Vf);
elseif get(h.Gtype,'selectedobject')==h.Rpoly
    [wave,tw]=ifftrl(h.VFwpol,h.Vf);
elseif get(h.Gtype,'selectedobject')==h.Rplog
    [wave,tw]=ifftrl(h.VFwlog,h.Vf);
end
wave=ifftshift(wave);

syn=convz(h.Vref,wave);

if length(syn)~=length(h.Vtrca);
    syn=pad_trace(syn(:),h.Vtrca(:));
end
phi=constphase(syn(:),h.Vtrca(:));

set(h.Epcon,'string',num2str(round(phi)));
guidata(hobj,h);
end

function analyzephs(hobj,h)
h=guidata(hobj);
% determine current wavelet and create zero phase
if get(h.Gtype,'selectedobject')==h.Rspln
    [wave,tw]=ifftrl(h.VFwspl,h.Vf);
elseif get(h.Gtype,'selectedobject')==h.Rpoly
    [wave,tw]=ifftrl(h.VFwpol,h.Vf);
elseif get(h.Gtype,'selectedobject')==h.Rplog
    [wave,tw]=ifftrl(h.VFwlog,h.Vf);
end
wave=ifftshift(wave);
tw=(-1*max(tw)/2):h.Vt(2):(max(tw)/2);
indt=near(h.Vt,h.Vtopz);
indb=near(h.Vt,h.Vbotz);

phi=constphsanalyze(h.Vtrca,h.Vref,h.Vt,wave,tw,[h.Vt(indt),h.Vt(indb)]);
set(h.Epcon,'string',num2str(round(phi)));
guidata(hobj,h);
end

function close(hobj,u)
uiresume;
end