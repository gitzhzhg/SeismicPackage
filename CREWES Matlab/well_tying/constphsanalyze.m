function phs=constphsanalyze(trc,r,t,wlet,tw,lim)
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


if nargin<6
    lim=[min(t),max(t)];
end

%h.Ffig=figure('units','pixels','position',[150 150 1000 700],'menubar','none','name','Wavelet Estimator','NumberTitle','off');
h.Ffig=figure('units','pixels','position',[50 50 900 700],'menubar','none','name','Wavelet Estimator','NumberTitle','off','CloseRequestFcn',@close);
%h.Ffig=figure('units','normalized','position',[0 0 .82 1],'name','Wavelet Estimator','NumberTitle','off','CloseRequestFcn',@close);
%set(h.Ffig,'DefaultaxesFontsize',16,'DefaultuicontrolFontsize',14,'DefaultuipanelFontsize',14);
set(h.Ffig,'DefaultaxesFontsize',12,'DefaultuicontrolFontsize',10,'DefaultuipanelFontsize',10);
% trace display and picking buttons
h.ATaxis=subplot('position',[.075 .15 .30 .80]);
h.Bptsyn=uicontrol('units','normalized','position',[.075 .05 .30 .05],'string','Update Synthetic','style','pushbutton','callback',@plottrc);
% display axis
h.A1axis=subplot('position',[.50 .6500 .40 .30]);
h.A2axis=subplot('position',[.50 .2250 .40 .325]);
% Phase slider and buttons
h.Sphs=uicontrol('units','normalized','position',[.500 .10 .40 .05],'string','','style','slider','callback',@pickphs,'max',180,'min',-180,'sliderstep',[1/361,10/361],'value',0);
h.Tphs=uicontrol('units','normalized','position',[.500 .05 .20 .05],'string','0','style','text');
h.Bphs=uicontrol('units','normalized','position',[.700 .05 .20 .05],'string','Choose Phase','style','pushbutton','callback',@close);


h.Vtrc=trc(:);
h.Vrcs=r(:);
h.Vrcsp=phsrot(r(:),90);
h.Vsgc=convz(r(:),wlet);
h.Vsgz=convz(r(:),wlet);
h.Vsgzp=phsrot(h.Vsgz(:),90);
h.Vt=t(:);
h.Vwletz=wlet(:);
h.Vwletc=wlet(:);
h.Vwletzp=phsrot(wlet(:),90);
h.Vtw=tw(:);
h.Vphs=0;
h.Vlim=lim;

h.allphs=-180:180;
h.maxcor=zeros(size(h.allphs));
h.maxcorlag=zeros(size(h.allphs));
indt=near(h.Vt,h.Vlim(1));
indb=near(h.Vt,h.Vlim(2));
if indt>indb
    ind=indt;
    indt=indb;
    indb=ind;
end
w = mwindow(indt:indb);
win=zeros(size(h.Vt(:)));
win(indt:indb)=w;
win=win(:);
for k=1:length(h.allphs)
    stheta=h.Vsgz*cosd(h.allphs(k))+h.Vsgzp*sind(h.allphs(k));
    c=xcorr(win.*h.Vtrc,win.*stheta);
    h.maxcor(k)=c(round(length(c)/2)+1);
    zmk=round(length(c)/2)+1;
    h.maxcorlag(k)=max(c(-25+zmk:25+zmk));
%     x=maxcorr(win.*h.Vtrc,win.*stheta,25);
%     h.maxcor(k)=x(1);
%     h.maxcorlag(k)=x(2);
end
set(h.Ffig,'currentaxes',h.A2axis);
plot(h.allphs,h.maxcor,'k',h.allphs,h.maxcorlag,'r'); hold on;
h.Lphs=plot([h.Vphs,h.Vphs],get(gca,'ylim'),'b');
xlabel('Phase (\o)');
title('Corrlation at 0 lag');
set(gca,'xlim',[-180 180]);
guidata(h.Ffig,h);
plottrc(h.Ffig,[]);
pickphs(h.Ffig,[]);
set(h.Ffig,'currentaxes',h.ATaxis);
flipy;
uiwait(h.Ffig);



h=guidata(h.Ffig);
phs=h.Vphs;
delete(h.Ffig);
end


function plottrc(hobj,u)
h=guidata(hobj);
indt=near(h.Vt,h.Vlim(1));
indb=near(h.Vt,h.Vlim(2));
if indt>indb
    ind=indt;
    indt=indb;
    indb=ind;
end
set(h.Ffig,'currentaxes',h.ATaxis);
cla;
h.Vsgc=balans(h.Vsgc,h.Vtrc,indt:indb);
mv=max([h.Vtrc(indt:indb);h.Vsgc(indt:indb)]);
wtva(h.Vtrc/mv,h.Vt,'k'); hold on;
wtva(h.Vsgc/mv+1,h.Vt,'r');
title('Trace and Synthetic');
ylabel('Time (s)');
set(gca,'xlim',[-1 2],'ylim',[h.Vt(indt),h.Vt(indb)]);
guidata(hobj,h);
end

function pickphs(hobj,u)
h=guidata(hobj);
h.Vphs=round(get(h.Sphs,'Value'));
set(h.Tphs,'string',num2str(h.Vphs));
h.Vsgc=h.Vsgz*cosd(h.Vphs)+h.Vsgzp*sind(h.Vphs);
h.Vwletc=h.Vwletz*cosd(h.Vphs)+h.Vwletzp*sind(h.Vphs);
set(h.Ffig,'currentaxes',h.A1axis);
plot(h.Vtw,h.Vwletc,'k');
title(['Wavelet Phase=',num2str(h.Vphs)]);
xlabel('Time (s)');
% get wavlet limits
cs=cumsum(abs(h.Vwletc));
tlim=h.Vtw(near(cs,max(abs(cs))*.95))+.1;
set(gca,'xlim',[-1*tlim,tlim]);
set(h.Lphs,'xdata',[h.Vphs,h.Vphs]);
guidata(hobj,h);
end


function close(hobj,u)
uiresume;
end