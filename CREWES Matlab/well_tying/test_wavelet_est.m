%estimate wavelets
%min phase synthetic
s2n=3;
dt=.002;
fdom=30;
tlen=.3;
[w,tw]=wavemin(dt,fdom,tlen);
[r,t]=reflec(2,dt,.1,3,pi);
s=convm(r,w);
if(~isinf(s2n))
    sn=s+rnoise(s,s2n);
end
jcausal=1;
%% ricker synthetic
s2n=3;
dt=.002;
fdom=30;
tlen=.3;
[w,tw]=ricker(dt,fdom,tlen);
w=phsrot(w,37);
[r,t]=reflec(2,dt,.1,3,pi);
s=convz(r,w);
if(~isinf(s2n))
    sn=s+rnoise(s,s2n);
end
jcausal=0;
%% klauder synthetic
s2n=3;
dt=.002;
fmin=10;
fmax=80;
slen=8;
tlen=.3;
taper=.25;
[w,tw]=wavevib(fmin,fmax,dt,slen,tlen,taper);
[r,t]=reflec(2,dt,.2,3,pi);
s=convz(r,w);
if(~isinf(s2n))
    sn=s+rnoise(s,s2n);
end
jcausal=0;
%% decon
top=.1;
nop=round(top/dt);
stab=.0001;
s=deconw(s,s,nop,stab);
sn=deconw(sn,sn,nop,stab);
%% simple

t1=.7;t2=1.2;%esimation time window
wsize=.4;%a fraction of window size
delf=5;%frequency domain smoother

[w1,tw1,stat1,phs1]=extract_wavelets_simple(s,t,r,.5*(t1+t2),t2-t1,delf,wsize);%no noise
[w1n,tw1n,stat1n,phs1n]=extract_wavelets_simple(sn,t,r,.5*(t1+t2),t2-t1,delf,wsize);%noisy

%construct model traces
sm=convz(s,w1{1});
snm=convz(sn,w1n{1});


% w1=w1{1};
% tw1=tw1{1};
% w1n=w1n{1};

pep1=penpred(s,r,w1{1},tw1{1});

pep1n=penpred(s,r,w1n{1},tw1{1});


figure
subplottop
names={'reflectivity','no noise','no noise model',['s2n= ' num2str(s2n)],'noisy model'};
trplot(t,[r,s,sm,sn,snm],'normalize',1,'tracespacing',1.5,'names',names);
set(gca,'xlim',[0 max(t)+.2])
yl=get(gca,'ylim');
line([t1 t1],yl,'color',[0 .8 0],'linestyle',':','linewidth',1);
line([t2 t2],yl,'color',[0 .8 0],'linestyle',':','linewidth',1);
title('wavelet estimation with smoothness-constrained match filter,  green lines show extraction window')

subplotbota
[err1,cc1]=waveleterr(w,tw,w1{1},tw1{1});

[errn,ccn]=waveleterr(w,tw,w1n{1},tw1{1});

it=near(tw,tw(1),tw(end));
names={'true',...
    ['no noise estimate, delf=' num2str(delf) ', err=' num2str(err1), ...
    ', cc(1)=' num2str(sigfig(cc1(1),2)) ', cc(2)=' num2str(cc1(2)) ', pep=' num2str(pep1,2)],...
    ['noisy estimate, delf=' num2str(delf) ', err=' num2str(errn), ...
    ', cc(1)=' num2str(sigfig(ccn(1),2)) ', cc(2)=' num2str(ccn(2)) ', pep=' num2str(pep1n,2)]};
names2={'',['static= ' num2str(stat1), ', phase=' num2str(phs1)],['static= ' num2str(stat1n), ', phase=' num2str(phs1n)]};
hh=trplot({tw(it) tw1{1} tw1{1}},{w(it),w1{1},w1n{1},},'zerolines','y','normalize',0,'names',names2);
legend(cell2mat(hh),names)
title('estimated wavelets')

subplotbotb
iw=near(t,t1,t2);
if(jcausal)
    wf=[2 ones(1,3)];
else
    wf=ones(1,4);
end
dbspec({tw(it) tw1{1} tw1{1},t(iw)},{w(it),w1{1},w1n{1},s(iw)},'windowflags',wf,'normoption',1);
ylim([-100,0]);
%legend(cell2mat(hh),names)
title('estimated wavelets spectra and trace spectrum')


prepfiga
%% match filter
icausal=10;
t1=.7;t2=1.2;%esimation time window
wsize=.5;%a fraction of window size
mu=1;
% [w1,tw1]=waveest_match(s,t,r,t,t1,t2,wlen,mu);
% [w1n,tw1n]=waveest_match(sn,t,r,t,t1,t2,wlen,mu);
%wlen2=wlen/(t2-t1);
[w1,tw1]=extract_wavelets_match(s,t,r,.5*(t1+t2),t2-t1,wsize,mu,icausal);%no noise smooth
w1a=extract_wavelets_match(s,t,r,.5*(t1+t2),t2-t1,wsize,0,icausal);%no noise not smooth
w1n=extract_wavelets_match(sn,t,r,.5*(t1+t2),t2-t1,wsize,mu,icausal);%noisy smooth
w1na=extract_wavelets_match(sn,t,r,.5*(t1+t2),t2-t1,wsize,0,icausal);%noisy not smooth

pep1=penpred(s,r,w1{1},tw1{1});
pep1a=penpred(s,r,w1a{1},tw1{1});
pep1n=penpred(s,r,w1n{1},tw1{1});
pep1na=penpred(s,r,w1na{1},tw1{1});

%model traces
izero=near(tw1{1},0);
sm=convz(s,w1{1},izero);
snm=convz(sn,w1n{1},izero);

% sm=convm(s,w1{1});
% snm=convm(sn,w1n{1});

% w1n2=w1n2{1};
% tw1n2=tw1n2{1};
figure
subplottop
names={'reflectivity','no noise','model trace',['s2n= ' num2str(s2n)],'model trace'};
trplot(t,[r,s,sm,sn,snm],'normalize',1,'tracespacing',1.5,'names',names);
set(gca,'ylim',[-1 7])
set(gca,'xlim',[0 max(t)+.2])
yl=get(gca,'ylim');
line([t1 t1],yl,'color',[0 .8 0],'linestyle',':','linewidth',1);
line([t2 t2],yl,'color',[0 .8 0],'linestyle',':','linewidth',1);
title('wavelet estimation with smoothness-constrained match filter,  green lines show extraction window')

subplotbota
[err1,cc1]=waveleterr(w,tw,w1{1},tw1{1});
[err1a,cc1a]=waveleterr(w,tw,w1a{1},tw1{1});
[errn,ccn]=waveleterr(w,tw,w1n{1},tw1{1});
[errna,ccna]=waveleterr(w,tw,w1na{1},tw1{1});
it=near(tw,tw(1),tw(end));
names={'true',...
    ['no noise estimate, mu=' num2str(mu) ', err=' num2str(err1), ...
    ', cc(1)=' num2str(sigfig(cc1(1),2)) ', cc(2)=' num2str(cc1(2)) ', pep=' num2str(pep1,2)],...
    ['no noise estimate, mu=' num2str(0) ', err=' num2str(err1a), ...
    ', cc(1)=' num2str(sigfig(cc1a(1),2)) ', cc(2)=' num2str(cc1a(2)) ', pep=' num2str(pep1a,2)],...
    ['noisy estimate, mu=' num2str(mu) ', err=' num2str(errn), ...
    ', cc(1)=' num2str(sigfig(ccn(1),2)) ', cc(2)=' num2str(ccn(2)) ', pep=' num2str(pep1n,2)],...
   ['noisy estimate, mu=' num2str(0) ', err=' num2str(errna), ...
    ', cc(1)=' num2str(sigfig(ccna(1),2)) ', cc(2)=' num2str(ccna(2)) ', pep=' num2str(pep1na,2)]};
hh=trplot({tw(it) tw1{1} tw1{1} tw1{1} tw1{1}},{w(it),w1{1},w1a{1},w1n{1},w1na{1}},'zerolines','y','normalize',0);
legend(cell2mat(hh),names)
title('estimated wavelets')

subplotbotb
if(jcausal)
    if(icausal==0)
        wf=[2 ones(1,4)];
    else
        wf=2*ones(1,5);
    end
else
    if(icausal==0)
        wf=ones(1,5);
    else
       wf=[1 2*ones(1,4)]; 
    end
end
hh=dbspec({tw(it) tw1{1} tw1{1} tw1{1} tw1{1}},{w(it),w1{1},w1a{1},w1n{1},w1na{1}},'windowflags',wf,'normoption',1);
ylim([-100,0]);
%legend(cell2mat(hh),names)
title('estimated wavelets spectra')


prepfiga
%% Roy White time domain
t1=.7;t2=1.2;%esimation time window
wsize=.5;%a fraction of window size
mu=1;
% [w1,tw1]=waveest_match(s,t,r,t,t1,t2,wlen,mu);
% [w1n,tw1n]=waveest_match(sn,t,r,t,t1,t2,wlen,mu);
%wlen2=wlen/(t2-t1);
[w1,tw1]=extract_wavelets_roywhite(s,t,r,.5*(t1+t2),t2-t1,wsize,mu);%no noise smooth
w1a=extract_wavelets_roywhite(s,t,r,.5*(t1+t2),t2-t1,wsize,0);%no noise not smooth
w1n=extract_wavelets_roywhite(sn,t,r,.5*(t1+t2),t2-t1,wsize,mu);%noisy smooth
w1na=extract_wavelets_roywhite(sn,t,r,.5*(t1+t2),t2-t1,wsize,0);%noisy not smooth

pep1=penpred(s,r,w1{1},tw1{1});
pep1a=penpred(s,r,w1a{1},tw1{1});
pep1n=penpred(s,r,w1n{1},tw1{1});
pep1na=penpred(s,r,w1na{1},tw1{1});

% w1n2=w1n2{1};
% tw1n2=tw1n2{1};
figure
subplottop
names={'reflectivity','no noise',['s2n= ' num2str(s2n)]};
trplot(t,[r,s,sn],'normalize',1,'tracespacing',1.5,'names',names);
set(gca,'xlim',[0 max(t)+.2])
yl=get(gca,'ylim');
line([t1 t1],yl,'color',[0 .8 0],'linestyle',':','linewidth',1);
line([t2 t2],yl,'color',[0 .8 0],'linestyle',':','linewidth',1);
title('wavelet estimation with Roy White''s method (time-domain), green lines show extraction window')

subplotbota
[err1,cc1]=waveleterr(w,tw,w1{1},tw1{1});
[err1a,cc1a]=waveleterr(w,tw,w1a{1},tw1{1});
[errn,ccn]=waveleterr(w,tw,w1n{1},tw1{1});
[errna,ccna]=waveleterr(w,tw,w1na{1},tw1{1});
it=near(tw,tw(1),tw(end));
names={'true',...
    ['no noise estimate, mu=' num2str(mu) ', err=' num2str(err1), ...
    ', cc(1)=' num2str(sigfig(cc1(1),2)) ', cc(2)=' num2str(cc1(2)) ', pep=' num2str(pep1,2)],...
    ['no noise estimate, mu=' num2str(0) ', err=' num2str(err1a), ...
    ', cc(1)=' num2str(sigfig(cc1a(1),2)) ', cc(2)=' num2str(cc1a(2)) ', pep=' num2str(pep1a,2)],...
    ['noisy estimate, mu=' num2str(mu) ', err=' num2str(errn), ...
    ', cc(1)=' num2str(sigfig(ccn(1),2)) ', cc(2)=' num2str(ccn(2)) ', pep=' num2str(pep1n,2)],...
   ['noisy estimate, mu=' num2str(0) ', err=' num2str(errna), ...
    ', cc(1)=' num2str(sigfig(ccna(1),2)) ', cc(2)=' num2str(ccna(2)) ', pep=' num2str(pep1na,2)]};
hh=trplot({tw(it) tw1{1} tw1{1} tw1{1} tw1{1}},{w(it),w1{1},w1a{1},w1n{1},w1na{1}},'zerolines','y','normalize',0);
legend(cell2mat(hh),names)
title('estimated wavelets')

subplotbotb
if(jcausal)
    wf=[2 ones(1,4)];
else
    wf=ones(1,5);
end
hh=dbspec({tw(it) tw1{1} tw1{1} tw1{1} tw1{1}},{w(it),w1{1},w1a{1},w1n{1},w1na{1}},'windowflags',wf,'normoption',1);
ylim([-100 0])
%legend(cell2mat(hh),names)
title('estimated wavelets spectra')


prepfiga

%% Roy White frequency domain
t1=.7;t2=1.2;%esimation time window
wsize=.5;%a fraction of window size
mu=1;
stab=.01;
method='two';
fsmo=5;
% [w1,tw1]=waveest_match(s,t,r,t,t1,t2,wlen,mu);
% [w1n,tw1n]=waveest_match(sn,t,r,t,t1,t2,wlen,mu);
%wlen2=wlen/(t2-t1);
[w1,tw1]=extract_wavelets_roywhite(s,t,r,.5*(t1+t2),t2-t1,wsize,mu,stab,fsmo,method);%no noise smooth
w1a=extract_wavelets_roywhite(s,t,r,.5*(t1+t2),t2-t1,wsize,0,stab,fsmo,method);%no noise not smooth
w1n=extract_wavelets_roywhite(sn,t,r,.5*(t1+t2),t2-t1,wsize,mu,stab,fsmo,method);%noisy smooth
w1na=extract_wavelets_roywhite(sn,t,r,.5*(t1+t2),t2-t1,wsize,0,stab,fsmo,method);%noisy not smooth

pep1=penpred(s,r,w1{1},tw1{1});
pep1a=penpred(s,r,w1a{1},tw1{1});
pep1n=penpred(s,r,w1n{1},tw1{1});
pep1na=penpred(s,r,w1na{1},tw1{1});

% w1n2=w1n2{1};
% tw1n2=tw1n2{1};
figure
subplottop
names={'reflectivity','no noise',['s2n= ' num2str(s2n)]};
trplot(t,[r,s,sn],'normalize',1,'tracespacing',1.5,'names',names);
set(gca,'xlim',[0 max(t)+.2])
yl=get(gca,'ylim');
line([t1 t1],yl,'color',[0 .8 0],'linestyle',':','linewidth',1);
line([t2 t2],yl,'color',[0 .8 0],'linestyle',':','linewidth',1);
title('wavelet estimation with Roy White''s method (frequency-domain), green lines show extraction window')

subplotbota
[err1,cc1]=waveleterr(w,tw,w1{1},tw1{1});
[err1a,cc1a]=waveleterr(w,tw,w1a{1},tw1{1});
[errn,ccn]=waveleterr(w,tw,w1n{1},tw1{1});
[errna,ccna]=waveleterr(w,tw,w1na{1},tw1{1});
it=near(tw,tw(1),tw(end));
names={'true',...
    ['no noise estimate, mu=' num2str(mu) ', err=' num2str(err1), ...
    ', cc(1)=' num2str(sigfig(cc1(1),2)) ', cc(2)=' num2str(cc1(2)) ', pep=' num2str(pep1,2)],...
    ['no noise estimate, mu=' num2str(0) ', err=' num2str(err1a), ...
    ', cc(1)=' num2str(sigfig(cc1a(1),2)) ', cc(2)=' num2str(cc1a(2)) ', pep=' num2str(pep1a,2)],...
    ['noisy estimate, mu=' num2str(mu) ', err=' num2str(errn), ...
    ', cc(1)=' num2str(sigfig(ccn(1),2)) ', cc(2)=' num2str(ccn(2)) ', pep=' num2str(pep1n,2)],...
   ['noisy estimate, mu=' num2str(0) ', err=' num2str(errna), ...
    ', cc(1)=' num2str(sigfig(ccna(1),2)) ', cc(2)=' num2str(ccna(2)) ', pep=' num2str(pep1na,2)]};
hh=trplot({tw(it) tw1{1} tw1{1} tw1{1} tw1{1}},{w(it),w1{1},w1a{1},w1n{1},w1na{1}},'zerolines','y','normalize',0);
legend(cell2mat(hh),names)
title('estimated wavelets')

subplotbotb
if(jcausal)
    wf=[2 ones(1,4)];
else
    wf=ones(1,5);
end
hh=dbspec({tw(it) tw1{1} tw1{1} tw1{1} tw1{1}},{w(it),w1{1},w1a{1},w1n{1},w1na{1}},'windowflags',wf,'normoption',1);
ylim([-100 0])
%legend(cell2mat(hh),names)
title('estimated wavelets spectra')


prepfiga

%% Roy white
t1=.5;t2=1;%estimation time window
wlen=.2;
stab=.01;
mu=1;
method='three';
%method='three';
names={'reflectivity','no noise',['s2n= ' num2str(s2n)]};
[w1,tw1]=waveest_roywhite(s,t,r,t,t1,t2,wlen,stab,mu,method);
[w1n,tw1n]=waveest_roywhite(sn,t,r,t,t1,t2,wlen,stab,mu,method);
wlen2=wlen/(t2-t1);
[w1n2,tw1n2]=extract_wavelets_roywhite(sn,t,r,.5*(t1+t2),t2-t1,wlen2,mu,stab,method);
w1n2=w1n2{1};
tw1n2=tw1n2{1};
figure
subplot(2,1,1)
trplot(t,[r,s,sn],'normalize',1,'tracespacing',1.5,'names',names);
yl=get(gca,'ylim');
line([t1 t1],yl,'color',[0 .8 0],'linestyle',':','linewidth',1);
line([t2 t2],yl,'color',[0 .8 0],'linestyle',':','linewidth',1);
title(['wavelet estimation by ''Roy White'' method ' method ' algorithm'])
subplot(2,1,2)
[err,cc]=waveleterr(w,tw,w1,tw1);
[errn,ccn]=waveleterr(w,tw,w1n,tw1n);
[errn2,ccn2]=waveleterr(w,tw,w1n2,tw1n2);
names={'true',['no noise estimate, err=' num2str(err), ...
    ', cc(1)=' num2str(sigfig(cc(1),2)) ', cc(2)=' num2str(cc(2))],...
    ['noisy estimate, err=' num2str(errn), ...
    ', cc(1)=' num2str(sigfig(ccn(1),2)) ', cc(2)=' num2str(ccn(2))],...
    ['noisy estimate alt, err=' num2str(errn2), ...
    ', cc(1)=' num2str(sigfig(ccn2(1),2)) ', cc(2)=' num2str(ccn2(2))]};
trplot({tw tw1 tw1n tw1n2},{w,w1,w1n,w1n2},'names',names,'zerolines','y','normalize',1);
if(strcmp(method,'three'))
    title(['estimated wavelets by ''Roy White'' method ' method ', mu=' num2str(mu)])
else
    title(['estimated wavelets by ''Roy White'' method ' method ', stab=' num2str(stab)])
end

prepfiga

%% build a nonstationary synthetic
dt=.002;
tmax=2;
fdom=30;
Q=70;
tlen=.3;
[w0,tw0]=wavemin(dt,fdom,tlen);
[r,t]=reflec(tmax,dt,.1,3,pi);
qmat=qmatrix(Q,t,w0,tw0);
s=qmat*r;
%% test match on nonstat
t1=.5;t2=1;
tc=.5*(t1+t2);
iwc=near(t,tc);
iwr=near(t,tc,tc+tlen);
w=qmat(iwr,iwc);
tw=dt*(0:length(w)-1)';
wlen=.2;
[w1,tw1]=waveest_match(s,t,r,t,t1,t2,wlen);
figure
subplot(2,1,1)
trplot(t,[r,s],'normalize',1);
yl=get(gca,'ylim');
line([t1 t1],yl,'color',[0 .8 0],'linestyle',':','linewidth',3);
line([t2 t2],yl,'color',[0 .8 0],'linestyle',':','linewidth',3);
title('match filter')
subplot(2,1,2)
[err,cc]=waveleterr(w,tw,w1,tw1);
trplot({tw tw1},{w,w1},'names',{'true','estimate'},'zerolines','y');

title(['estimate wlen=' time2str(wlen),...
    ', err=' num2str(err), ', cc(1)=' num2str(sigfig(cc(1),2)) ', cc(2)=' num2str(cc(2))])
%% test roy white on nonstat
t1=.5;t2=1;
tc=.5*(t1+t2);
iwc=near(t,tc);
iwr=near(t,tc,tc+tlen);
w=qmat(iwr,iwc);
tw=dt*(0:length(w)-1)';
wlen=.1;
[w1,tw1]=waveest_roywhite(s,t,r,t,t1,t2,wlen);
figure
subplot(2,1,1)
trplot(t,[r,s]);
yl=get(gca,'ylim');
line([t1 t1],yl,'color',[0 .8 0],'linestyle',':','linewidth',3);
line([t2 t2],yl,'color',[0 .8 0],'linestyle',':','linewidth',3);
title('Roy White')
subplot(2,1,2)
[err,cc]=waveleterr(w,tw,w1,tw1);
trplot({tw tw1},{w,w1},'names',{'true','estimate'},'zerolines','y');
title(['estimate wlen=' time2str(wlen) ', err=' num2str(err), ', cc(1)=' num2str(sigfig(cc(1),2)) ', cc(2)=' num2str(cc(2))])