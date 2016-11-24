%% 1) minimum phase synthetic
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
icausal=10;
%% 2) ricker synthetic
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
icausal=50;
%% 3) klauder synthetic
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
icausal=50;

%% 4) match filter

t1=.7;t2=1.2;%esimation time window
wsize=.5;%a fraction of window size
mu=1;%smootheness constraint

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
title('estimated wavelets spectra')


prepfiga
