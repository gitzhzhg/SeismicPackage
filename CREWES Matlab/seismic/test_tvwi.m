%test tvwi (time variant wavelet inversion
load sampleresult

seisresult=thisresult.seisf;
t=thisresult.t;
x=thisresult.x;
tref=thisresult.tref;
sref=thisresult.sref;

wlets=thisresult.wletsb;
tws=thisresult.twsb;
t0s=thisresult.t0s;

%% extract wavelets
%tinc=.2;
%t0s=tref(1)+tinc:tinc:tref(end)-tinc;
t0s=thisresult.t0s;
twin=thisresult.twinw;
%twin=.7;
wsize=.25;%a fraction of twin;
mu=1;
wmethod='match';
ind=near(t,tref(1),tref(end));
s=seisresult(:,1);
% s=s/max(s);%normalize
% %sref=sref/max(sref);%normalize
% sref=s(ind)/10;
if(strcmp(wmethod,'match'))
    [wlets,tws]=extract_wavelets_match(s(ind),t(ind),sref,t0s,twin,wsize,mu);
elseif(strcmp(wmethod,'roywhite'))
    [wlets,tws]=extract_wavelets_roywhite(s(ind),t(ind),sref,t0s,twin,wsize,mu,.001,'three');
else
    [wlets,tws]=extract_wavelets_simple(s(ind),t(ind),sref,t0s,twin);
end

names=cell(1,length(t0s));
for k=1:length(t0s)
    names{k}=['t0 = ' num2str(t0s(k))];
end

figure
subplot(1,3,1)
hh=trplot(tws,wlets,'order','down','zerolines','y');
title([wmethod ' derived wavelets'])
legend(cell2mat(hh),names)
subplot(1,3,2)
hh=dbspec(tws{1},wlets,'signallength',512,'windowflags',ones(size(t0s)));
title('wavelets spectra')
ylim([-100 0])
legend(names)
subplot(1,3,3)
hh2=tvdbspec(t,s,t0s-.5*twin,twin,twin,'trace spectra');
set(hh2(1),'color','k');
set(hh2(2),'color',get(hh{1},'color'));
set(hh2(3),'color',get(hh{2},'color'));
set(hh2(4),'color',get(hh{3},'color'));
ylim([-100 0])
prepfiga

%% make trace comparison plot
figure
subplot1
trplot({t tref},{s sref},'order','d','tracespacing',1.5);
legend('trace','reflectivity')
title('time domain')

subplot2
dbspec({t tref},{s sref},'normoption',1,'windowflags',[1 1]);
title('frequency domain')
ylim([-100 0])

prepfiga
%% method 1

stab=.001;
pcttaper=20;
fmin=0;fmax=100;
delf=0;
method='one';

[si,gop]=tvwi(seisresult,t,wlets,tws,t0s,stab,pcttaper,delf,method);
si=butterband(si,t,fmin,fmax,4,0);


srefb=butterband(thisresult.sref,thisresult.tref,fmin,fmax,4,0);


si2=zeros(length(t),length(x)+1);
ind=near(t,thisresult.tref(1),thisresult.tref(end));
si2(ind,1)=srefb;
si2(:,2:end)=si*max(srefb)/max(si(:,1));
x2=zeros(1,length(x)+1);
x2(2:end)=x;

seis2=zeros(length(t),length(x)+1);
seis2(ind,1)=srefb;
seis2(:,2:end)=seisresult*max(srefb)/max(seisresult(:,1));

[ccb,strb]=maxcorr_phs(seis2(ind,1),seis2(ind,2));
[cca,stra]=maxcorr_phs(si2(ind,1),si2(ind,2));

figure
subplot(1,2,1)

plotseis(seis2,t,x,1,[1.5 max(abs(seis2(:)))],1,1,'b');
title(['input, ' strb])

subplot(1,2,2)
plotseis(si2,t,x2,1,[1.5 max(abs(si2(:)))],1,1,'r');
title({['tvwi: method ' method ', fmax=' num2str(fmax) 'Hz, ' stra],...
    [' stab=' num2str(stab) ', pcttaper=' num2str(pcttaper)]})

boldlines(gcf,.5)

prepfiga

%% method 2
stab=.01;

fmin=0;fmax=100;
pcttaper=20;
delf=10;
method='two';

[si,gop,trow,fcol]=tvwi(seisresult,t,wlets,tws,t0s,stab,pcttaper,delf,method);
si=butterband(si,t,fmin,fmax,4,0);


srefb=butterband(thisresult.sref,thisresult.tref,fmin,fmax,4,0);


si2=zeros(length(t),length(x)+1);
ind=near(t,thisresult.tref(1),thisresult.tref(end));
si2(ind,1)=srefb;
si2(:,2:end)=si*max(srefb)/max(si(:,1));
x2=zeros(1,length(x)+1);
x2(2:end)=x;

seis2=zeros(length(t),length(x)+1);
seis2(ind,1)=srefb;
seis2(:,2:end)=seisresult*max(srefb)/max(seisresult(:,1));

[ccb,strb]=maxcorr_phs(seis2(ind,1),seis2(ind,2));
[cca,stra]=maxcorr_phs(si2(ind,1),si2(ind,2));


figure
subplot(1,2,1)

plotseis(seis2,t,x,1,[1.5 max(abs(seis2(:)))],1,1,'b');
grid
title(['input, ' strb])

subplot(1,2,2)
plotseis(si2,t,x2,1,[1.5 max(abs(si2(:)))],1,1,'r');
grid
title({['tvwi: method ' method ', fmax=' num2str(fmax) 'Hz, ' stra],...
    [' stab=' num2str(stab) ', pcttaper=' num2str(pcttaper)]})

boldlines(gcf,.5)

prepfiga

figure
imagesc(fcol,trow,real(todb((gop))));
colorbar
title(['gabor operator method ' method ' (db), stab=' num2str(stab)])
xlabel('frequency (Hz)');
ylabel('time (sec)')
prepfiga

%% method 3
stab=.01;
pcttaper=00;
fmin=0;fmax=100;
delf=0;

method='three';

[si,gop,trow,fcol]=tvwi(seisresult,t,wlets,tws,t0s,stab,pcttaper,delf,method);
si=butterband(si,t,fmin,fmax,4,0);

srefb=butterband(thisresult.sref,thisresult.tref,fmin,fmax,4,0);


si2=zeros(length(t),length(x)+1);
ind=near(t,thisresult.tref(1),thisresult.tref(end));
si2(ind,1)=srefb;
si2(:,2:end)=si*max(srefb)/max(si(:,1));
x2=zeros(1,length(x)+1);
x2(2:end)=x;

seis2=zeros(length(t),length(x)+1);
seis2(ind,1)=srefb;
seis2(:,2:end)=seisresult*max(srefb)/max(seisresult(:,1));

[ccb,strb]=maxcorr_phs(seis2(ind,1),seis2(ind,2));
[cca,stra]=maxcorr_phs(si2(ind,1),si2(ind,2));

figure
subplot(1,2,1)

plotseis(seis2,t,x2,1,[1.5 max(abs(seis2(:)))],1,1,'b');
grid
title(['input, ' strb])

subplot(1,2,2)
plotseis(si2,t,x2,1,[1.5 max(abs(si2(:)))],1,1,'r');
grid
title({['tvwi: method ' method ', fmax=' num2str(fmax) 'Hz, ' stra],...
    [' stab=' num2str(stab) ', pcttaper=' num2str(pcttaper)]})

boldlines(gcf,.5)

prepfiga

figure
imagesc(fcol,trow,real(todb((gop))));
colorbar
title(['gabor operator method ' method ' (db), stab=' num2str(stab)])
xlabel('frequency (Hz)');
ylabel('time (sec)')
prepfiga
%% method 4
stab=.01;

fmin=0;fmax=100;
pcttaper=20;
delf=10;
method='four';

[si,gop,trow,fcol]=tvwi(seisresult,t,wlets,tws,t0s,stab,pcttaper,delf,method);
si=butterband(si,t,fmin,fmax,4,0);


srefb=butterband(thisresult.sref,thisresult.tref,fmin,fmax,4,0);


si2=zeros(length(t),length(x)+1);
ind=near(t,thisresult.tref(1),thisresult.tref(end));
si2(ind,1)=srefb;
si2(:,2:end)=si*max(srefb)/max(si(:,1));
x2=zeros(1,length(x)+1);
x2(2:end)=x;

seis2=zeros(length(t),length(x)+1);
seis2(ind,1)=srefb;
seis2(:,2:end)=seisresult*max(srefb)/max(seisresult(:,1));

[ccb,strb]=maxcorr_phs(seis2(ind,1),seis2(ind,2));
[cca,stra]=maxcorr_phs(si2(ind,1),si2(ind,2));


figure
subplot(1,2,1)

plotseis(seis2,t,x,1,[1.5 max(abs(seis2(:)))],1,1,'b');
grid
title(['input, ' strb])

subplot(1,2,2)
plotseis(si2,t,x2,1,[1.5 max(abs(si2(:)))],1,1,'r');
grid
title({['tvwi: method ' method ', fmax=' num2str(fmax) 'Hz, ' stra],...
    [' stab=' num2str(stab) ', pcttaper=' num2str(pcttaper)]})

boldlines(gcf,.5)

prepfiga

figure
imagesc(fcol,trow,real(todb((gop))));
colorbar
title(['gabor operator method ' method ' (db), stab=' num2str(stab)])
xlabel('frequency (Hz)');
ylabel('time (sec)')
prepfiga