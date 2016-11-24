%test wavelet explorer
%estimate wavelets
%min phase synthetic
s2n=3;
dt=.002;
fdom=30;
tlen=.3;
tmax=2;
name=['min phase, s2n=' num2str(s2n) ', '];
[w,tw]=wavemin(dt,fdom,tlen);
[r,t]=reflec(tmax,dt,.1,3,pi);
tz1=.2;tz2=1.8;%extent of log
ind=near(t,tz1,tz2);
rr=r(ind);
tr=t(ind);
s=convm(r,w);
if(~isinf(s2n))
    sn=s+rnoise(s,s2n);
end

%% ricker synthetic
s2n=3;
dt=.002;
fdom=30;
tlen=.3;
tmax=2;
[w,tw]=ricker(dt,fdom,tlen);
w=phsrot(w,37);
[r,t]=reflec(tmax,dt,.1,3,pi);
s=convz(r,w);
if(~isinf(s2n))
    sn=s+rnoise(s,s2n);
end

%% klauder synthetic
s2n=3;
dt=.002;
fmin=10;
fmax=80;
slen=8;
tlen=.3;
taper=.25;
tmax=2;
[w,tw]=wavevib(fmin,fmax,dt,slen,tlen,taper);
[r,t]=reflec(tmax,dt,.2,3,pi);
s=convz(r,w);
if(~isinf(s2n))
    sn=s+rnoise(s,s2n);
end
%% Roy White Explorer
waveex_rw(sn,t,rr,tr,name,w,tw)

%% Match filter Explorer
waveex_match(sn,t,rr,tr,name,w,tw)

%%

%%Simple Explorer
waveex_simple(sn,t,rr,tr,name,w,tw)





