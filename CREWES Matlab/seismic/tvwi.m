function [si,gop,trow,fcol]=tvwi(s,t,wlets,tws,t0s,stab,pcttaper,delf,method)
% TVWI ... time-variant wavelet inversion applied to a seismic trace
%
% 
% TVSI takes a cell array of wavelets estimated at different times through
% well tying and performs a time-variant deconvolution of a seismic trace
% with them. A variety of methods are available and most of them are done
% in the Gabor domain and result in a Gabor operator that is returned.
%
% s ... seismic trace (or traces) to be deconvolved
% t ... time coordinate for s
% wlets ... cell array of wavelets estimated at different times
% tws ... cell array of time-coordinate vectors for wlets
%  *** Currently all wavelets should have the same time coordinate ***
% t0s ... vector of estimation times for the wavelets
%         these should be in order of increasing time.
% NOTE: wlets, tws, and t0s should all be the same length.
% stab ... stability constant used in the spectral division
% pcttaper ... percent taper of an mwindow applied to wavelets.
% delf ... width of frequency smoother (Hz)
% method ... 'one' : apply stationary inverses for each wavelet and
%            interpolate the result in the time domain. No Gabor operator.
%            'two' : interpolate the wavelets (time domain) and build a
%            Gabor operator to apply by division;
%            'three' : interpolate the inverse wavelets (frequency domain)
%            and build a Gabor operator to apply by multiplication.
%            'four' : interpolate the log spectra of the wavelets and build
%            a Gabor operator to apply by division.
%
% si ... the inverted seismic trace (or traces)
% gop ... the Gabor operator (null if method is 'one'
%

nt=length(t);
nw=length(wlets);
ntrcs=size(s,2);
if(size(s,1)~=nt)
    error('s and t have incompatible sizes');
end

if(nw~=length(tws))
    error('wlets and tws must have the same length')
end

if(length(t0s)~=nw)
    error('wlets and t0s must have the same length');
end

switch method
    case 'one'
        % design the inverse operators
        d=cell(size(wlets));
        mw=mwindow(length(wlets{1}),pcttaper);
        for k=1:nw;
           n=round(length(wlets{k})/2);
           d{k}=toinv(wlets{k}.*mw,stab,n,0); 
        end
        wts=designweights(t,t0s,50);
        si=zeros(size(s));
        for k=1:ntrcs
            stmp=zeros(nt,nw);
            for j=1:nw;
                stmp(:,j)=wts(:,j).*convz(s(:,k),d{j});
            end
            si(:,k)=sum(stmp,2);
        end
        gop=[];
        trow=[];
        fcol=[];
        
    case 'two'
        %design the gabor operator
        nw=length(wlets);
        ntrows=10*nw;% just a guess that we want the Gabor operator to have ten times as many rows as we have wavelets.
        tinc=(t(end)-t(1))/(ntrows-1);
        twin=4*tinc;
        [sg,trow]=fgabor(s(:,1),t,twin,tinc);%to get the size of things
        tinc2=trow(2)-trow(1);%fgabor changes tinc
        %pad out t to match trow
        tt=t(1):t(2)-t(1):trow(end);
        [gop,trow,fcol]=buildgaboropw(wlets,tws{1},t0s,tt,tinc2,stab,pcttaper,delf);
        %apply the operator in the gabor domain
        si=zeros(size(s));
        for k=1:ntrcs
            if(k>1)
                sg=fgabor(s(:,k),t,twin,tinc);
            end
            sgi=sg.*gop;
            sgi(:,end)=real(sgi(:,end));%ensure real Nyquist
            tmp=igabor(sgi,trow,fcol,twin,tinc);
            si(:,k)=tmp(1:length(t));
        end
        
    case 'three'
        dt=t(2)-t(1);
        % design the inverse operators
        nw=length(wlets);
        d=cell(size(wlets));
        mw=mwindow(length(wlets{1}),pcttaper);
        for k=1:nw;
           n=round(length(wlets{k})/2);
           d{k}=toinv(wlets{k}.*mw,stab/100,n,0); 
        end
        n2=floor(n/2);
        td=dt*(-n2:n-n2)';
        %design the gabor operator
        ntrows=10*nw;% just a guess that we want the Gabor operator to have ten times as many rows as we have wavelets.
        tinc=(t(end)-t(1))/(ntrows-1);
        twin=4*tinc;
        [sg,trow]=fgabor(s(:,1),t,twin,tinc);%to get the size of things
        tinc2=trow(2)-trow(1);%fgabor changes tinc
        %pad out t to match trow
        tt=t(1):t(2)-t(1):trow(end);
        [gop,trow,fcol]=buildgaboropd(d,td,t0s,tt,tinc2,stab);
        %apply the operator in the gabor domain
        si=zeros(size(s));
        for k=1:ntrcs
            if(k>1)
                sg=fgabor(s(:,k),t,twin,tinc);
            end
            sgi=sg.*gop;
            sgi(:,end)=real(sgi(:,end));%ensure real Nyquist
            tmp=igabor(sgi,trow,fcol,twin,tinc);
            si(:,k)=tmp(1:length(t));
        end
    case 'four'
        %design the gabor operator
        nw=length(wlets);
        ntrows=10*nw;% just a guess that we want the Gabor operator to have ten times as many rows as we have wavelets.
        tinc=(t(end)-t(1))/(ntrows-1);
        twin=4*tinc;
        [sg,trow]=fgabor(s(:,1),t,twin,tinc);%to get the size of things
        tinc2=trow(2)-trow(1);%fgabor changes tinc
        %pad out t to match trow
        tt=t(1):t(2)-t(1):trow(end);
        [gop,trow,fcol]=buildgaborop_logw(wlets,tws{1},t0s,tt,tinc2,stab,pcttaper,delf);
        %apply the operator in the gabor domain
        si=zeros(size(s));
        for k=1:ntrcs
            if(k>1)
                sg=fgabor(s(:,k),t,twin,tinc);
            end
            sgi=sg.*gop;
            sgi(:,end)=real(sgi(:,end));%ensure real Nyquist
            tmp=igabor(sgi,trow,fcol,twin,tinc);
            si(:,k)=tmp(1:length(t));
        end
end

end

function w=designweights(t,t0s,transpct)
%design a set of raised cosine weights to transition between a set of reference times
%
transpct=transpct/100;
nw=length(t0s);
nt=length(t);
w=zeros(nt,nw);

for k=1:nw
    if(k==1)
        ind=t<=t0s(k);
        w(ind,k)=1;
        %back end
        dt=t0s(k+1)-t0s(k);
        ta=t0s(k);
        tb=ta+.5*(1-transpct)*dt;
        tc=tb+transpct*dt;
        %td=t0s(k+1);
        ind=between(ta,tb,t,1);% (ta<=t) && (t<tb);
        if(ind~=0)
            w(ind,k)=1;
        end
        ind=between(tb,tc,t,1);%(tb<=t) && (t<tc);
        w(ind,k)=.5+.5*cos(pi*(t(ind)-tb)/(tc-tb));
    elseif(k==nw)
        ind=t>=t0s(k);
        w(ind,k)=1;
        %front end
        dt=t0s(k)-t0s(k-1);
        ta=t0s(k-1);
        tb=ta+.5*(1-transpct)*dt;
        tc=tb+transpct*dt;
        td=t0s(k);
        ind=between(tc,td,t,1);%(tc<=t) && (t<td);
        if(ind~=0)
            w(ind,k)=1;
        end
        ind=between(tb,tc,t,1);%(tb<=t) && (t<tc);
        w(ind,k)=.5+.5*cos(pi*(t(ind)-tc)/(tc-tb));
    else
        %front end
        dt=t0s(k)-t0s(k-1);
        ta=t0s(k-1);
        tb=ta+.5*(1-transpct)*dt;
        tc=tb+transpct*dt;
        td=t0s(k);
        ind=between(tc,td,t,1);%(tc<=t) && (t<td);
        if(ind~=0)
            w(ind,k)=1;
        end
        ind=between(tb,tc,t,1);%(tb<=t) && (t<tc);
        w(ind,k)=.5+.5*cos(pi*(t(ind)-tc)/(tc-tb));
        %back end
        dt=t0s(k+1)-t0s(k);
        ta=t0s(k);
        tb=ta+.5*(1-transpct)*dt;
        tc=tb+transpct*dt;
        %td=t0s(k+1);
        ind=between(ta,tb,t,1);% (ta<=t) && (t<tb);
        if(ind~=0)
            w(ind,k)=1;
        end
        ind=between(tb,tc,t,1);%(tb<=t) && (t<tc);
        w(ind,k)=.5+.5*cos(pi*(t(ind)-tb)/(tc-tb));
    end
    
    
end



end

function [gop,trow,fcol]=buildgaboropw(w,tw,t0s,t,tinc,stab,pcttaper,delf)
% w ... cell of wavelets
% tw ... single time coordiante vector assuming all wavelets have the same
% t0s ... estimation times for the wavelets
% 
% method:
% 1- expand the wavelet spcification by repeating the first wavelet at
%       t(1) and the last wavelet at t(end). This gives nw+2 wavelets.
% 2- Run fftrl on each wavelet to get the onr-sided spectra. Put these in a
%       matrix goptmp, one on each row.
% 3- Apply linear phase shifts to account for noncausal operators
% 4- Interpolate from nw+2 wavelets to one wavelet every tinc for the span
%       of t. Interpolation with interp1 (linear).
% 5- Invert each row to get decon operators. This is gopf
goptmp=zeros(length(w)+2,length(t));
t0s2=zeros(length(w)+2,1);
goptmp(1,:)=pad_trace(w{1},t);
t0s2(1,:)=t(1);
t0s2(end)=t(end);
t0s2(2:end-1)=t0s;
goptmp(end,:)=pad_trace(w{end},t);
for k=2:length(w)+1
    goptmp(k,:)=pad_trace(w{k-1},t);
end
%fourier transform
nt=length(t);
nt2=2^nextpow2(nt);
[tmp,fcol]=fftrl(goptmp(1,:),t,pcttaper,nt2);
gopftmp=zeros(length(t0s2),length(fcol));
gopftmp(1,:)=tmp;
nsmo=round(delf/fcol(2));
for k=1:length(t0s2)
    if(k>1)
        gopftmp(k,:)=fftrl(goptmp(k,:),t,pcttaper,nt2);
    end
%     gopftmp(k,:)=smooth_ampspec(gopftmp(k,:),nsmo);
%     gopftmp(k,:)=polyfit_ampspec(gopftmp(k,:),delf);
end
%apply linear phase assuming each wavelet is noncausal with time zero in
%the middle. The sign of this thing is tricky. Since we will be inverting
%this, the sign is opposite to what might be expected from the wavelets
%themselves.
ind=near(tw,0);
dt=tw(2)-tw(1);
tshift=dt*(ind(1)-1);
shiftr=exp(1i*2.*pi*tshift*fcol);
for k=1:length(t0s2)
   gopftmp(k,:)=gopftmp(k,:).*shiftr; 
end
%interpolate
trow=t(1):tinc:t(end);
gopw=zeros(length(trow),length(fcol));
for k=1:length(fcol)
%     tmp=gopftmp(:,k);
%     Amp=interp1(t0s2,abs(tmp),trow,'linear');
%     Phs=interp1(t0s2,tmp./abs(tmp),trow,'linear');
%     gop(:,k)=Amp.*Phs;
    gopw(:,k)=interp1(t0s2,gopftmp(:,k),trow,'linear');
end
Amax=max(abs(gopftmp(:)));
tmp=abs(gopw);
ntsmo=3;
tmp2=conv2(tmp+stab*Amax,ones(ntsmo,nsmo),'same')/(ntsmo*nsmo);
gopw=tmp2.*gopw./tmp;
%add stab and smooth
%invert
gop=1./gopw;
% Amax=max(abs(gopftmp(:)));
% gop=zeros(size(gop));
% for k=1:length(trow)
%     tmp=gopw(k,:);
%     gop(k,:)=1 ./(tmp+stab*Amax);
% end

end

function [gop,trow,fcol]=buildgaborop_logw(w,tw,t0s,t,tinc,stab,pcttaper,delf)
% w ... cell of wavelets
% tw ... single time coordiante vector assuming all wavelets have the same
% t0s ... estimation times for the wavelets
% 
% method:
% 1- expand the wavelet spcification by repeating the first wavelet at
%       t(1) and the last wavelet at t(end). This gives nw+2 wavelets.
% 2- Run fftrl on each wavelet to get the onr-sided spectra. Put these in a
%       matrix goptmp, one on each row.
% 3- Apply linear phase shifts to account for noncausal operators
% 4- Interpolate from nw+2 wavelets to one wavelet every tinc for the span
%       of t. Interpolation with interp1 (linear).
% 5- Invert each row to get decon operators. This is gopf
goptmp=zeros(length(w)+2,length(t));
t0s2=zeros(length(w)+2,1);
goptmp(1,:)=pad_trace(w{1},t);
t0s2(1,:)=t(1);
t0s2(end)=t(end);
t0s2(2:end-1)=t0s;
goptmp(end,:)=pad_trace(w{end},t);
for k=2:length(w)+1
    goptmp(k,:)=pad_trace(w{k-1},t);
end
%fourier transform
nt=length(t);
nt2=2^nextpow2(nt);
[tmp,fcol]=fftrl(goptmp(1,:),t,pcttaper,nt2);
gopftmp=zeros(length(t0s2),length(fcol));
gopftmp(1,:)=tmp;
nsmo=round(delf/fcol(2));
for k=1:length(t0s2)
    if(k>1)
        gopftmp(k,:)=fftrl(goptmp(k,:),t,pcttaper,nt2);
    end
%     gopftmp(k,:)=smooth_ampspec(gopftmp(k,:),nsmo);
%     gopftmp(k,:)=polyfit_ampspec(gopftmp(k,:),delf);
end
%apply linear phase assuming each wavelet is noncausal with time zero in
%the middle. The sign of this thing is tricky. Since we will be inverting
%this, the sign is opposite to what might be expected from the wavelets
%themselves.
ind=near(tw,0);
dt=tw(2)-tw(1);
tshift=dt*(ind(1)-1);
shiftr=exp(1i*2.*pi*tshift*fcol);
for k=1:length(t0s2)
   gopftmp(k,:)=gopftmp(k,:).*shiftr; 
end
%interpolate
trow=t(1):tinc:t(end);
gopw=zeros(length(trow),length(fcol));
for k=1:length(fcol)
%     tmp=gopftmp(:,k);
%     Amp=interp1(t0s2,abs(tmp),trow,'linear');
%     Phs=interp1(t0s2,tmp./abs(tmp),trow,'linear');
%     gop(:,k)=Amp.*Phs;
    gopw(:,k)=exp(interp1(t0s2,log(gopftmp(:,k)),trow,'linear'));
end
Amax=max(abs(gopftmp(:)));
tmp=abs(gopw);
ntsmo=3;
tmp2=conv2(tmp+stab*Amax,ones(ntsmo,nsmo),'same')/(ntsmo*nsmo);
gopw=tmp2.*gopw./tmp;
%add stab and smooth
%invert
gop=1./gopw;
% Amax=max(abs(gopftmp(:)));
% gop=zeros(size(gop));
% for k=1:length(trow)
%     tmp=gopw(k,:);
%     gop(k,:)=1 ./(tmp+stab*Amax);
% end

end

function [gop,trow,fcol]=buildgaboropd(d,tw,t0s,t,tinc,stab)
% d ... cell of inverse operators
% tw ... single time coordiante vector assuming all operators have the same
% t0s ... estimation times for the operators
% 
% method:
% 1- expand the wavelet spcification by repeating the first wavelet at
%       t(1) and the last wavelet at t(end). This gives nw+2 wavelets.
% 2- Run fftrl on each wavelet to get the onr-sided spectra. Put these in a
%       matrix goptmp, one on each row.
% 3- Apply linear phase shifts to account for noncausal operators
% 4- Interpolate from nw+2 wavelets to one wavelet every tinc for the span
%       of t. Interpolation with interp1 (linear).
% 5- Invert each row to get decon operators. This is gopf
goptmp=zeros(length(d)+2,length(t));
t0s2=zeros(length(d)+2,1);
goptmp(1,:)=pad_trace(d{1},t);
t0s2(1,:)=t(1);
t0s2(end)=t(end);
t0s2(2:end-1)=t0s;
goptmp(end,:)=pad_trace(d{end},t);
for k=2:length(d)+1
    goptmp(k,:)=pad_trace(d{k-1},t);
end
%fourier transform
nt=length(t);
nt2=2^nextpow2(nt);
[tmp,fcol]=fftrl(goptmp(1,:),t,0,nt2);
gopftmp=zeros(length(t0s2),length(fcol));
gopftmp(1,:)=tmp;
Amax=0;
for k=2:length(t0s2)
    gopftmp(k,:)=fftrl(goptmp(k,:),t,0,nt2);
    Am=max(abs(gopftmp(k,:)));
    if(Am>Amax)
        Amax=Am;
    end
end

gopftmp=gopftmp+stab*Amax;

%apply linear phase assuming each wavelet is noncausal with time zero in
%the middle. The sign of this thing is tricky. Since we will be inverting
%this, the sign is opposite to what might be expected from the wavelets
%themselves.
ind=near(tw,0);
dt=tw(2)-tw(1);
tshift=dt*(ind(1)-1);
shiftr=exp(1i*2.*pi*tshift*fcol);
for k=1:length(t0s2)
   gopftmp(k,:)=gopftmp(k,:).*shiftr; 
end
%interpolate
trow=t(1):tinc:t(end);
gop=zeros(length(trow),length(fcol));
for k=1:length(fcol)
    gop(:,k)=interp1(t0s2,gopftmp(:,k),trow,'linear');
end

end

function specout=smooth_ampspec(spec,nsmo)

amp=abs(spec);
ampsmo=convz(amp,ones(nsmo,1))/nsmo;
specout=ampsmo.*spec./amp;

end


function specout=polyfit_ampspec(spec,norder)

x=1:length(spec);
amp=abs(spec);
p=polyfit(.5*x/x(end),amp,norder);
ampsmo=polyval(p,.5*x/x(end));
specout=ampsmo.*spec./amp;

end