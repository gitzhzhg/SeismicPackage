function wstats=wavelet_stats(w,tw)
% wavelet_stats measures dominant frequency, delay, and phase rotation of a wavelet.
%
% wstats{1} ... a string summarizing the measurements
% wstats{2} ... the time delay
% wstats{3} ... the phase rotation
% wstats{4} ... the dominant frequency
w=w(:);tw=tw(:);
[wz,twz]=tozero(w,tw);
dt=tw(2)-tw(1);
small=.0000000001*dt;
%equalize the time axes
if(abs(twz(1)-tw(1))>small)
    if(twz(1)<tw(1))
        npad=round((tw(1)-twz(1))/dt);
        w=[zeros(npad,1);w];
        tw=(twz(1):dt:tw(end))';
    else
        npad=round((twz(1)-tw(1))/dt);
        wz=[zeros(npad,1);wz];
        twz=(tw(1):dt:twz(end))';
    end
end
if(abs(twz(end)-tw(end))>small)
    if(twz(end)<tw(end))
        npad=round((tw(end)-twz(end))/dt);
        wz=[wz;zeros(npad,1)];
        twz=(twz(1):dt:tw(end))';
    else
        npad=round((twz(end)-tw(end))/dt);
        w=[w;zeros(npad,1)];
        tw=(tw(1):dt:twz(end))';
    end
end

%measure delay and phase
x=maxcorr_ephs(wz,w);
tdelay=-x(2)*dt; %the minus sign means a positive delay is a wavelet at greater time.
phs=x(3);
fdom=dom_freq(w,tw);

str=[' delay= ' time2str(tdelay) ' s, phase= ' int2str(phs) ', deg, fdom= ' int2str(fdom) ' Hz'];

wstats={str,tdelay,phs,fdom};

        