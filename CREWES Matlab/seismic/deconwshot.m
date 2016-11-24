function [shotd,tstart,tend]=deconwshot(shot,t,xoff,tstart,tend,xd,top,stab)
% DECONWSHOT: applies Wiener deoon to a shot record
%
% shot ... shot gather as a matrix of traces
% t ... time coordinate for shot
% xoff ... offset coordinate for the traces in shot
% tstart ... length 2 vector of design window start times
% tend ... length 2 vector of design window end times
% xd ... length 2 vector of the two offsets at which tstart and tend are
%       prescribed
% top ... length of decon operator in seconds
%  ****** default 0.1 ******
% stab ... decon white noise (stab) factor
%  ****** default =.001 *****
% 
% shotd ... deconvolved shot record
% tstart ... length(xoff) vector of decon gate start times
% tend ... length(xoff) vector of decon gate end times

if(nargin<8)
    stab=.001;
end
if(nargin<7)
    top=.1;
end

%determine hyperbolae for start and end of design window
xd=abs(xd);
if(diff(xd)==0)
    error('xd must give two distinct offsets');
end
if(length(tstart)~=2)
    error('tstart must contain two entries');
end
if(length(tend)~=2)
    error('tend must contain two entries');
end
if(length(xd)~=2)
    error('xd must contain two entries');
end

vstart=sqrt((xd(2)^2-xd(1)^2)/(tstart(2)^2-tstart(1)^2));
vend=sqrt((xd(2)^2-xd(1)^2)/(tend(2)^2-tend(1)^2));
t0start=sqrt(tstart(1)^2-xd(1)^2/vstart^2);
t0end=sqrt(tend(1)^2-xd(1)^2/vend^2);
%define top of gate at all offsets
tstart=sqrt(t0start^2+xoff.^2/vstart^2);
%define bottom of gate at all offsets
tend=sqrt(t0end^2+xoff.^2/vend^2);

%determine operator length in samples
dt=t(2)-t(1);
nop=round(top/dt);

shotd=zeros(size(shot));

for k=1:length(xoff)
    tmp=shot(:,k);
    idesign=near(t,tstart(k),tend(k));
    if(length(idesign)<2*nop)
        idesign=[idesign idesign(end)+1:2*nop];%pad out to 2*nop samples
        %adjust tend(k)
        tend(k)=tstart(k)+2*nop*dt;
    end
    shotd(:,k)=deconw(tmp,tmp(idesign),nop,stab);
end
