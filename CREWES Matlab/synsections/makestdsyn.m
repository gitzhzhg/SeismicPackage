function [amat,t,x]=makestdsyn(dt,dx,tmax,xmax,v,w,tw)
% MAKESTDSYN: Make a non-diffraction synthetic to demo migration codes
%
% [amat,t,x]=makestdsyn(dt,dx,tmax,xmax,v,w,tw)
%
% make standard migration synthetic
% dt ... desired time sample rate
% dx ... desired space sample rate
% tmax ... maximum time desired in seconds
% xmax ... length of section in physical units
% v ... velocity
% w ... wavelet
% tw ... wavelet time coordinate
%
% amat ... synthetic seismic section
% t ... time coordinate vector for amat
% x ... space coordinate vector for amat
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

ve=v/2;

x1=xmax*4/30;
x2=xmax*15/30;
x3=xmax*15/30;
x4=xmax*26/30;

th1=20; th2=40; th3=60; th4=80;

dtdx1=sin(pi*th1/180)/ve;
dtdx2=sin(pi*th2/180)/ve;
dtdx3=sin(pi*th3/180)/ve;
dtdx4=sin(pi*th4/180)/ve;

%t1=tmax*.04/1.5;t1a=t1+dtdx1*(x2-x1);
%t2=tmax*.07/1.5;t2a=t2+dtdx2*(x2-x1);
%t3=tmax*.12/1.5;t3a=t3+dtdx3*(x2-x1);
%t4=tmax*.18/1.5;t4a=t4+dtdx4*(x2-x1);
t1=0;t1a=t1+dtdx1*(x2-x1);
t2=0;t2a=t2+dtdx2*(x2-x1);
t3=0;t3a=t3+dtdx3*(x2-x1);
t4=0;t4a=t4+dtdx4*(x2-x1);

x=0:dx:xmax;
t=0:dt:tmax;

nx = length(x);
nt = length(t);

amat=zeros(nt,nx);

%dipping events
disp('eight dipping events to build')
amat= event_dip(amat,t,x,[t1 t1a],[x1 x2],1.0);
disp('first event completed')
amat= event_dip(amat,t,x,[t2 t2a],[x1 x2],1.0);
disp('second event done')
amat= event_dip(amat,t,x,[t3 t3a],[x1 x2],1.0);
disp('third event finalized')
amat= event_dip(amat,t,x,[t4 t4a],[x1 x2],1.0);
disp('fourth event accomplished')

amat= event_dip(amat,t,x,[t1a t1],[x3 x4],1.0);
disp('fifth event over')
amat= event_dip(amat,t,x,[t2a t2],[x3 x4],1.0);
disp('sixth event through')
amat= event_dip(amat,t,x,[t3a t3],[x3 x4],1.0);
disp('seventh event ended')
amat= event_dip(amat,t,x,[t4a t4],[x3 x4],1.0);
disp('And the eigth event, c''est fini')

%flat event
t5=tmax*1/1.5;
amat=event_dip(amat,t,x,[t5 t5],[0 xmax],1);

%spikes
ts1=tmax*.25/1.5;ts2=tmax*.75/1.5;
amat=event_spike(amat,t,x,ts1,xmax/2,10);
amat=event_spike(amat,t,x,ts2,xmax/2,10);

%hyperbolas
th1=tmax*.6/1.5;th2=tmax*1.2/1.5;
amat=event_hyp(amat,t,x,th1,xmax/2,v,1);
amat=event_hyp(amat,t,x,th2,xmax/2,v,1,1,1000);

disp('just about done')
amat=sectconv(amat,t,w,tw);