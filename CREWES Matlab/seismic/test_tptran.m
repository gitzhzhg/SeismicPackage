%% test tau-p transform code
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

%make a text shot record
nx=200;
nt=501;
dx=10;
dt=.002;
x=(0:nx-1)*dx;
t=((0:nt-1)*dt)';
shot=zeros(nt,nx);
v0Left=1750;
v0Right=1250;
v1=2000;
v2=3000;
xs=(nx/2)*dx;
xoffmax=max(x-xs);
t0=.1;
shot=event_dip(shot,t,x,[xoffmax/v0Left+t0 t0],[0 xs],[.1 .2]);
shot=event_dip(shot,t,x,[t0 xoffmax/v0Right+t0],[xs max(x)],[.2 .1]);
shot=event_hyp(shot,t,x,max(t)/4,xs,2*v1,.1);
shot=event_hyp(shot,t,x,max(t)/2,xs,2*v2,-.1);
[w,tw]=ricker(dt,30,.2);
shot=convz(shot,w);
xoff=x-xs;
plotimage(shot,t,xoff);
title('Shot gather to be transformed')
xlabel('Offset (m)')
v0=min([v0Left v0Right]);
pmin=-2/v0;pmax=-pmin;
dp=.25*(pmax-pmin)/nx;

[stp,tau,p]=tptran(shot,t,xoff,pmin,pmax,dp);

plotimage(stp,tau,p)
title('Tau-p transform of shot gather')
xlabel('Slowness (s/m)')

%% test inverse tau-p transform
%run the previous first
xmin=xoff(1);
xmax=xoff(end);
% dx=2*(xmax-xmin)/(length(p)-1);
[seis2,t2,x2]=itptran(stp,tau,p,xoff(1),xoff(end),dx);

plotimage(seis2,t2,x2)
title('reconstructed shot gather')

plotimage(shot-seis2(1:length(t),:),t,x2)
title('difference')