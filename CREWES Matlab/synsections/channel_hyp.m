% CHANNEL: model a channel beneath a few layers
%
% low velocity channel beneath a v(z) medium
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

% Just run the script
dx=5; %cdp interval
dt=.002; %time sample rate
xmax=2500;tmax=1.0; %maximum line length and maximum time
x=0:dx:xmax; % x coordinate vector
t=0:dt:tmax; % t coordinate vector
v=3000; % velocity
z=v*t/2; % z coordinate vector

%initialize seismic matrix
seis=zeros(length(t),length(x));

%first event
z1=100;
seis=event_pwlinh(seis,t,x,v,[0 xmax],[z1 z1],[.1 .1]);
disp('first event of five done')

%second event
z2=200;
seis=event_pwlinh(seis,t,x,v,[0 xmax],[z2 z2],[-.1 -.1]);
disp('second event of five done')

%third event
z3=271;
seis=event_pwlinh(seis,t,x,v,[0 xmax],[z3 z3],[.05 .05]);
disp('third event of five done')

%fourth event
z4=398;
seis=event_pwlinh(seis,t,x,v,[0 xmax],[z4 z4],[-.04 -.04]);
disp('fourth event of five done')

%channel
width=100;
thk=50;
xchan=[xmax/2-width/2  xmax/2-.5*width/2 xmax/2+.5*width/2 xmax/2+width/2];
zchan=[z4 z4+thk z4+thk z4];
seis=event_pwlinh(seis,t,x,v,xchan,zchan,.04*ones(size(xchan)));
disp('fifth event of five done')

plotimage(seis,t,x);title('Unfiltered model')

%apply a filter

seis=sectfilt(seis,t,[10 5],[150 30]);

plotimage(seis,t,x);title('Filtered to 10-150 Hz.');