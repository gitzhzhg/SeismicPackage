function [A,f]=aveampspectr(s,t,x,tw1,tw2,xw,pct)
% AVEAMPSPECTR ... average amplitude spectrum in shaped (t,x) window
%
% [A,f]=aveampspectr(s,t,x,tw1,tw2,xw,pct)
% 
% The Fourier amplitude spectrum of each trace in a polygonal window is
% computed and normalized for trace length within the window. Then the
% average of thse is computed and returned. The window is defined by start
% and end times at an arbitrary set of x coordinates. If the defined window
% does not span the data gather (in x) it is automatically expanded to do
% so by horizontal extrapolation.
%
% s ... seismic gather
% t ... time coordinate for s
% x ... space coordinate for s
% tw1 ... length(xw) vector of window start times
% tw2 ... length(xw) vector of window end times
% xw ... x coordinates of tw1 and tw2 specifications
% pct ... percent taper on window ends
% ******** default pct = 10 *********
%
%
% G.F. Margrave, CREWES Project, 2016
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
if(nargin<3)
    x=1:size(s,2);
end

if(nargin<4)
    tw1=[t(1) t(1)];
    tw2=[t(end) t(end)];
    xw=[x(1) x(end)];
else
    %make sure the spatial extend of windows covers that of traces
    xw=xw(:)';
    tw1=tw1(:)';
    tw2=tw2(:)';
    [xw,isort]=sort(xw);
    tw1=tw1(isort);
    tw2=tw2(isort);
    if(min(xw)>min(x))
            xw=[min(x) xw];
            tw1=[tw1(1) tw1];
            tw2=[tw2(1) tw2];
    end
    if(max(xw)<max(x))
        xw=[xw max(x)];
        tw1=[tw1 tw1(end)];
        tw2=[tw2 tw2(end)];
    end
end
if(nargin>3 && nargin<6)
    error('you must specify all three of tw1 tw2 xw');
end
if(length(tw1)~=length(xw))
    error('tw1 and xw must have the same length');
end
if(length(tw2)~=length(xw))
    error('tw2 and xw must have the same length');
end
if(nargin<7)
    pct=10;
end

%compute a window at each trace and 
%find the widest window
twid=0;
t1=zeros(size(x));
t2=t1;
for k=1:length(x)
    t1(k)=interp1(xw,tw1,x(k));%window start time
    t2(k)=interp1(xw,tw2,x(k));%window end time
    tmp=t2(k)-t1(k);
    if(tmp>twid)
        twid=tmp;
        k0=k;
    end
end
    
tmin=tw1(k0);
tmax=tw2(k0);
inom=near(t,tmin,tmax);
n=length(inom);
n2=2^nextpow2(n);


swin=zeros(size(s));
for k=1:length(x)
    stmp=zeros(size(inom));
    iuse=near(t,t1(k),t2(k));
    stmp(iuse-inom(1)+1)=s(iuse,k).*mwindow(length(iuse),pct);
    swin(iuse,k)=s(iuse,k).*mwindow(length(iuse),pct);
    
    [Stmp,f]=fftrl(stmp,t(inom),0,n2);
    
    if(k==1)
        A=zeros(size(f));
    end
    A=A+abs(Stmp)/length(iuse);%note normalization for window length
    
end

A=A/length(x);