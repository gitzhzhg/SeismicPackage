function [autos,tlags]=autotv(seis,t,t1s,t2s,tmlags,pflag,name)
%
% seis ... input seismic section
% t ... time coordinate
% t1s ... start times of the windows
% t2s ... end times of the windows
% tmlags ... max lag in each window
% pflag ... plotting flag, 1 to plot, 0 to not plot
% name ... string to title the top axes
%

nwins=length(t1s);
if(nwins~=length(t2s))
    error('t1s and t2s must be same size vectors');
end

if(nwins~=length(tmlags))
    error('tmlags must be the same size as t1s');
end

autos=cell(1,nwins);
tlags=autos;
dt=t(2)-t(1);
nx=size(seis,2);

for k=1:nwins
    tmlag=round(tmlags(k)/dt)*dt;
    tau=-tmlag:dt:tmlag;
    n=tmlag/dt;
    ind=near(t,t1s(k),t2s(k));
    mw=mwindow(length(ind));
    otto=zeros(length(tau),nx);
    for kk=1:nx
        otto(:,kk)=auto2(mw.*seis(ind,kk),n);
    end
    autos{k}=otto;
    tlags{k}=tau;
end

if(pflag)
    figure
    axht=.8/nwins;
    axwid=.8;
    x=1:nx;
    xnot=.1;
    ynot=.1;
    sep=.02;
    ynow=.95;
    for k=1:nwins
        ynow=ynow-axht;
        axes('position',[xnot,ynow,axwid,axht-sep])
        imagesc(x,tlags{k},autos{k},[-.8,1]);
        text(1,.5*tmlags(k),['Autos from ' num2str(t1s(k)) 's to ' num2str(t2s(k)) 's'])
        ylabel('lag time (s)');
        if(k<nwins)
            set(gca,'xticklabel',[])
        end
        if(k==1)
            title(name)
        end
        grid
        colorbar
    end
    if(~iscell(name))
        set(gcf,'name',['autotv: ' name])
    else
        set(gcf,'name',['autotv: ' name{1}])
    end
    prepfiga
end
        
