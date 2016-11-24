%
% Demo Q wavelets
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

%
dt=.0005;%time sample rate
tmax=1.5;%maximum record time
v=2000;%constant velocity
Q=50;%Q value
fdom=100;%dominant frequency of initial wavelet

x=[0 250 500 750 1000];%distances to examine (there should always be 5 entries)

[w,tw]=wavemin(dt,fdom,.2);%initial waveform
%array to hold propagated wavelets
wlet=zeros(length(x),round(tmax/dt)+1);%array for propagated wavelets
wlet2=wlet;%array for wavlets in retarded time
qimp=wlet;%array for Q impulse responses
qimp2=wlet;%array for Q impulse responses in retarded time
%function 'einar' is based on Kjartanssen (1979 Geophysics)
for k=1:length(x)
    [qimp(k,:),t]=einar(Q,x(k),v,dt,tmax);%with delay
    qimp2(k,:)=einar(Q,x(k),v,dt,tmax,0);%delay removed
    wlet(k,:)=convm(qimp(k,:),w);%apply initial wavelet
    wlet2(k,:)=convm(qimp2(k,:),w);%apply initial wavelet
end

%plots
figure
inc=max(qimp(:))/8;
h1=zeros(length(x),2);
rightedge=tmax/2;
for k=1:length(x);
    h1(k,:)=wtva((k-1)*inc+qimp(k,:),t,'b',(k-1)*inc,1,-1);
    if(k==1)
        set(h1(k,1),'color','red');
        set(h1(k,2),'facecolor','red');
    end
%     h1(k,:)=line(t,qimp(k,:)+(k-1)*inc);
%     if(k==1)
%         set(h1(k,:),'color','red');
%     end
    text(rightedge,(k-.8)*inc,['Distance = ' num2str(x(k))],'horizontalalignment','right')
end
title({['Fig1: For Q=' num2str(Q) ' An initial, unit magnitude, impulse (red)'];'is shown after various propagation distances (blue)'})
xlim([-.1 rightedge])
prepfig
% yl=get(gca,'ylim');
set(gca,'ylim',[-.1 .6]);
xlabel('seconds')

figure
inc=1;
h2=zeros(length(x),2);
rightedge=.1;
for k=1:length(x);
    h2(k,:)=wtva((k-1)*inc+qimp2(k,:)/max(qimp2(k,:)),t,'b',(k-1)*inc,1,-1);
    if(k==1)
        set(h2(k,1),'color','red');
        set(h2(k,2),'facecolor','red');
    end
%     h1(k,:)=line(t,qimp2(k,:)/max(qimp2(k,:))+(k-1)*inc);
%     if(k==1)
%         set(h1(k,:),'color','red');
%     end
    text(rightedge,(k-.8)*inc,['Distance = ' num2str(x(k))],'horizontalalignment','right')
end
title({'Fig 2: Similar to figure 1 except that the waveforms are normalized';...
    ' and the most of the time delay is removed'})
xlim([-.01 rightedge])
prepfig
yl=get(gca,'ylim');
set(gca,'ylim',[-.05 yl(2)]);
xlabel('seconds')

figure
inc=max(wlet(:))/4;
h3=zeros(length(x),2);
rightedge=tmax/2;
for k=1:length(x);
    h3(k,:)=wtva((k-1)*inc+wlet(k,:),t,'b',(k-1)*inc,1,-1);
    if(k==1)
        set(h3(k,1),'color','red');
        set(h3(k,2),'facecolor','red');
    end
    text(rightedge,(k-.8)*inc,['Distance = ' num2str(x(k))],'horizontalalignment','right')
end
title({['Fig. 3: For Q=' num2str(Q) ' An initial minimum phase wavelet (red)'];...
    'is shown after various propagation distances (blue)'})
xlim([-.1 rightedge])
prepfig
% yl=get(gca,'ylim');
% set(gca,'ylim',[yl(1) yl(2)]);
% set(gca,'ytick',[])
xlabel('seconds')

figure
inc=2;
h4=zeros(length(x),2);
tcut=.08;
ind=near(t,0,tcut);
for k=1:length(x);
    h4(k,:)=wtva((k-1)*inc+wlet2(k,ind)/max(wlet2(k,ind)),t(ind),'b',(k-1)*inc,1,-1);
    if(k==1)
        set(h4(k,1),'color','red');
        set(h4(k,2),'facecolor','red');
    end
    text(tcut,(k-1)*inc-.2,['Distance = ' num2str(x(k))],'horizontalalignment','right')
end
xlim([-.01 tcut+.01])
% yl=get(gca,'ylim');
% set(gca,'ylim',[yl(1) yl(2)]);
%grid
prepfig
title({['Fig 4: For Q=' num2str(Q) ' similar to Fig 3 except '];...
    'that the wavelets have been normalized and most of the time delay removed'})
% set(gca,'ytick',[])
xlabel('seconds')

% examine spectra
figure
dbspec(t,wlet','windowflags',[2 1 1 1 1])
prepfig
legend(['Distance ' num2str(x(1)) 'm'],['Distance ' num2str(x(2)) 'm'],...
    ['Distance ' num2str(x(3)) 'm'],['Distance ' num2str(x(4)) 'm'],...
    ['Distance ' num2str(x(5)) 'm'])
title('Fig 5: Amplitude spectra of wavelets in Figure 4')