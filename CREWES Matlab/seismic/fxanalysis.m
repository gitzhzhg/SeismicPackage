function [phs,fphs,amp,famp]=fxanalysis(seis,t,t1s,t2s,fmax,dname,x,xname,clip)
% FXANALYSIS ... performs a time-vatiant f-x analysis
%
% [phs,fphs,amp,famp]=fxanalysis(seis,t,t1s,t2s,fmax,dname,x,xname)
%
% seis ... input seismic matrix
% t ... time coordinate for seis
% t1s ... vector of start times for analysis windows
% t2s ... vector of end times for analysis windows
% fmax ... maximum frequency of interest
% dname ... dataset name 
% ******** default '' *********
% x ... horizontal spatial coordinate. Entry of nan gets the default.
% ******** default trace number *******
% xname ... text name of the horizontal coordinate. Entry of nan gets the
%       default.
% ******** default: 'trace number' ********
% clip ... clipping value for the amplitude spectra
% ******** default No Clipping ********
%
% phs ... cell array of phase sections
% fphs ... cell array of frequency coordinate vectors for phs
% amp ... cell array of amplitude sections
% famp ... cell array of frequency coordinate vectors for amp
%
%
% G.F. Margrave, CREWES Project, August 2016
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


if(nargin<9)
    clip=nan;
end
if(nargin<6)
    dname='';
end
if(nargin<7)
    x=nan;
end
if(nargin<8)
    xname=nan;
end
if(isnan(x))
    x=1:size(seis,2);
end
if(isnan(xname))
    xname='trace number';
end


nzones=length(t1s);
if(length(t2s)~=nzones)
    error('t2s and t2s must have the same number of entries');
end

phs=cell(1,nzones);
fphs=phs;
amp=phs;
famp=phs;

pctaper=10;
pctrand=10;
amin=10^20;
amax=0;
stds=zeros(1,nzones);
means=stds;
for k=1:nzones
    [phs{k},fphs{k},amp{k},famp{k}] = fxtran(seis,x,t,t1s(k),t2s(k),pctaper,pctrand,fmax);
    amin=min([amin min(amp{k}(:))]);
    amax=max([amax max(amp{k}(:))]);
    stds(k)=std(amp{k}(:));
    means(k)=mean(amp{k}(:));
end

figure
ht=.85;
wid=.85;
xbeg=.1;
ybeg=.05;
sep=.02;
xnot=mean(x);
fs=max([18/nzones 6]);
fs2=fs;
if(fs2>10)
    fs2=10;
end
%fs0=5;
fs0=max([10/nzones 8]);

kol=.95*ones(1,3);

for k=1:nzones
    kht=ht/nzones;
    kwid=wid;
    xnow=xbeg;
    ynow=ybeg+(nzones-k)*kht+sep;
    subplot('position',[xnow,ynow,kwid,kht-sep]);
    imagesc(x,fphs{k},phs{k},[-1 1])
    h=ylabel('Hz');
    set(gca,'fontsize',fs0)
    set(h,'fontsize',fs0)
    if(k~=nzones)
        %xtick([]);
        set(gca,'xticklabel','');
    else
        hx=xlabel(xname);
        set(hx,'fontsize',fs0);
    end
    if(k==1)
        colormap seisclrs
        htxt=text(xnot,0,[dname ' f-x phase']);
        set(htxt,'fontsize',nzones*fs/2,'horizontalalignment','center','verticalalignment','bottom');
    end

    htxt=text(xnot,fmax,[num2str(t1s(k)) ' to ' num2str(t2s(k)) ' sec']);
    set(htxt,'fontsize',fs2,'horizontalalignment','center','verticalalignment','bottom',...
        'backgroundcolor',kol);
end
prepfig
set(gcf,'name','F-X phase analysis');

figure

for k=1:nzones
    kht=ht/nzones;
    kwid=wid;
    xnow=xbeg;
    ynow=ybeg+(nzones-k)*kht+sep;
    subplot('position',[xnow,ynow,kwid,kht-sep]);
    if(isnan(clip))
        clim=[amin amax];
    else
        c1=max([amin .5*amax-clip*stds(k)]);
        c2=min([amax .5*amax+clip*stds(k)]);
        clim=[c1 c2];
    end
    imagesc(x,famp{k},amp{k},clim)
    h=ylabel('Hz');
    set(gca,'fontsize',fs0)
    set(h,'fontsize',fs0)
    if(k~=nzones)
        %xtick([]);
        set(gca,'xticklabel','');
    else
        hx=xlabel(xname);
        set(hx,'fontsize',fs0);
    end
    if(k==1)
        cm=gray(128);
        %colormap(cm(64:-1:1,:));
        colormap(flipud(cm));
        htxt=text(xnot,0,[dname ' f-x amplitude']);
        set(htxt,'fontsize',nzones*fs/2,'horizontalalignment','center','verticalalignment','bottom');
    end
    htxt=text(xnot,fmax,[num2str(t1s(k)) ' to ' num2str(t2s(k)) ' sec']);
    set(htxt,'fontsize',fs2,'horizontalalignment','center',...
        'verticalalignment','bottom','backgroundcolor',kol);
    xshift=.7;awid=.15;
    aht=kht/3;
    %axes('position',[xnow+xshift-sep ynow+2*sep awid aht]);
    axes('position',[xnow+xshift ynow+2*sep awid aht]);
    Aspec=sum(amp{k},2);
    plot(famp{k},todb(Aspec));
    set(gca,'fontsize',.7*fs2,'fontweight','normal','yaxislocation','right',...
        'tickdir','out')
    ylabel('decibels');xlabel('Hz');
    %title('Average')
    xl=get(gca,'xlim');yl=get(gca,'ylim');
    text(mean(xl),mean(yl),'Average','horizontalalignment','right',...
        'verticalalignment','middle','fontsize',fs2);
    grid
    ylim([-100 0]);xlim([0 fmax]);
end
prepfig
set(gcf,'name','F-X amplitude analysis');
