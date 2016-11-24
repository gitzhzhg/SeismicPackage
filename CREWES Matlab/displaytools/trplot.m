function [htraces,hnames]=trplot(times,traces,varargin)
% TRPLOT ... make a nice trace plot
%
% [htraces,hnames]=trplot(times,traces,varargin)
%
% Plots in the current axes an array of traces with the time axis
% horizontal (by default). Options exist to shift the traces vertically,
% resample for smooth curves, normalize, and so on. This is meant as a
% time-domain partner for dbspec.
%
% times ... cell array of times or a single time vector if all traces are
%           the same length
% traces ... cell array of traces or a matrix of traces, one per column
%
% The remaining arguments are input as name,value pairs. The name is always
% a string and the value comes immediately after it. The defined names and
% their default values are described below:
%  Name        ... value
% 'distribute' ... 'y' or 'n' 
% '*********** default is 'y' ***************
% 'direction' ... 'h' means horizontal 'v' means vertical
%  *********** default is 'h' *********
% 'normalize' ... 0 means don't normalize (default)
%                 1 means normalize all traces to max of 1
%                 2 means normalize all traces w.r.t the first trace
% ************ default is 0 (don't normalize) ***********
% 'normwindow' ... [t1 t2] a length 2 vector of start and end times defining
%              the normalization window. The normalization process will only
%              look at amplitudes in this window. The trace separation will
%              also be determined by the maximum amplitude in this window.
% ************ default [t(1) t(end)] ***********
% 'yaxis' ... 'n' ... means don't show numerical labels on y tick marks (default)
%             'y' ... means do show numerical labels
% ************ deafult is 'n' (don't show) ****************
% 'names' ... Cell array of names (Strings) , one per trace, to be 
%            annotated at the end (or see the next two arguments) of each trace
% ************ default is no names are annotated ***************
% 'nameslocation' ... Choose one of 'end', 'middle','beginning'. Means that
%            supplied name is placed at the end, middle, or beginning of
%            the trace.
% ************ default is 'end' ***************
% 'namesalign' ... Choose one of 'left','right','center'. Means that
%            the name is either left, center, or right justified with the
%            end, beginning, or middle.
% ************ default is 'left' for 'end' , 'right' for 'beginning',
%              and 'center' for 'middle' **************
% 'nameshift' ... a scalar value between +1 and -1 giving the shift of the 
%               names in the direction orthogonal to the trace. Specified
%               as a fraction of the trace spacing. A value of 0 gets the
%               default behaviour while 1 shifts the name up to the next
%               trace.
% ************ default is 0 *************
% 'resample' ... n ... the traces will be resampled (sinc interpolation) 
%             to have n times as many samples as input. So n=1 means no
%             resampling, n=2 means twice as many samples, and so on. May
%             be a decimal number.  
% ************* Default is to resample if traces are shorter than 500 samples 
%               such that they always have at least 500 samples. **********
% 'order' ... 'u' ... means plot trace #1 at the bottom and other traces above it (default)
%         ... 'd' ... means plot trace #1 at the top and other traces below it
% ************* Default is 'u' ****************
% 'fontsize' ... fontsize for names annotation. Default is 9
% ************* Default is 9 ******************
% 'linewidths' ... vector of values, one per trace giving the linewidths. A
%               value of 1 is the standard linewidth while a value of 2 is
%               a double width. If a single value is given it is assumed
%               to apply to all traces. If more than 1 value, then there
%               must be one value per trace.
% ************* Default is ones(1,ntraces) ************
% 'linestyles' ... cell array of linestyles, one per trace giving the
%               linestyles. Possible values are '-', '-.', ':', and 'none'.
%               A value of '' (empty) is the same as the default '-'.
% ************* Default is '-' ************
% 'markers' ... cell array of marker types, one per trace giving the
%               marker. Common choices are '*', '.', 'o', and 'none'.
%               A value of '' (empty) is the same as the default 'none'
% ************* Default is 'none' ************
% 'ievery' ... ordinary array, one value per trace giving the frequency at
%               which markers are plotted. A value of 1 means that a marker
%               is placed at every point while a value of 10 means a maker
%               is placed at every tenth point. Must be a value >= 1 for
%               each trace. Has no effect if 'markers' is empty or 'none'
% ************* Default is ones(1,ntraces) ************
% 'markersizes' ... ordinary array, one value per trace giving the
%               marke size. A value of 1 is the default size while 2 is
%               double size and .5 is half size
% ************* Default is ones(1,ntraces) ************
% 'colors' ... cell array of colors, one per trace giving the
%               color. Common choices are 'b', 'r', 'g', and 'c'.
%               A value of '' gets the default color.
% ************* Default is the color order set by Matlab for the axes ************
% 'zerolines' ... 'y' means draw a dotted line indicating zero amplitude in
%               the same color as the signal. 'n' means don't
% ************* default is 'n' ***********
% 'zorder' ... an ordinary array giving the order of the trace plots 
%               in the third dimension. This is intended to give control
%               over how the various traces overlay one another. There
%               must be one value per trace and the default is 1:ntraces
%               which means that the traces are plotted on top of each
%               other in the order of their input. The first trace is on
%               the bottom and the last trace is on top. For example, if
%               there are 3 traces then 1:3 gives the default order while
%               [3 1 2] makes the first trace on top and the last trace on
%               the bottom. You can think of zorder as giving the data a
%               third dimension that is perpendicular to the plot and has
%               the value of zorder. This really only matters if
%               'distribute' is set to 'n'.
% ********* default 1:ntraces *********
% 'tracespacing' ... a scalar to adjust the tracespacing. A value of 1 is
%               the default. A value of 2 would space the traces 2 times
%               farther apart. Values greater than 0 and less than 5
%               accepted.
% ********** default is 1 ***********
%
% htraces ... cell array of trace handles. Normally there is one handle per
%           trace, however, if both lines and markers are requested then
%           there are two handles per trace.
% hnames ... cell array of name handles
%
% Examples: 
% plot traces s1, s2, and s3, which are all the sample length versus t.
%      trplot(t,[s1 s2 s3]);%accept the default for all name-value pairs
%      trplot(t,[s1 s2 s3],'normalize',1,'resample',2); %as above except
%                               normalize and double the number of samples
%      trplot(t,[s1 s2 s3],'names',{'trace 1','trace 2','trace 3'})%plot names 
%                               at the ends of the traces
% plot traces s1, s2, and s3, which are different lengths as specified by t1, t2, and t3
%      trplot({t1, t2, t3},{s1, s2, s3})%note the use of cell arrays
%
% 
% by G.F. Margrave, 2016
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

%check for non-cell input and convert to cell
if(~iscell(traces))
    [nr,nc]=size(traces);
    if((nr-1)*(nc-1)==0)
        traces=traces(:);
        ntraces=1;
    else
        ntraces=nc;
        %nsamps=nr;
        tmp=cell(1,ntraces);
        for k=1:ntraces
            tmp{k}=traces(:,k);
        end
        traces=tmp;
    end
else
    ntraces=length(traces);
end

if (~iscell(times))
    [nr,nc]=size(times);
    if((nr-1)*(nc-1)~=0)
        error('times must be either a vector or a cell array of vectors');
    end
    if(length(times)~=length(traces{1}))
        error('times and traces do not have the same number of samples')
    end
    times=times(:);
    tmp=cell(1,ntraces);
    for k=1:ntraces
        tmp{k}=times;
    end
    times=tmp;
end

if(length(times)~=length(traces))
    error('times and traces must have same number of entries')
end

%find the maximum min time and the minimum max fime
tmaxs=zeros(1,ntraces);
tmins=tmaxs;
for k=1:ntraces
    tmins(k)=min(times{k});
    tmaxs(k)=max(times{k});
end
tmax=min(tmaxs);
tmin=max(tmins);


%set defaults
distribute=1;
direction='h';
normalize=0;
yaxis=0;
names=cell(1,ntraces);
linewidths=ones(1,ntraces);
linestyles=cell(1,ntraces);
markers=cell(1,ntraces);
markersizes=ones(1,ntraces);
ievery=ones(1,ntraces);
colors=cell(1,ntraces);
resample=0;
nsamp0=500;%traces shorter than this will be reampled by default
order=0;
fs=9;
namesloc='end';
namesalign='';
nameshift=0;
zerolines=0;
zorder=1:ntraces;
tnorm=[tmin tmax];
trspace=1;

%handle varargin
if(nargin>2)
    nargs=length(varargin);%number of input arguments after the first two
    if(2*floor(nargs/2)~=nargs)
        error('the extra arguments must be name-value pairs')
    end

    for k=1:2:nargs
        arg=varargin{k};
        if(~ischar(arg))
            error('additional arguments must be (string,value) pairs')
        end
        recognized=0;
        if(strcmp(arg,'distribute'))
            val=varargin{k+1};
            if(strcmpi(val,'y')||strcmpi(val,'yes'))
                distribute=1;
                recognized=1;
            elseif(strcmpi(val,'n')||strcmpi(val,'no'))
                distribute=0;
                recognized=1;
            else
                error('''distribute'' must be either ''y'' or ''n''')
            end
        end
        if(strcmp(arg,'zerolines')||strcmp(arg,'zeroline'))
            val=varargin{k+1};
            if(strcmpi(val,'y')||strcmpi(val,'yes'))
                zerolines=1;
                recognized=1;
            elseif(strcmpi(val,'n')||strcmpi(val,'no'))
                zerolines=0;
                recognized=1;
            elseif(strcmpi(val,'on'))
                zerolines=1;
                recognized=1;
            elseif(strcmpi(val,'off'))
                zerolines=0;
                recognized=1;
            else
                error('''zerolines'' must be either ''y'' or ''n''')
            end
        end
        if(strcmp(arg,'direction'))
            direction=varargin{k+1};
            if(~strcmp(direction,'h')&&~strcmp(direction,'v'))
                error('''direction'' must be either ''h'' or ''v''')
            end
            recognized=1;
        end
        if(strcmp(arg,'order'))
            val=varargin{k+1};
            if(strcmpi(val,'u')||strcmpi(val,'up'))
                order=0;
                recognized=1;
            elseif(strcmpi(val,'d')||strcmpi(val,'down'))
                order=1;
                recognized=1;
            else
                error('''order'' must be either ''u'' or ''d''')
            end
        end
        if(strcmp(arg,'normalize'))
            normalize=round(varargin{k+1});
            if((normalize-varargin{k+1})~=0)
                error('''normalize'' must be a single integer number');
            end
            if(normalize~=0 && normalize ~=1 && normalize~=2)
                error('''normalize'' must 0, 1, or 2');
            end
            recognized=1;
        end
        if(strcmp(arg,'normwindow'))
            tnorm=varargin{k+1};
            if(length(tnorm)~=2)
                error('''normwindow'' must be a length 2 vector');
            end
            if(diff(tnorm)<=0)
                error('''normwindow'' second value must be larger than the first');
            end
            if(tnorm(1)<tmin)
                error('''normwindow'' tnorm(1) cannot be smaller than tmin');
            end
            if(tnorm(2)>tmax)
                error('''normwindow'' tnorm(2) cannot be greater than tmax');
            end
            recognized=1;
        end
        if(strcmp(arg,'yaxis'))
            val=varargin{k+1};
            if(strcmpi(val,'y')||strcmpi(val,'yes')||strcmpi(val,'on'))
                yaxis=1;
                recognized=1;
            elseif(strcmpi(val,'n')||strcmpi(val,'no')||strcmpi(val,'off'))
                yaxis=0;
                recognized=1;
            else
                error('''yaxis'' must be either ''y'' or ''n''')
            end
        end
        if(strcmp(arg,'names'))
            names=varargin{k+1};
            if((~iscell(names)))
                error('''names'' must be a cell array of strings, one per trace');
            end
            if(length(names)~=ntraces)
                error('''names'' must have one entry per trace');
            end
            recognized=1;
        end
        if(strcmp(arg,'nameslocation'))
            namesloc=varargin{k+1};
            if(~strcmpi(namesloc,'end')&&strcmpi(namesloc,'middle')&&strcmp(namesloc,'beginning'))
                error('''nameslocation'' must be either ''end'' or ''middle'' or ''beginning''')
            end
            recognized=1;
        end
        if(strcmp(arg,'namesalign')||strcmp(arg,'namealign'))
            namesalign=varargin{k+1};
            if(~strcmpi(namesalign,'left')&&strcmpi(namesalign,'center')&&strcmp(namesalign,'right'))
                error('''namesalign'' must be either ''left'' or ''right'' or ''middle''')
            end
            recognized=1;
        end
        if(strcmp(arg,'namesshift')||strcmp(arg,'nameshift'))
            nameshift=varargin{k+1};
            if(nameshift>1 || nameshift<-1)
                error('''nameshift'' must be between +1 and -1')
            end
            recognized=1;
        end
        if(strcmp(arg,'linewidths')||strcmp(arg,'linewidth'))
            linewidths=varargin{k+1};
            if(length(linewidths)==1)
                linewidths=linewidths*ones(1,ntraces);
            end
            if(length(linewidths)~=ntraces)
                error('''linewidths'' must have one entry per trace');
            end
            recognized=1;
        end
        if(strcmp(arg,'linestyles'))
            linestyles=varargin{k+1};
            if(length(linestyles)~=ntraces)
                error('''linestyles'' must have one entry per trace');
            end
            recognized=1;
        end
        if(strcmp(arg,'markers'))
            markers=varargin{k+1};
            if(length(markers)~=ntraces)
                error('''markers'' must have one entry per trace');
            end
            recognized=1;
        end
        if(strcmp(arg,'ievery'))
            ievery=varargin{k+1};
            if(length(ievery)~=ntraces)
                error('''ievery'' must have one entry per trace');
            end
            if(any(ievery<1))
                error('''ievery'' must be >= 1 for all traces')
            end
            recognized=1;
        end    
        if(strcmp(arg,'markersizes'))
            markersizes=varargin{k+1};
            if(length(markersizes)~=ntraces)
                error('''markersizes'' must have one entry per trace');
            end
            if(any(markersizes<0))
                error('''markersizes'' must be >= 0 for all traces')
            end
            recognized=1;
        end 
        if(strcmp(arg,'colors')||strcmp(arg,'color'))
            colors=varargin{k+1};
            if(length(colors)~=ntraces)
                error('''colors'' must have one entry per trace');
            end
            recognized=1;
        end
        if(strcmp(arg,'resample'))
            resample=round(varargin{k+1});
            if((~isnumeric(resample)))
                error('''resample'' must be a number >=1');
            end
            if(resample<1)
                error('''resample''must be a number >=1');
            end
            recognized=1;
        end
        if(strcmp(arg,'fontsize'))
            fs=round(varargin{k+1});
            if((~isnumeric(fs)))
                error('''fontsize'' must be a number >=1');
            end
            if(fs<1)
                error('''fontsize''must be a number >=1');
            end
            recognized=1;
        end
        if(strcmp(arg,'zorder'))
            zorder=varargin{k+1};
            if(length(zorder)~=ntraces)
                error('there must be one zorder per trace');
            end
            recognized=1;
        end
        if(strcmp(arg,'tracespacing'))
            trspace=varargin{k+1};
            if(length(trspace)~=1)
                error('tracespacing mus t be a scalar');
            end
            if(trspace<=0 || trspace>5)
                error('tracespacing must be greater than 0 and less than 5')
            end
            recognized=1;
        end
        if(~recognized)
            error([arg ' is not a recognized parameter name'])
        end
    end
end

if(strcmp(namesloc,'end') && isempty(namesalign))
    namesalign='left';
elseif(strcmp(namesloc,'beginning') && isempty(namesalign))
    namesalign='right';
elseif(strcmp(namesloc,'middle') && isempty(namesalign))
    namesalign='center';
end
if(strcmp(direction','v'))
    rotation=-90;
    if(strcmp(namesloc,'end'))
        if(strcmp(namesalign,'left'))
            rotation=-90;
        elseif(strcmp(namesalign,'right'))
            rotation=+90;
        elseif(strcmp(namesalign,'center'))
            rotation=0';
        end
    elseif(strcmp(namesloc,'beginning'))
        if(strcmp(namesalign,'left'))
            rotation=90;
        elseif(strcmp(namesalign,'right'))
            rotation=-90;
        elseif(strcmp(namesalign,'center'))
            rotation=0';
        end
    end
end


if(normalize==1)
    for k=1:ntraces
        tmp=traces{k};
        if(sum(tmp)~=0)
            inorm=near(times{k},tnorm(1),tnorm(2));
            traces{k}=tmp/max(abs(tmp(inorm)));
        end
    end
elseif(normalize==2)
    inorm=near(times{1},tnorm(1),tnorm(2));
    a=1/max(abs(traces{1}(inorm)));
    for k=1:ntraces
        traces{k}=traces{k}*a;
    end    
end

if(distribute==1)
    amax=0;
    for k=1:ntraces
        inorm=near(times{k},tnorm(1),tnorm(2));
        am=max(abs(traces{k}(inorm)));
        if(am>amax)
            amax=am;
        end
    end
    if(order==0)
        ystart=0;
        inc=amax*trspace;
    else
        ystart=(ntraces-1)*amax;
        inc=-amax*trspace;
    end
else
    ystart=0;
    inc=0;
end

ht=cell(size(traces));
hn=ht;
for k=1:ntraces
    trace=traces{k};
    time=times{k};
    hh=zeros(1,2);%for trace handles
    if(isempty(linestyles{k}))
        ls='-';
    else
        ls=linestyles{k};
    end
    if(isempty(markers{k}))
        mk='none';
    else
        mk=markers{k};
    end
    if(length(time)~=length(trace))
        error(['input trace ' int2str(k) ...
            ' has a length different from its time vector'])
    end
    if(resample==0)
        nt=length(time);
        if(nt<nsamp0)
            rsmpl=ceil(nsamp0/nt);
        else
            rsmpl=1;
        end
    else
        rsmpl=resample;
    end
    if(rsmpl>1)
        time=linspace(times{k}(1),times{k}(end),rsmpl*length(times{k}));
        trace=interpbl(times{k},traces{k},time);
    end
    %nameshift=-.2;
    if(strcmp(direction,'h'))
        if(~strcmp(ls,'none'))
            hh(1)=plot(time,trace+ystart+(k-1)*inc);
            zd=zorder(k)*ones(size(time));
            set(hh(1),'linestyle',ls,'zdata',zd);
        end
        if(~strcmp(mk,'none'))
            hh(2)=plot(time(1:ievery(k):end),trace(1:ievery(k):end)+ystart+(k-1)*inc);
            zd=zorder(k)*ones(size(time(1:ievery(k):end)));
            set(hh(2),'linestyle','none','marker',mk,'zdata',zd);
        end
        if(zerolines)
           hz=line(time,zeros(size(time))+ystart+(k-1)*inc,'linestyle',':'); 
           zd=zorder(k)*ones(size(time));
           set(hz,'zdata',zd)
        end
        if(~isempty(names{k}))
            if(strcmp(namesloc,'end'))
                hn{k}=text(time(end),ystart+(k-1)*inc+nameshift*inc,-1,names{k},...
                    'fontsize',fs,'horizontalalignment',namesalign,'backgroundcolor','w');
            elseif(strcmp(namesloc,'middle'))
                imiddle=round(length(time)/2);
                hn{k}=text(time(imiddle),ystart+(k-1)*inc+nameshift*inc,-1,names{k},...
                    'fontsize',fs,'horizontalalignment',namesalign,'backgroundcolor','w');
            else
                hn{k}=text(time(1),ystart+(k-1)*inc+nameshift*inc,-1,names{k},...
                    'fontsize',fs,'horizontalalignment',namesalign,'backgroundcolor','w');
            end
        end
    else
        if(~strcmp(ls,'none'))
            hh(1)=plot(trace+ystart+(k-1)*inc,time);
            zd=zorder(k)*ones(size(trace));
            set(hh(1),'linestyle',ls,'zdata',zd);
        end
        if(~strcmp(mk,'none'))
            hh(2)=plot(trace(1:ievery(k):end)+ystart+(k-1)*inc,time(1:ievery(k):end));
            zd=zorder(k)*ones(size(trace(1:ievery(k):end)));
            set(hh(2),'linestyle','none','marker',mk,'zdata',zd);
        end
        if(zerolines)
            hz=line(zeros(size(time))+ystart+(k-1)*inc,time,'linestyle',':');
            zd=zorder(k)*ones(size(trace));
            set(hz,'zdata',zd)
        end
        if(~isempty(names{k}))
            if(strcmp(namesloc,'end'))
                hn{k}=text(ystart+(k-1)*inc+nameshift*inc,time(end),names{k},'fontsize',fs,...
                'rotation',rotation,'backgroundcolor','w');
            elseif(strcmp(namesloc,'middle'))
                imiddle=round(length(time)/2);
                hn{k}=text(ystart+(k-1)*inc+nameshift*inc,time(imiddle),names{k},'fontsize',fs,...
                'rotation',rotation,'backgroundcolor','w');
            else
                hn{k}=text(ystart+(k-1)*inc+nameshift*inc,time(1),names{k},'fontsize',fs,...
                'rotation',rotation,'backgroundcolor','w');
            end
        end
    end
    if(k==1)
        hold on;
        if(strcmp(direction,'v'))
            flipy;
        end
    end
    if(linewidths(k)~=1)
        if(hh(1)~=0)
            set(hh(1),'linewidth',linewidths(k)*get(hh(1),'linewidth'));
        end
    end
    if(markersizes(k)~=1)
        if(hh(2)~=0)
            set(hh(2),'markersize',markersizes(k)*get(hh(2),'markersize'));
        end
    end
    if(~isempty(colors{k}))
        if(hh(1)~=0)
            set(hh(1),'color',colors{k});
        end
        if(hh(2)~=0)
            set(hh(2),'color',colors{k});
        end
    end
    if(hh(1)~=0 && hh(2)~=0)
        ht{k}=hh;
    elseif(hh(2)==0)
        ht{k}=hh(1);
    else
        ht{k}=hh(2);
    end
    %make sure zero line has the same color as the trace
    if(zerolines)
        set(hz,'color',get(ht{k}(1),'color'));
    end
end
if(yaxis==0)
    if(strcmp(direction,'h'))
        set(gca,'yticklabels',[]);
    else
        set(gca,'xticklabels',[]);
    end
end
if(strcmp(direction,'h'))
    xlabel('time (sec)')
else
    ylabel('time (sec)')
end
hold off
grid


if(nargout>0)
    htraces=ht;
end
if(nargout>1)
    hnames=hn;
end
