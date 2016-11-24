function hs=dbspec(t,s,varargin)
% DBSPEC: plots a Fourier amplitude spectrum using a decibel scale.
%
% hs=dbspec(t,s,varargin)
% hs=dbspec(t,s)
%
% DBSPEC plots a simple Fourier decibel amplitude spectrum (one-sided) in
% the current axes. If s is a vector, then a single curve is plotted
% showing the amplitude spectrum of s. If s is a matrix or a cell array, then a separate
% spectral curve is determined and plotted for each column or each cell. (The decibel
% reference is controlled by 'normoptions'.) A companion program trplot
% offers similar functionality for the time domain.
%
%
% s= input trace or traces. If a matrix, then the spectra of the columns
%   are computed and plotted. In this case, all signals must tbe the same
%   length. If a cell array, then each entry should be a single trace. The
%   traces may be of variable lengths and all are padded with zeros to the
%   length of the longest before computing their spectra.
% t= input time coordinate vector. That is, t is a vector giving the times
%   at which the samples of s occur. Requirement: length(t) must equal
%   size(s,1).
% varargin ... cell array of (name,value) pairs. The names must be Matlab
%   strings. The following name,values are recognized
%  Name    ... values
%  'windowflags' ... one entry per trace with the meaning:
%               0 ... transform directly without windowing
%               1 ... apply an mwindow to s before transforming. (An mwindow
%                   is a boxcar with a raised cosine taper on each end. The
%                   window will be the length of the signal and the taper
%                   comprises 20% of the length.)
%               2 ... apply a half-mwindow to s before transforming. (A
%                   half-mwindow is a boxcar with a raised cosine taper
%                   only at the end of the window. This is appropriate for
%                   causal, impulsive signals.)
%                       ************* default=0 **************
%  'normoption' ... a single scalar either 0 or 1
%               0 ... normalize decibels to the maximum over all input traces
%               1 ... each input trace has decibels calculated with respect
%               to its own maximum
%                      ************** default =0 *************
%  'signallength' ... a single scalar greater than or equal to the trace
%                     length. The traces will be padded with zeros to this
%                     length before transforming.
%                       ************* default is no padding *********
%  'linestyles' ... cell aray of plotting linestyles, one per trace.
%                   An entry of '' is the same as the default '-'
%                      ************** default is '-' for all lines ********
%  'linewidths' ... array of numerical line widths, one per trace. A value
%                   of 1 gets the standard Matlab linewidth while 2 gets
%                   double the standard and .5 gets half that. If a single
%                   value is given it is assumed to apply to all traces. If
%                   more than 1 value, then there must be one value per trace.
%                      ************** default is 1 for all lines ********
%  'colors'     ... cell array of colors, one per trace. An entry of ''
%                   gets the Matlab default color. For example, if there
%                   are three traces, then {'','k',''} gets the default
%                   colors for the first and third traces but the second is
%                   changed to black.
%                   ******* default is the standard Matlab colors *******        
%  'graylevels' ... rather than colored lines, gray lines will be plotted.
%                   There must be one value per line between 0 and 1 with 0
%                   being black and 1 being white.
%                      ****** default is lines are plotted in color *******
%  'markers' ... cell array of markers for each line. An entry of '' is
%                   treated as 'none'
%                  ******* default is 'none' for all lines  *******
%  'markersizes' ... array of numeric marker sizes for each line. 
%                  ******* default is 6 for all lines *******
%  'ievery' ... ordinary array, one value per trace giving the spacing at
%               which markers are plotted. A value of 1 means that a marker
%               is placed at every point while a value of 10 means a marker
%               is placed at every tenth point. Must be a value >= 1 for
%               each trace. Has no effect if 'markers' is empty or 'none'
%               ************* Default is ones(1,ntraces) ************
%  'zorder' ... an ordinary array giving the order of the spectral plots 
%               in the third dimension. This is intended to give control
%               over how the various spectra overlay one another. There
%               must be one value per trace and the default is 1:ntraces
%               which means that the traces are plotted on top of each
%               other in the order of their input. The first trace is on
%               the bottom and the last trace is on top. For example, if
%               there are 3 traces then 1:3 gives the default order while
%               [3 1 2] makes the first trace on top and the last trace on
%               the bottom. You can think of zorder as giving the data a
%               third dimension that is perpendicular to the plot and has
%               the value of zorder.
%               ********* default 1:ntraces *********
%  'average' ... 1 means compute and plot only the average amplitude
%               spectrum, 2 means compute and plot both the individual
%               trace spectra and the average amplitude spectrum. 0 Means
%               plot only the individual trace spectra. Note: the average
%               spectrum will be displayed as a solid black line.
%               ********** default = 0 ********
%
% examples:
%   dbspec(t,s) ... simplest possble. Will result in a color plot
%   dbspec(t,[s1 s2 s3]) ... s1,s2,and s3 are 3 column vectors of the same
%           length as t. Again, is a color plot.
%   dbspec(t,[s1 s2 s3],'windowflags',[0 1 0]) ... as before except that
%           s2 will have a window applied.
%   dbspec(t,[s1 s2 s3],'graylevels',[0 0 .5],'linestyles,{'-', ':', ':'})
%           Here the plot will be in graylevels with s1 and s2 being black 
%           and s3 being medium gray. s1 will be a solid line while s2
%           and s3 are dotted.
%
%  To see the allowed linestyles and markers, type>> help plot
%
% hs= cell array of the handles of the plotted spectra, one per trace unless both lines
% and markers are requested in which case there are two handles for such
% traces.
% 
% 
% by G.F. Margrave, 1991-2015
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

%check for cell array input

if(iscell(s))
    ntraces=length(s);
    tracelengths=zeros(1,ntraces);
    %determine longest signal and pad the others to that same length
    N=length(s{1});
    for k=2:ntraces
        if(length(s{k})>N)
            N=length(s{k});
        end
    end
    %make a new s array
    stmp=zeros(N,ntraces);
    tmp=1:N;
    for k=1:ntraces
        tracelengths(k)=length(s{k});%save the original trace length for proper windowing
        stmp(:,k)=pad_trace(s{k},tmp);
    end
    %examine t
    if(iscell(t))
        ntraces=length(t);
        dt=t{1}(2)-t{1}(1);
        for k=2:ntraces
            dt2=t{1}(2)-t{1}(1);
            if(dt2~=dt)
                error('all input traces must have the same sample rate');
            end
        end
    else
        if(~isvec(t))
            error('t must be a simple vector')
        end
        dt=t(2)-t(1);
    end
    s=stmp;
    t=dt*(0:N-1)';  
else
    if(size(s,2)>1)
        tracelengths=size(s,1)*ones(1,size(s,2));
    else
        tracelengths=length(s);
    end
end
            

%figure
[nsamps,ntraces]=size(s);
%test for a row vector
if(nsamps==1)
    s=s(:);
    [nsamps,ntraces]=size(s);
end
if(length(t)~=nsamps)
    error('length(t) not equal to the number of samples')
end
%set default behavior
n=nsamps;
flags=zeros(1,ntraces);
normoption=0;
klrs=get(gca,'colororder');
nklrs=size(klrs,1);

%fix for having more traces than colors. KWH.
if nklrs <= ntraces
    klrs = repmat(klrs,ceil(ntraces/nklrs),1);
end

lws=ones(1,ntraces);
kolors=cell(1,ntraces);
lstyles=cell(1,ntraces);
markers=cell(1,ntraces);
mkrsizes=6*ones(1,ntraces);
ievery=ones(1,ntraces);
zorder=1:ntraces;
for k=1:ntraces
    lstyles{k}='-';
    markers{k}='none';
%     if(k<=nklrs) %this fix for having more traces than colors does not
%     work in R2014b! KWH
        kolors{k}=klrs(k,:);
%     else
%         kolors{k}=klrs(k-nklrs,:);
%     end
end

usegray=0;
npad=n;
average=0;
if(nargin>2)
    %decompose varagin
    nargs=length(varargin);
    for k=1:2:nargs
        arg=varargin{k};
        if(~ischar(arg))
            error('additional arguments must be (string,value) pairs')
        end
        recognized=0;
        if(strcmp(arg,'windowflags'))
            flags=varargin{k+1};
            if(length(flags)==1)
                flags=flags*ones(1,ntraces);
            end
            if(length(flags)~=ntraces)
                error('there must be one windowflag per trace');
            end
            recognized=1;
        end
        if(strcmp(arg,'normoption')||strcmp(arg,'normopt')||strcmp(arg,'normalize'))
            normoption=round(varargin{k+1});
            if((normoption-varargin{k+1})~=0)
                error('normoption must be a single integer number');
            end
            if(normoption~=0 && normoption ~=1)
                error('normoption must 0 or 1');
            end
            recognized=1;
        end
        if(strcmp(arg,'signallength'))
            npad=round(varargin{k+1});
            if(length(npad)>1)
                error('signallength must be a single integer number');
            end
            if(npad<nsamps)
                error('signallength must not be less than length of input traces');
            end
            recognized=1;
        end
        if(strcmp(arg,'graylevels')||strcmp(arg,'graylevel'))
            gls=varargin{k+1};
            if(length(gls)~=ntraces)
                error('there must be one graylevel per trace');
            end
            kolors=cell(1,ntraces);
            for j=1:ntraces
                kolors{k}=gls(j)*ones(1,3);
            end
            usegray=1;
            recognized=1;
        end
        if(strcmp(arg,'linewidths')||strcmp(arg,'linewidth'))
            lws=varargin{k+1};
            if(length(lws)==1)
                lws=lws*ones(1,ntraces);
            end
            if(length(lws)~=ntraces)
                error('there must be one linewidth per trace');
            end
            recognized=1;
        end
        if(strcmp(arg,'linestyles')||strcmp(arg,'linestyle'))
            lstyles=varargin{k+1};
            if(length(lstyles)~=ntraces)
                error('there must be one linestyle per trace');
            end
            if(~iscell(lstyles))
                error('linestyles must be a cell array');
            end
            recognized=1;
        end
        if(strcmp(arg,'colors'))
            kolors=varargin{k+1};
            if(length(kolors)~=ntraces)
                error('there must be one color per trace');
            end
            if(~iscell(kolors))
                error('colors must be a cell array');
            end
            recognized=1;
        end
        if(strcmp(arg,'markers')||strcmp(arg,'marker'))
            markers=varargin{k+1};
            if(length(markers)~=ntraces)
                error('there must be one marker per trace');
            end
            if(~iscell(markers))
                error('markers must be a cell array');
            end
            recognized=1;
        end
        if(strcmp(arg,'markersizes')||strcmp(arg,'markersize'))
            mkrsizes=varargin{k+1};
            if(length(mkrsizes)~=ntraces)
                error('there must be one markersize per trace');
            end
            recognized=1;
        end
        if(strcmp(arg,'ievery'))
            ievery=varargin{k+1};
            if(length(ievery)~=ntraces)
                error('there must be one ievery per trace');
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
        if(strcmp(arg,'average'))
            average=varargin{k+1};
            if(average~=0 && average~=1 && average~=2)
                error('average can only be 0, 1, or 2');
            end
            recognized=1;
        end
        if(~recognized)
            error([arg ' is not a recognized parameter name'])
        end
    end
end


% test to see if s is complex
test=imag(s);
mag=sum(abs(test));
% branch accordingly
if(mag>0.0)
    error('complex-valued traces not supported')
%     if flag <2
%         mw=ones(size(s(1,:)));
%         if flag==0, mw=mwindow(n,20);end
%         if flag==1, mw=mwhalf(n,20);end
%         for it=1:ntraces, s(:,it)=s(:,it).*mw;end
%     end
%     %spec=abs(fftshiftm(fft(s.',n)));
%     spec=abs(fftshiftm(fft(s,n)));
%     db=20*log10(spec/max(max(spec)));
%     dt=t(2)-t(1);
%     fn=1./(2*dt);
%     df=1/((n-1)*dt);
%     f=linspace(-fn,fn-df,n);
else
    %make sure npad is even
    npad=2*ceil(npad/2);
    if(npad>n)
        s=[s;zeros(npad-n,ntraces)];
    end
    iuse=1:n;
    for k=1:length(flags)       
        if flags(k) >0
            nn=tracelengths(k);
%             if (flags(k)==1); mw=mwindow(nn,20);end
%             if (flags(k)==2); mw=mwhalf(nn,20);end
            mw=mwindow2(nn,20,flags(k)-1);
            tmp=s(iuse,k);
            tmp(1:nn)=tmp(1:nn).*mw;
            s(iuse,k)=tmp;          
        end
    end
    s=abs(fft(s,npad));
    spec=s(1:round(npad/2),:);
    if(average>0)
        Aspec=mean(spec,2);
    end
    if(normoption==0)
        amax=max(max(spec));
        db=20*log10(spec/amax);
        if(average>0)
            Adb=20*log10(Aspec/amax);
        end
    else
        db=zeros(size(spec));
        for k=1:ntraces
            db(:,k)=20*log10(spec(:,k)/max(spec(:,k)));
        end
        if(average>0)
            Adb=20*log10(Aspec/max(Aspec));
        end
    end
    dt=t(2)-t(1);
    fn=1./(2*dt);
    f=linspace(0.,fn,round(npad/2));
end
% now plot it
hs=cell(1,ntraces);

if(average~=1)
    for k=1:ntraces
        hline=[];
        hmarker=[];
        if(isempty(lstyles{k}))
            lstyles{k}='-';
        end
        if(isempty(markers{k}))
            markers{k}='none';
        end
        if(~usegray)
            kol=kolors{k};
        else
            kol=gls(k)*[1 1 1];
        end
        if(~strcmp(lstyles{k},'none'))
            hline=plot(f,db(:,k));
            if(k==1); hold on; end
        end
        if(~strcmp(markers{k},'none'))
            ind=1:ievery(k):length(f);
            hmarker=plot(f(ind),db(ind,k),'linestyle','none','marker',markers{k});
            if(k==1); hold on; end
        end
        if(isempty(kol))
            if(~isempty(hline))
                kol=get(hline,'color');
            else
                kol=get(hmatker,'color');
            end
        end
        if(~isempty(hline))
            zd=zorder(k)*ones(size(get(hline,'xdata')));
            set(hline,'color',kol,'linewidth',lws(k)/2,'linestyle',lstyles{k},...
                'markersize',mkrsizes(k),'zdata',zd);
        end
        if(~isempty(hmarker))
            zd=zorder(k)*ones(size(get(hmarker,'xdata')));
            set(hmarker,'color',kol,'markersize',mkrsizes(k),'zdata',zd);
        end
        if(isempty(hline))
            hs{k}=hmarker;
        elseif(isempty(hmarker))
            hs{k}=hline;
        else
            hs{k}=[hline hmarker];
        end
    end
end

if(average~=0)
    ha=plot(f,Adb,'k');
    set(ha,'linewidth',lws(1)/2,'zdata',zd+1);
end
% if(~usegray)
%     hs=plot(f,db);
%     if(othersettings)
%         for k=1:ntraces
%             if(isempty(lstyles{k}))
%                 lstyles{k}='-';
%             end
%             if(isempty(markers{k}))
%                 markers{k}='none';
%             end
%             if(isempty(kolors{k}))
%                 kolors{k}=get(hs(k),'color');
%             end
%             set(hs(k),'linewidth',lws(k)/2,'color',kolors{k},'linestyle',lstyles{k},...
%                 'marker',markers{k},'markersize',mkrsizes(k));
%         end
%     end
% 
% else
%     hs=zeros(1,ntraces);
%     for k=1:ntraces
%         hs(k)=linesgray({f,db(:,k),lstyles{k},lws(k)/2,gls(k),markers{k},mkrsizes(k)});
%     end
% end
xlabel('frequency (Hz)');
ylabel('decibels');
grid;
hold off