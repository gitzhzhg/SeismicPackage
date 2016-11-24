 function [ap,ae,tp,xp]=picker(seis,t,x,te,xe,delt,flag)
% PICKER ... pick amplitudes of seismic events in a 2D seismic matrix
%
% [ap,ae,tp,xp]=picker(seis,t,x,te,xe,delt,flag)
%
% also:
% [methods,flag]=picker('methods'); %gets the list of available picking
%                                   methods (cell array) and their flags
% flag=picker('method2flag',method);%converts a method (string) to a
%                                   numeric flag
% method=picker('flag2method',flag);%converts a numerica flag to a method 
%
% PICKER extracts amplitudes from an event in a seismic matrix. The event
% is defined by a single-valued traveltime trajectory which is a set of
% (x,t) guide points. The picks are made at the x coordinates of the traces
% and to the nearest tenth of the time sample size. The defined trajectory
% is interpreted as piecewise linear between supplied guide points. So a
% linear event need only have its endpoints specified. Trajectories are not
% extrapolated, so if a pick is desired at every trace then the ends of the
% trajectory should equal or exceed the bounds of the seismic. The times
% specified for the trajectory may bounce around as needed to follow some
% perceived event. There are presently (2016) 12 different picking methods
% but this list grows and changes as the code evolves. Therefore, PICKER
% has a special invocation that returns the list of availble picking
% methods and their corresponding flags. Mechanisms are also provided to
% convert a method to a flag and vice-versa.
%
% See also PICKTOOL, which provides an interactive interface to picker.
%
% seis ... seismic matrix
% t ... time coordinate vector for seis (length(t)=size(seis,1))
% x ... x coordinate vector for seis (length(x)=seis(seis,2))
% te ... vector of times defining the event
% xe ... vector of x's defining the event (length(te)=length(xe)). The
%   entries of xe must be monotonic to ensure the event is single valued.
% delt ... halfwidth of fairway around the defined trajectory
%    NOTE: set delt to zero to force the picks to be taken at exactly the 
%       trajectory times. This makes methods 3 and 4 identical and is not
%       available for methods 5, 6, and 7.
% flag ... 1: pick max(abs) amplitude in fairway
%          1.1: pick maximum of the Hilbert envelope in the fairway
%          2: pick closest (to the event) peak of Hilbert envelope in fairway
%          3: pick closest (to the event) peak in fairway
%          3.1: pick largest peak in fairway
%          4: pick closest (to the event) trough in fairway
%          4.1: pick largest (most negative) trough in fairway
%          5: pick closest (to the event) + to - zero crossing in fairway
%          6: pick closest (to the event) - to + zero crossing in fairway
%          7: pick closest (to the event) zero crossing of either polarity
%          8: pick first breaks by sta(env)/lta(env) > threshold
%          9: pick first breaks by max Hilbert envelope in fairway
% 
% ap ... vector of picked amplitudes (for options 5,6,7 this is the
%           amplitude of the Hilbert envelope at the zero crossing)
% ae ... vector of event amplitudes at the same times as the picks
% NOTE: ae and ap are identical for methods 3 and 4
% tp ... vector of pick times
% xp ... vector of pick x coordinates
%
% NOTE: The various methods behave slightly differently if the criteria are
% not met in the fairway (usually happens if delt is too small). 
% For methods 5,6,&7, if no suitable zero crossing is found in the fairway  
% then the pick is assigned nan (not a number). This will cause the graphic 
% display to be blank for that pick. For methods 3 and 4, if no peak is found 
% then the pick is assigned to the maximum value in the fairway. For method 4, 
% the pick is assigned to the minimum if no trough is found.
%
% FIRST BREAK PICKING. Method 8 is the common sta/lta method of first break
% picking. Here sta and lta stand for short term average and long term
% average of the Hilbert envelope. Given suitable definitions of short term
% and long term, the method taks a pick where the ratio=sta/lta exceeds a
% threshold. Both averages are calculated in a causal fashion by averageing
% the n samples after a given time. The number of samples averaged is
% typically 10 times greater for the lta than the sta. The lta average is
% set at 10% of the fairway width (i.e. 2*delt/10=delt/5). The ratio is
% actually calculated as ratio=g.*sta./(lta+stab*ltamax). Here the
% denominator is stabilized by the common technique of adding a small
% number stab*ltamax to lta where stab is about .1 and ltamax=max(lta). The
% factor g in the ratio is a gaussian window centered on the event time
% (defined by te and xe) and having a standard deviation of .5*delt. The
% gaussian causes the picks to be preferred near the event. Both the
% stabilization and the gaussian help stabilize the picking. The default
% behavior of this method can be controlled by defining the three global
% variables THRESH TIMES STAB. THRESH defaults to 1.0. Making it larger may
% reduce wild picks, while smaller may cause more picks to be made.
% Additionally smaller makes the picks earlier and larger makes them
% later. TIMES refers to the ratio of the averaging interval for lta versus
% sta. TIMES defaults to 10, meaning the lta is 10 times longer than the
% sta. STAB defines the stability constant used in the denominator and
% defaults to 0.1
%
% G.F. Margrave, 2007-16, CREWES
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

%test for method autoidentify
if(ischar(seis))
    if(strcmp(seis,'methods'))
        nmethods=11;
        methods=cell(1,nmethods);
        methods{1}='pick max(abs) amplitude in fairway';
        methods{2}='pick maximum of the Hilbert envelope in the fairway';
        methods{3}='pick closest (to the event) peak of Hilbert envelope in fairway';
        methods{4}='pick closest (to the event) peak in fairway';
        methods{5}='pick largest peak in fairway';
        methods{6}='pick closest (to the event) trough in fairway';
        methods{7}='pick largest (most negative) trough in fairway';
        methods{8}='pick closest (to the event) + to - zero crossing in fairway';
        methods{9}='pick closest (to the event) - to + zero crossing in fairway';
        methods{10}='pick closest (to the event) zero crossing of either polarity';
        methods{11}='pick first breaks by sta(env)/lta(env) > threshold';
        methods{12}='pick first breaks by max Hilbert envelope in fairway';
        flags=[1 1.1 2 3 3.1 4 4.1 5 6 7 8 9];
        ap=methods;
        ae=flags;
        tp=[];
        xp=[];
        return;
    end
    if(strcmp(seis,'method2flag'))
        %convert a method to a flag
        % syntax: flag=picker('method2flag',method)
        %
        method=t;
        flag=[];
        [methods,flags]=picker('methods');
        for k=1:length(methods)
            if(strcmp(methods{k},method))
                flag=flags(k);
            end
        end
        ap=flag;
        return
    end
    if(strcmp(seis,'flag2method'))
        %convert a flag to a method
        % syntax: method=picker('flag2method',flag)
        %
        flag=t;
        method=[];
        [methods,flags]=picker('methods');
        for k=1:length(methods)
            if(flags(k)==flag)
                method=methods{k};
            end
        end
        ap=method;
        return
    end
end


global THRESH TIMES STAB

[nt,nx]=size(seis);
if(length(t)~=nt)
    error('time vector is the wrong size')
end
if(length(x)~=nx)
    error('coordinate vector is wrong size')
end
if(length(te)~=length(xe))
    error('te and xe must be the same length')
end
if(prod(diff(xe))<0)
    error('xe must be monotonic')
end

dt=t(2)-t(1);
if(delt==0)
    method='exact';
    delt=5*dt;%a nominal fairway
else
    method='interp';
end

if(delt<dt)
    warning('fairway specified is less than the sample size, rounding up')
end

te=te(:);%force column vector

%determine the part of the trajectory that lies in the matrix
tmin=min(t);
tmax=max(t);
xmin=min(x);
xmax=max(x);
ind=find((te<tmin) | (te>tmax));
if(~isempty(ind))
    te(ind)=[];
    xe(ind)=[];
end
ind=find((xe<xmin) | (xe>xmax));
if(~isempty(ind))
    te(ind)=[];
    xe(ind)=[];
end
if(isempty(te))
    ap=[];
    ae=[];
    tp=[];
    xp=[];
    return
end

%slice the matrix
indx=between(min(xe),max(xe),x,2);
xp=x(indx);
te_int=pwlint(xe,te,x(indx));%interpolate times at each trace
te_int=te_int(:);%column vector

te_int=(round((te_int-t(1))/dt)+1)*dt+t(1);%force te_int to fall exactly on a sample

samps=slicemat(seis(:,indx),(te_int-t(1))/dt+1,ceil(delt/dt));%grab the fairway slice
[nt2,nx2]=size(samps);
nh=(nt2-1)/2;
trel=(-nh:nh)*dt;%relative time of slice
%interpolate to 10 times the samples. So, pick times will be to nearest
%neighbor but at one-tenth the sample rate
dt2=dt/20;
trel2=(trel(1):dt2:trel(end))';%column vector
samps2=zeros(length(trel2),nx2);
for k=1:nx2
    samps2(:,k)=spline(trel,samps(:,k),trel2);
end
switch method
    case {'exact'} %'exact means we are forcing the picks to be at the supplied event times
        switch flag
            case {1}
                ap=zeros(nx2,1);
                ae=ap;
                tp=ap;
                for k=1:nx2
                    tabs=round(te_int(k)/dt)*dt+trel2;
%                     imax=find(max(abs(samps2(:,k))));
%                     tp(k)=tabs(imax);
%                     ae(k)=abs(samps2(:,imax));
%                     ap(k)=samps2(:,imax);
                    ae(k)=spline(tabs,samps2(:,k),te_int(k));
                    ap(k)=abs(ae(k));
                    tp(k)=te_int(k);
                end
            case {2}
                ap=zeros(nx2,1);
                ae=ap;
                tp=ap;
                for k=1:nx2
                    tabs=round(te_int(k)/dt)*dt+trel2;
                    env=abs(hilbert(samps2(:,k)));
                    ae(k)=spline(tabs,samps2(:,k),te_int(k));
                    ap(k)=spline(tabs,env,te_int(k));
                    tp(k)=te_int(k);
                end
            case {3,4}
                ap=zeros(nx2,1);
                ae=ap;
                tp=ap;
                for k=1:nx2
                    tabs=round(te_int(k)/dt)*dt+trel2;
                    ae(k)=spline(tabs,samps2(:,k),te_int(k));
                    ap(k)=ae(k);
                    tp(k)=te_int(k);
                end
            case {5,6,7,8,9}
                msgbox('Exact picking not possible for cases 5,6,7,8,9')
            otherwise
                error('unknown picking method')
        end
    case {'interp'}
        switch flag
            case {1}
                [ap,ind]=max(abs(samps2));%find max abs value on each trace
                ae=zeros(nx2,1);
                for k=1:nx2
                    ae(k)=samps2(ind(k),k); %extract the true amp at the location of max_abs
                end
                tp=te_int+trel2(ind);%true time of the pick
            case {1.1}
                ae=zeros(nx2,1);
                ap=ae;
                tp=ae;
                for k=1:nx2
                    env=abs(hilbert(samps2(:,k)));
                    [ap(k),ind]=max(env);%envelope maximum
                    ae(k)=samps2(ind,k); %extract the true amp at the location of max_abs
                    tp(k)=te_int(k)+trel2(ind);%true time of the pick
                end
            case {2}
                ap=zeros(nx2,1);
                ae=ap;
                tp=ap;
                for k=1:nx2
                    env=abs(hilbert(samps2(:,k)));
                    iex=findex(env,1);
                    if(~isempty(iex))
                        it=near(trel2(iex),0);%find closest to the event time
                        ap(k)=env(iex(it(1)));
                        ae(k)=samps2(iex(it(1)),k);
                        tp(k)=te_int(k)+trel2(iex(it(1)));
                    else
                        %[ap(k),ind]=nan;
                        %tp(k)=te_int(k)+trel2(ind(1));
                        %ae(k)=samps2(ind(1),k);
                        ap(k)=nan;
                        tp(k)=nan;
                        ae(k)=nan;
                    end
                end
            case {3}
                ae=zeros(nx2,1);
                ap=ae;
                tp=ae;
                for k=1:nx2
                    iex=findex(samps2(:,k),1);%find peaks
                    if(~isempty(iex))
                        it=near(trel2(iex),0);%find closest to the event time
                        ap(k)=samps2(iex(it(1)),k);
                        tp(k)=te_int(k)+trel2(iex(it(1)));
                        ae(k)=ap(k);
                    else
%                         [ap(k),ind]=max(samps2(:,k));
%                         tp(k)=te_int(k)+trel2(ind(1));
%                         ae(k)=ap(k);
                        ap(k)=nan;
                        tp(k)=nan;
                        ae(k)=nan;
                    end
                end
            case {3.1}
                ae=zeros(nx2,1);
                ap=ae;
                tp=ae;
                for k=1:nx2
                    iex=findex(samps2(:,k),1);%find peaks
                    if(~isempty(iex))
                        [ap(k),it]=max(samps2(iex,k));
                        tp(k)=te_int(k)+trel2(iex(it(1)));
                        ae(k)=ap(k);
                    else
%                         [ap(k),ind]=max(samps2(:,k));
%                         tp(k)=te_int(k)+trel2(ind(1));
%                         ae(k)=ap(k);
                        ap(k)=nan;
                        tp(k)=nan;
                        ae(k)=nan;
                    end
                end
            case {4}
                ae=zeros(nx2,1);
                ap=ae;
                tp=ae;
                for k=1:nx2
                    iex=findex(samps2(:,k),-1);%find troughs
                    if(~isempty(iex))
                        it=near(trel2(iex),0);%find closest to the event time
                        ap(k)=samps2(iex(it(1)),k);
                        tp(k)=te_int(k)+trel2(iex(it(1)));
                        ae(k)=ap(k);
                    else
%                         [ap(k),ind]=min(samps2(:,k));
%                         tp(k)=te_int(k)+trel2(ind(1));
%                         ae(k)=ap(k);
                        ap(k)=nan;
                        tp(k)=nan;
                        ae(k)=nan;
                    end
                end
            case {4.1}
                ae=zeros(nx2,1);
                ap=ae;
                tp=ae;
                for k=1:nx2
                    iex=findex(samps2(:,k),-1);%find troughs
                    if(~isempty(iex))
                        [ap(k),it]=min(samps2(iex,k));
                        tp(k)=te_int(k)+trel2(iex(it(1)));
                        ae(k)=ap(k);
                    else
%                         [ap(k),ind]=min(samps2(:,k));
%                         tp(k)=te_int(k)+trel2(ind(1));
%                         ae(k)=ap(k);
                        ap(k)=nan;
                        tp(k)=nan;
                        ae(k)=nan;
                    end
                end
            case {5}
                ap=zeros(nx2,1);
                ae=ap;
                tp=ap;
                for k=1:nx2
                    ind_xc=find_zero_crossings(samps2(:,k),1);
                    env=abs(hilbert(samps2(:,k)));
                    if(~isempty(ind_xc))
                        it=near(trel2(ind_xc),0);%find closest to the event time
                        tp(k)=te_int(k)+trel2(ind_xc(it(1)));
                        ae(k)=samps2(ind_xc(it(1)),k);
                        ap(k)=env(ind_xc(it(1)));
                    else
                        tp(k)=nan;
                        ae(k)=nan;
                        ap(k)=nan;
                    end
                end
            case {6}
                ap=zeros(nx2,1);
                ae=ap;
                tp=ap;
                for k=1:nx2
                    ind_xc=find_zero_crossings(samps2(:,k),-1);
                    env=abs(hilbert(samps2(:,k)));
                    if(~isempty(ind_xc))
                        it=near(trel2(ind_xc),0);%find closest to the event time
                        tp(k)=te_int(k)+trel2(ind_xc(it(1)));
                        ae(k)=samps2(ind_xc(it(1)),k);
                        ap(k)=env(ind_xc(it(1)));
                    else
                        tp(k)=nan;
                        ae(k)=nan;
                        ap(k)=nan;
                    end
                end
            case {7}
                ap=zeros(nx2,1);
                ae=ap;
                tp=ap;
                for k=1:nx2
                    ind_xc=find_zero_crossings(samps2(:,k),0);
                    env=abs(hilbert(samps2(:,k)));
                    if(~isempty(ind_xc))
                        it=near(trel2(ind_xc),0);%find closest to the event time
                        tp(k)=te_int(k)+trel2(ind_xc(it(1)));
                        ae(k)=samps2(ind_xc(it(1)),k);
                        ap(k)=env(ind_xc(it(1)));
                    else
                        tp(k)=nan;
                        ae(k)=nan;
                        ap(k)=nan;
                    end
                end
            case {8}
                if(isempty(THRESH))
                    thresh=1.0;
                else
                    thresh=THRESH;
                end
                if(isempty(TIMES))
                    times=10;%the lta will be this many times longer than the sta
                else
                    times=TIMES;
                end
                if(isempty(STAB))
                    stab=.1;
                else
                    stab=STAB;
                end
                %define a Gaussian to be used in weighting the picks to
                %favor those near the center of the fairway
                sigma=delt/2;%standard deviation is 1/4 of the fairway width
                g=exp(-((trel2).^2/sigma^2));%g max is at trel==0 which should be the defined event time
                ap=zeros(nx2,1);
                ae=ap;
                tp=ap;
                for k=1:nx2

                    env=abs(hilbert(samps2(:,k)));
                    %compute sta/lta
                    nshort=max([round(length(env)/(10*times)) 2]);%at least two samples in sta 
                    nlong=min([times*nshort length(env)]);%length of lta (long term average)
                    shortones=ones(1,nshort);
                    longones=ones(1,nlong);
                    sta=convm(env,shortones)/nshort;
                    lta=convm(env,longones)/nlong;
                    ltamax=max(lta);
                    ratio=g.*sta./(lta+stab*ltamax);
                    ind=find(ratio>thresh);
                        
%                     if(k==201)
%                         honk=1;
%                     end

                    if(~isempty(ind)&&ind(1)~=1)
                        %it=near(trel2(ind(1)),0);%find closest to the event time
                        ap(k)=samps2(ind(1),k);
                        ae(k)=samps2(ind(1),k);
                        tp(k)=te_int(k)+trel2(ind(1));
                    else
                        %ind=near(trel2,0);
                        %                         ap(k)=env(ind(1));
                        %                         tp(k)=te_int(k)+trel2(ind(1));
                        %                         ae(k)=samps2(ind(1),k);
                        ap(k)=nan;
                        tp(k)=nan;
                        ae(k)=nan;
                    end
                end
            case {9}
                %define a Gaussian to be used in weighting the picks to
                %favor those near the center of the fairway
                sigma=delt/2;%standard deviation is 1/4 of the fairway width
                g=exp(-((trel2).^2/sigma^2));%g max is at trel==0 which should be the defined event time
                ap=zeros(nx2,1);
                ae=ap;
                tp=ap;
                for k=1:nx2
                    env=g.*abs(hilbert(samps2(:,k)));
%                     sif=ins_freq(samps2(:,k),trel2);
%                     test=env.*sif;
%                     bkgrd=mean(test);
%                     ind=find(test>thresh*bkgrd);
                    [em,ind]=max(env);
                    
                    if(~isempty(ind)&&ind(1)~=1)
                        %it=near(trel2(ind(1)),0);%find closest to the event time
                        ap(k)=samps2(ind(1),k);
                        ae(k)=samps2(ind(1),k);
                        tp(k)=te_int(k)+trel2(ind(1));
                    else
                        ap(k)=nan;
                        tp(k)=nan;
                        ae(k)=nan;
                    end
                end
            otherwise
                error('unknown picking method')
        end
end