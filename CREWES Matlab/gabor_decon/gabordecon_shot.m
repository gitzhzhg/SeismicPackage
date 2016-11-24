function shotgd=gabordecon_shot(shot,t,xrec,xshot,tstart,xstart,twin,tinc,fsmo,tsmo,ihyp,stab)
% GABORDECONSHOT: applies Wiener deoon to a shot record
%
% shotgd=gabordeconshot(shot,t,xrec,xshot,tstart,xstart,twin,tinc,fsmo,tsmo,ihyp,stab)
%
% GABORDECONSHOT applies gabordecon to all traces in a shot gather or to a
% cell array of shots. The deconvolution start time is specified by a
% linear with offset time that should be chosen to track the first breaks.
% Deconvolution is done on a trace by trace basis.
%
% shot ... shot gather as a matrix of traces. Can also be a cell array of
%       shots if a line is being processed.
% t ... time coordinate for shot (must be the same for all traces)
% xrec ... receiver coordinates for the traces in shot. If shot is a cell
%       array then this can be a single vector if all shots have the same
%       receiver coordinates. Otherwish it should be a cell array also.
% xshot ... shot coordinate (one per shot)
% tstart ... length 2 vector of start times matching (roughly) the first
%       breaks at offsets xstart. This is mostly cosmetic and simply avoids
%       deconvolving the traces above the first breaks
% xstart ... length 2 vector of the two offsets (non negative) at which 
%       tstart is prescribed
% twin ... half width of gaussian temporal window (sec)
%  ************** Default = max(t)/10 **************
% tinc ... temporal increment between windows (sec)
%  ************** Default = twin/6 **************
% tsmo ... size of temporal smoother (sec)
% *************** default 2*twin **************
% fsmo ... size of frequency smoother (Hz)
% *************** default 10 **************
% ihyp ... 1 for hyperbolic smoothing, 0 for ordinary boxcar smoothing
%    Hyperbolic smoothing averages the gabor magnitude spectrum along
%    curves of t*f=constant.
% ************** Default = 1 ***********
% stab ... stability constant
%   ************* Default = 0 **************
% 
% shotgd ... deconvolved shot record or cell array of deconvolved shot
%           records
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
if(nargin<7)
    twin=max(t)/10;
end
if(nargin<8)
    tinc=twin/6;
end
if(nargin<9)
    tsmo=2*twin;
end
if(nargin<10)
    fsmo=10;
end
if(nargin<11)
    ihyp=1;
end
if(nargin<12)
    stab=0;
end

nsamps=length(t);

if(~iscell(shot))
    %determine start al all offsets
    xstart=abs(xstart);
    if(diff(xstart)==0)
        error('xstart must give two distinct offsets');
    end
    if(length(tstart)~=2)
        error('tstart must contain two entries');
    end
    if(length(xstart)~=2)
        error('xstart must contain two entries');
    end
    if(length(xshot)~=1)
        error('shot coordinate must be a scalar');
    end
    if(length(t)~=size(shot,1))
        error('invalid t coordinate vector')
    end
    if(length(xrec)~=size(shot,2))
        error('invalid x coordinate vector')
    end
    
    xoff=xrec-xshot;
    
    %define top of gate at all offsets
    vstart=abs(diff(xstart)/diff(tstart));
    t0=abs((tstart(2)*xstart(1)-tstart(1)*xstart(2))/(xstart(1)-xstart(2)));
    tstart=t0+abs(xoff)/vstart;
    
    shotgd=zeros(size(shot));
    small=1000*eps;
    ievery=100;
    for k=1:length(xoff)
        ind=near(t,tstart(k),t(end));%these samples will be deconvolved
        tmp=shot(:,k);
        %check for a mute
        it=find(tmp~=0);
        if(it(1)>ind(1))
            ind=it(1):nsamps;%start with first live sample
        end
        %apply a taper at the beginning
        mw=flipud(mwhalf(length(ind),5));
        if(sum(abs(tmp))>small)%avoid deconvolving a zero trace
            tmpg=gabordecon(tmp(ind),t(ind),twin,tinc,tsmo,fsmo,ihyp,stab);
            shotgd(ind,k)=tmpg.*mw;
        end
        if(rem(k,ievery)==0)
            disp(['finished ' int2str(k) ' traces of ' int2str(length(xoff)) ' total'])
        end
    end
else
    nshots=length(shot);

    if(~iscell(xrec))
        xx=xrec;
        xrec=cell(1,nshots);
        for k=1:nshots
            xrec{k}=xx;
        end
    end
    if(length(t)~=size(shot{1},1))
        error('invalid t coordinate vector')
    end
    if(length(xrec{1})~=size(shot{1},2))
        error('invalid x coordinate vector')
    end
    if(length(xshot)~=nshots)
        error('invalid shot coordinate array')
    end
    
    shotgd=cell(1,nshots);
    small=1000*eps;
    tbegin=clock;
    for j=1:nshots
        xoff=xrec{j}-xshot(j);
    
        %define top of gate at all offsets
        vstart=abs(diff(xstart)/diff(tstart));
        t0=abs((tstart(2)*xstart(1)-tstart(1)*xstart(2))/(xstart(1)-xstart(2)));
        tnot=t0+abs(xoff)/vstart;
        
        tmpshot=zeros(size(shot{j}));
        ievery=100;
        for k=1:length(xoff)
            ind=near(t,tnot(k),t(end));%these samples will be deconvolved
            tmp=shot{j}(:,k);
            %check for a mute
            it=find(tmp~=0);
            if(it(1)>ind(1))
                ind=it(1):nsamps;
            end
            %apply a taper at the beginning
            mw=flipud(mwhalf(length(ind),5));
%             if(k==714)
%                 disp('hey');
%             end
            if(sum(abs(tmp))>small)%avoid deconvolving a zero trace
                tmpg=gabordecon(tmp(ind),t(ind)-t(ind(1)),twin,tinc,tsmo,fsmo,ihyp,stab);
                tmpshot(ind,k)=tmpg.*mw;
            end
            if(rem(k,ievery)==0)
                disp(['finished ' int2str(k) ' traces of ' int2str(length(xoff)) ' total, for shot ' int2str(j)])
            end
        end
%         if(j==5)
%             disp('hey!');
%         end
        shotgd{j}=tmpshot;
        tnow=clock;
        time_used=etime(tnow,tbegin);
        time_per_shot=time_used/j;
        time_remaining=time_per_shot*(nshots-j);
        disp(['shot ' int2str(j) ' of ' num2str(nshots) ' deconvolved'])
        disp(['elapsed time=' num2str(time_used/60) ' min, estimated time remaining=' num2str(time_remaining/60) ' min'])
    end
end
