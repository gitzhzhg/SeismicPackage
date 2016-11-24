function [shotd,tstart,tend]=deconw_shot3D(shot,t,xoff,tstart,tend,xd,top,stab)
% DECONWSHOT3D: applies Wiener decon to a shot record
%
% [shotd,tstart,tend]=deconw_shot3D(shot,t,xoff,tstart,tend,xd,top,stab)
%
% DECONW_SHOT3D applies deconw (Wiener deconvolution) to all traces in a 3D shot
% gather or to a cell array of shots. The deconvolution design gate is
% specified by hyperbolic (with offset) start and end times. In the event
% that the width of the design gate drops below 2*top (twice the decon
% operator length) then the bottom of the gate is automatically extended to
% ensure this minimum size. Deconvolution is done on a trace by trace
% basis. This only difference between this and deconw_shot is that here the
% offsets are input directly wheras in deconw_shot they are computed from
% input shot and receiver coordinates. So this code will also work  fine in
% 2D.
%
% shot ... shot gather as a matrix of traces. Can also be a cell array of
%       shots if a line is being processed.
% t ... time coordinate for shot
% xoff ... offsets for the traces in shot. If shot is a cell
%       array then this can be a single vector if all shots have the same
%       receiver coordinates. Otherwish it should be a cell array also.
% tstart ... length 2 vector of design window start times
% tend ... length 2 vector of design window end times
% xd ... length 2 vector of the two offsets at which tstart and tend are
%       prescribed
% top ... length of decon operator in seconds
%  ****** default 0.1 ******
% stab ... decon white noise (stab) factor
%  ****** default =.001 *****
% 
% shotd ... deconvolved shot record or cell array of deconvolved shot
%           records
% tstart ... length(xrec) vector of decon gate start times. If shotd is a
%           cell array then this will be also giving the decon sstart gate
%           for each shot.
% tend ... length(xrec) vector of decon gate end times. Will be a cell
%           array if shotd is.
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
if(nargin<8)
    stab=.001;
end
if(nargin<7)
    top=.1;
end

if(~iscell(shot))
    %determine hyperbolae for start and end of design window
    xd=abs(xd);
    if(diff(xd)==0)
        error('xd must give two distinct offsets');
    end
    if(length(tstart)~=2)
        error('tstart must contain two entries');
    end
    if(length(tend)~=2)
        error('tend must contain two entries');
    end
    if(length(xd)~=2)
        error('xd must contain two entries');
    end
    if(length(t)~=size(shot,1))
        error('invalid t coordinate vector')
    end
    if(length(xoff)~=size(shot,2))
        error('invalid offset vector')
    end
    
    vstart=sqrt((xd(2)^2-xd(1)^2)/(tstart(2)^2-tstart(1)^2));
    vend=sqrt((xd(2)^2-xd(1)^2)/(tend(2)^2-tend(1)^2));
    t0start=sqrt(tstart(1)^2-xd(1)^2/vstart^2);
    t0end=sqrt(tend(1)^2-xd(1)^2/vend^2);
    %define top of gate at all offsets
    tstart=sqrt(t0start^2+xoff.^2/vstart^2);
    %define bottom of gate at all offsets
    tend=sqrt(t0end^2+xoff.^2/vend^2);
    
    %determine operator length in samples
    dt=t(2)-t(1);
    nop=round(top/dt);
    
    shotd=zeros(size(shot));
    small=1000*eps;
    for k=1:length(xoff)
        tmp=shot(:,k);
        if(sum(abs(tmp))>small)%avoid deconvolving a zero trace
            idesign=near(t,tstart(k),tend(k));
            if(length(idesign)<2*nop)
                idesign=[idesign idesign(end)+1:2*nop];%pad out to 2*nop samples
                %adjust tend(k)
                tend(k)=tstart(k)+2*nop*dt;
            end
            mw=mwindow(length(idesign),10);
            shotd(:,k)=deconw(tmp,tmp(idesign).*mw,nop,stab);
        end
    end
else
    nshots=length(shot);

    if(~iscell(xoff))
        xx=xoff;
        xoff=cell(1,nshots);
        for k=1:nshots
            xoff{k}=xx;
        end
    end
    if(length(t)~=size(shot{1},1))
        error('invalid t coordinate vector')
    end
    if(length(xoff{1})~=size(shot{1},2))
        error('invalid offset vector')
    end
    if(length(xshot)~=nshots)
        error('invalid shot coordinate array')
    end
    
    shotd=cell(1,nshots);
    ttop=tstart;
    tbot=tend;
    tstart=cell(1,nshots);
    tend=tstart;
    for j=1:nshots
        xoffj=xoff{j};
        
        vstart=sqrt((xd(2)^2-xd(1)^2)/(ttop(2)^2-ttop(1)^2));
        vend=sqrt((xd(2)^2-xd(1)^2)/(tbot(2)^2-tbot(1)^2));
        t0start=sqrt(ttop(1)^2-xd(1)^2/vstart^2);
        t0end=sqrt(tbot(1)^2-xd(1)^2/vend^2);
        %define top of gate at all offsets
        tstart{j}=sqrt(t0start^2+xoffj.^2/vstart^2);
        %define bottom of gate at all offsets
        tend{j}=sqrt(t0end^2+xoffj.^2/vend^2);
        
        %determine operator length in samples
        dt=t(2)-t(1);
        nop=round(top/dt);
        
        tmpshot=zeros(size(shot{j}));
        small=1000*eps;
        for k=1:length(xoffj)
            tmp=shot{j}(:,k);
            if(sum(abs(tmp))>small)%avod deconvolving a zero trace
                idesign=near(t,tstart{j}(k),tend{j}(k));
                if(length(idesign)<2*nop)
                    idesign=[idesign idesign(end)+1:2*nop];%pad out to 2*nop samples
                    %adjust tend(k)
                    tend{j}(k)=tstart{j}(k)+2*nop*dt;
                end
                mw=mwindow(length(idesign),10);
                tmpshot(:,k)=deconw(tmp,tmp(idesign).*mw,nop,stab);
            end
        end
        shotd{j}=tmpshot;
        disp(['shot ' int2str(j) ' of ' num2str(nshots) ' deconvolved'])
    end
end
