function [shot_nmo,xcmp,offsets,fold]=nmor_cmp(shot,t,x,xshot,vrmsmod,xv,tv,dxcmp,x0cmp,x1cmp,flag,smax,offlims)
% NMOR_CMP: Remove NMO and map to CMP for a shot record
%
% [shot_nmo,xcmp,offsets,fold]=nmor_cmp(shot,t,x,xshot,vrmsmod,xv,tv,dxcmp,x0cmp,x1cmp,flag,smax,offlims)
%
% shot ... input shot record
% t ... time coordinate for shot
% x ... receiver coordinate for shot
% xshot ... source coordinate for shot
% vrmsmod ... rms velocity model for shot
% xv ... x coordinate for vrmsmod
% tv ... time coordinate for vrmsmod
% dxcmp ... cmp increment
% x0cmp ... initial cmp coordinate
% x1cmp ... final cmp coordinate (not used for flag=0)
% flag ... 0 means output cmp range just spans the input range
%         1 means output cmp range extends from x0cmp to x1cmp and ouput
%         gather is padded with zero traces
% smax ... NMO stretch mute (percent). Muting occurs when stretch is
% greater than this value
%  ************ default 30 ****************
% offlims ... length 2 vector where offlims(1) is the minimum absolute-value
%           offset allowed in the stack and offlims(2) is the maximum.
% *************** default offlims=[0 inf] meaning all offsets *************
%
% shot_nmo ... nmo corrected shot with traces mapped to cmp axis. This will
%       be a matrix of length(t) rows and length(xcmp) columns.
% xcmp ... cmp axis for shot_nmo. This is just x0cmp:dxcmp:x1cmp when flag=1.
% offsets ... mean offset of each output trace. When more than one trace
%       falls in a CMP bin, the traces are summed and the mean offset is
%       calculated.
% fold ... fold for shot_nmo (NOTE: shot_nmo is not fold normalized)
%
%
% G.F. Margrave, CREWES Project, August 2013
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
if(nargin<12)
    smax=30;
end

if(length(x)~=size(shot,2))
    error('invalid x coordinate for shot gather')
end
if(length(t)~=size(shot,1))
    error('invalid t coordinate for shot gather')
end
if(length(xv)~=size(vrmsmod,2))
    error('invalid x coordinate for velocity model')
end
if(length(tv)~=size(vrmsmod,1))
    error('invalid t coordinate for velocity model')
end
tv=tv(:);%force column vector
xv=xv(:)';%force row vector

%raw cmps and offsets
xcmpraw=(x+xshot)/2;
xoff=abs(x-xshot);

%expand velocity model if need be to span input cmps
if(min(xv)>min(xcmpraw))||(max(xv)<max(xcmpraw))
    xv=[min(xcmpraw)-dxcmp xv max(xcmpraw)+dxcmp];
    vrmsmod=[vrmsmod(:,1) vrmsmod vrmsmod(:,end)];%repeat first and last traces
end
%expand velocity model in time
if(tv(end)<t(end))
    tv=[tv;t(end)];
    vrmsmod=[vrmsmod;vrmsmod(:,end)];
end
%test to see if we need velocity interpolation in time
test=sum(abs(diff(tv)));
if(test>100*eps)
    interp_t=1;
else
    interp_t=0;
end
%make cmp axis

if(flag==0)
    x1cmp=(round((max(xcmpraw)-x0cmp)/dx)+1)*dx;
end
icmp=round((xcmpraw-x0cmp)/dxcmp)+1;%cmp bin numbers for each trace
xcmpnom=x0cmp:dxcmp:x1cmp;%nominal cmp bin centers from origin to max of this shot
if(flag==0)
    xcmp=xcmpnom(min(icmp):max(icmp));
    i0=min(icmp);%first output bin number
else
    xcmp=xcmpnom;
    i0=1;
end
shot_nmo=zeros(length(t),length(xcmp));
fold=zeros(size(xcmp));
offsets=zeros(size(xcmp));

%loop over input traces
for k=1:length(x)
    %determine velocity for this trace
    ind=surround(xv,xcmpraw(k));
    vrms=(xcmpraw(k)-xv(ind(1)+1))*vrmsmod(:,ind(1))/(xv(ind(1))-xv(ind(1)+1))...
        +(xcmpraw(k)-xv(ind(1)))*vrmsmod(:,ind(1)+1)/(xv(ind(1)+1)-xv(ind(1)));
    if(interp_t)
       vrms=interp1(tv,vrms,t);
    end
    %remove moveout on current trace
    params=nan*ones(1,3);
    params(1)=smax;
    if(xoff(k)>=offlims(1) && xoff(k)<=offlims(2))%test for offset limits
        shot_nmo(:,icmp(k)-i0+1)=nmor(shot(:,k),t,xoff(k),vrms,1,params);
        %the following line calculates the mean offset of the traces falling in a bin
        offsets(icmp(k)-i0+1)=(offsets(icmp(k)-i0+1)*fold(icmp(k)-i0+1)+xoff(k))/(fold(icmp(k)-i0+1)+1);
        fold(icmp(k)-i0+1)=fold(icmp(k)-i0+1)+1;%increment fold
    end
    
end
%normalize by fold
% for k=1:length(xcmp)
%     if(fold(k)>0)
%         shot_nmo(:,k)=shot_nmo(:,k)/fold(k);
%     end
% end
    