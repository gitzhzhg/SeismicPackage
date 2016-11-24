
function [alf,f,tau]=tvf_xfer(dt,tmax,ts,fmins,fmaxs,phases,ninterp)
%
% [alf,f,tau]=tvf_xfer(dt,tmax,ts,fmins,fmaxs,phases,ninterp)
%
% Design a time variant filter transfer function
%
% dt ... time sample rate
% tmax ... temporal length of trace(s) to be filtered
% ts ... times at which filter specs apply.
% fmins ... minimum frequencies for each ts
%     if n=length(ts) then fmins can be size (n,2) where the
%         second column contains filter gaussian rolloff widths in Hz
%     widths default to 5 Hz
% fmaxs ... maximum frequencies for each ts
%     if n=length(ts) then fmaxs can be size (n,2) where the
%         second column contains filter gaussian rolloff widths in Hz
%     widths default to 10 Hz
% phases ... phase flags for each ts (0->zero; 1-> minimum;
%         Other numbers are interpreted as a constant phase rotation
%     in degrees
%     ******** default is zeros(size(ts)) *********
% ninterp ... interpolate parameters tp a temporal space in ninterp*dt
%               BEFORE filter design. This will result in more accurate phase
%               for non-zero phase designs.
%       ********* defualt is 0 (no action) ************
%
% G.F. Margrave, CREWES, Aug 1996
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

if(nargin<6)
        phases=zeros(size(phases));
end
if(nargin<7)
        ninterp=0;
end

%make a tau axis
tau=0:dt:tmax;

%make sure ts spans tau
ts=ts(:);phases=phases(:);
if(ts(1)~=0.0)
        ts=[0;ts];
        fmins=[fmins(1,:);fmins];
        fmaxs=[fmaxs(1,:);fmaxs];
        phases=[phases(1);phases];
end
if(ts(length(ts))~=tau(length(tau)))
        nts=length(ts);
        ts=[ts;tau(length(tau))];
        fmins=[fmins;fmins(nts,:)];
        fmaxs=[fmaxs;fmaxs(nts,:)];
        phases=[phases;phases(nts)];
end

%expand ts if ninterp~=0
if(ninterp~=0)
        nts=length(ts);
        tsnew= (ts(1):ninterp*dt:ts(nts))';
        %make sure old ts's are in the new set
        tsnew= sort([tsnew;ts]);
        test=diff(tsnew);
        ind=find(test==0.0);
        if(ind~=[])
                tsnew(ind)=[];
        end
        nnew=length(tsnew);
        fm1=pwlint(ts,fmins(:,1),tsnew);
        fm2=pwlint(ts,fmins(:,2),tsnew);
        fmins=[fm1 fm2];
        fm1=pwlint(ts,fmaxs(:,1),tsnew);
        fm2=pwlint(ts,fmaxs(:,2),tsnew);
        fmaxs=[fm1 fm2];
        phases=pwlint(ts,phases,tsnew);
        ts=tsnew;
end

nts=length(ts);
its=round(ts/dt)+1;
%compute filters at specified locations
for k=1:nts
        [tmp,f]=filtspec(dt,tmax,fmins(k,:),fmaxs(k,:),phases(k));
        if(k==1)
                alf=zeros(length(f),length(tau));
        end
        alf(:,its(k))=tmp(:);
end

%interpolate between filters
for k=1:nts-1
        a1=abs(alf(:,its(k)));
        a2=abs(alf(:,its(k+1)));
        p1=alf(:,its(k))./a1;
        p2=alf(:,its(k+1))./a2;

        for kk=its(k)+1:its(k+1)-1;
                fact= (kk-its(k))/(its(k+1)-its(k));
                atmp= a1*(1-fact)+fact*a2;
                ptmp= p1*(1-fact)+fact*p2;
                alf(:,kk)=atmp.*ptmp;
        end
end