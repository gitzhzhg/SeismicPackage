function stackingchart(xshot,xrec,dx)
% STACKINGCHART ... plots seismic line geometry
% 
% stackingchart(xshot,xrec)
%
% xshot ... vector of shot coordinates
% xrec ... cell array of receiver coordinates
%   Requirement: length(xshot) must equal length(xrec)
% dx ... cmp binning interval
%  **** default half of the median receiver spacing *****
% 
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
if(length(xshot)~=length(xrec))
    error('xshot and xrec must be the same length')
end

if(~iscell(xrec))
    error('xrec must be a cell array');
end

if(iscell(xshot))
    error('xshot must not be a cell array')
end
xcmpmin=10^20;
xcmpmax=-xcmpmin;
if(nargin<3)
    dxs=zeros(size(xshot));
    for k=1:length(xshot)
        dxs(k)=mean(diff(xrec{k}));
        xcmps=.5*(xshot(k)+xrec{k});
        if(min(xcmps)<xcmpmin)
            xcmpmin=min(xcmps);
        end
        if(max(xcmps)>xcmpmax)
            xcmpmax=max(xcmps);
        end
    end
    dx=mean(dxs)/2;
end
xcmp=xcmpmin:dx:xcmpmax;
figure
nshots=length(xshot);
for k=1:nshots
    h=line(xrec{k},xshot(k)*ones(size(xrec{k})));
    set(h,'linestyle','none','marker','.','color','r');
    h=line(xshot(k),xshot(k));
    set(h,'linestyle','none','marker','*');
end
xlabel('Receiver coordinate')
ylabel('Shot coordinate')
title('Shot-Receiver Chart')

figure
fold=zeros(size(xcmp));
for k=1:nshots
    xmid=(xshot(k)+xrec{k})/2;
    xoff=abs(xrec{k}-xshot(k));
    h=line(xmid,xoff);
    set(h,'linestyle','none','marker','x');
%     h=line(xshot(k),xshot(k));
%     set(h,'linestyle','none','marker','*');
    %count fold
    icmp=ceil((xmid-xcmpmin)/dx);
    for kk=1:length(icmp)
        if(icmp(kk)>0 && icmp(kk)<=length(fold))
            fold(icmp(kk))=fold(icmp(kk))+1;
        end
    end
end
xlabel('Midpoint coordinate')
ylabel('Offset coordinate')
title('Midpoint-Offset Chart')

figure
plot(xcmp,fold)
title('CMP Fold')
xlabel('cmp coordinate')