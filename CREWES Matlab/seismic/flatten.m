function [seisflat,dtau]=flatten(seis,t,x,te,xe,twid)
% 
% [seisflat,dtau]=flatten(seis,t,x,te,xe,twid)
%
% seis ... input seismic section
% t ... time coordinate vector for seis
% x ... space coordinate vector for seis
% NOTE: seis must be a matrix of size nt rows and nx columns where nt is
%   the number of time samples and nx is the number of traces. The t must
%   be a column vector of length nt and x must be a row vector of length
%   nx.
% te ... time of event on seis that will guide the cross correlation to
%       determine the flattening time shifts
% xe ... x coordinate of the guide event.
% NOTE: te and xe must be the same size and must have at least two entries.
% twid ... determines the width of the correlation time window. It does
%        from the even time + or minus 2*twid. So twid is the 1/4 width.
%        It is in seconds.
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

% twid ... half width of event

nx=size(seis,2);
indx=near(x,xe);
indt=near(t,te-2*twid,te+2*twid);
spilot=seis(indt,indx(1));
if(sum(abs(spilot))==0)
    error('pilot is all zeros')
end
dt=t(2)-t(1);

seisflat=zeros(size(seis));
dtau=zeros(1,nx);
nlags=round(twid/dt);

for k=1:nx
    snow=seis(indt,k);
    if(sum(abs(snow))==0)
        dtau(k)=nan;
    else
        cc=maxcorr(spilot,snow,nlags,1);
        dtau(k)=dt*cc(2);
        seisflat(:,k)=stat(seis(:,k),t,dtau(k));
    end
end

ind=find(isnan(dtau));
if(~isempty(ind))
    xx=x;
    dtau(ind)=[];
    xx(ind)=[];
    dtau=interp1(xx,dtau,x);
end