function [amat,t,x]=makehyps(dt,dx,tmax,xmax,v,w,tw)
% [amat,t,x]=makehyps(dt,dx,tmax,xmax,v,w,tw)
%
% dt ... desired time sample rate
% dx ... desired space sample rate
% tmax ... maximum time desired in seconds
% xmax ... length of section in physical units
% v ... velocity for hyperbolas
% w ... wavelet
% tw ... wavelet time coordinate
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
x=0:dx:xmax;
t=0:dt:tmax;
xmax=x(length(x));
xhyps=linspace(0,xmax,10);
thyps = .6*tmax;
amat=zeros(length(t),length(x));
%hyps
for k=1:length(xhyps)
	amat=event_hyp(amat,t,x,thyps,xhyps(k),v/2,1);
end
amat=sectconv(amat,t,w,tw);