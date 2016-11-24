function [stp,tau,p]=tptran(seis,t,x,pmin,pmax,dp)
%TPTRAN ... forward tau-p transform (linear trajectories)
%
% [stp,tau,p]=tptran(seis,t,x,pmin,pmax,dp)
%
% The forward tau-p transform is computed in the space-frequency domain.
%
% seis ... input seismic matrix or shot record
% t ... time (row) coordinate for seis
% x ... space (column) coordinate for seis
% NOTE: size(seis,1) must equal size(t,1) and size(seis,2) must equal size(x,2)
% pmin ... minimum slowness value to steer over
% pmax ... maximum slowness value to steer over
% dp ... slowness increment
% ******** default = 0.5*(pmax-pmin)/(length(x)-1) ********
% NOTE: The default for dp gives twice as many p traces as x traces.  The
% corresponding default in itptran gives half as many x traces as p traces.
%
% stp ... tau-p transform of seis
% tau ... row coordinate of stp
% p   ... column coordinate of stp
% 
%
% by G.F. Margrave, September 2014
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

if(size(seis,1)~=size(t,1))
    error('t vector incompatible with seis')
end
if(size(seis,2)~=size(x,2))
    error('x vector incompatible with seis')
end

if(nargin<6)
    dp=0.5*(pmax-pmin)/(length(x)-1);
end

[nt,nx]=size(seis);
nt2=2^nextpow2(nt);
if(nt2>nt)
    seis=[seis;zeros(nt2-nt,nx)];
    tau=((t(2)-t(1))*(0:nt2-1))';
else
    tau=t;
end
%Transform over time
[seisf,f]=fftrl(seis,tau);%seisf is in (x,f) space
p=pmin:dp:pmax;%slowness values to steer over
np=length(p);
stp=zeros(nt2,np);%preallocate space for tau-p transform
%loop over p values
for k=1:length(p)
    dtx=p(k)*x;
    %calculate phase shift operator to flatten linear events
    shiftr=exp(1i*2.*pi*f*dtx);
    %apply the phase shift and then sum (stack) traces
    trcf=sum(seisf.*shiftr,2);
    trcf(end)=real(trcf(end));%make sure Nyquist is real
    stp(:,k)=ifftrl(trcf,f);%inverse transform
end
stp=stp/length(p);