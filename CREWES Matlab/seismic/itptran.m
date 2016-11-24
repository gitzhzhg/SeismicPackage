function [seis,t,x]=itptran(stp,tau,p,xmin,xmax,dx)
%ITPTRAN ... inverse tau-p transform (linear trajectories)
%
% [seis,t,x]=itptran(stp,tau,p,xmin,xmax,dx)
%
% The inverse tau-p transform is computed in the space-frequency domain.
% The algorithm is filtered back-projection.
%
% stp ... input tau-p transform
% tau ... time (row) coordinate for stp
% p ... slowness (column) coordinate for stp
% NOTE: size(stp,1) must equal size(tau,1) and size(stp,2) must equal size(p,2)
% xmin ... minimum x value for output
% pmax ... maximum x value for output
% dx ... spatial increment
% ******** default = 2*(xmax-xmin)/(length(p)-1) ********
% NOTE: The default for dx gives have as many x traces as p traces.  The
% corresponding default in tptran gives twice as many p traces as x traces.
%
% seis ... output seismic gather
% t ... row coordinate of seis
% x   ... column (space) coordinate of seis
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

if(size(stp,1)~=size(tau,1))
    error('tau vector incompatible with stp')
end
if(size(stp,2)~=size(p,2))
    error('p vector incompatible with stp')
end

if(nargin<6)
    dx=2*(xmax-xmin)/(length(p)-1);
end

[nt,np]=size(stp);
nt2=2^nextpow2(nt);
if(nt2>nt)
    stp=[stp;zeros(nt2-nt,np)];
    t=(tau(2)-tau(1))*(0:nt2-1);
else
    t=tau;
end
[stpf,f]=fftrl(stp,tau);
x=xmin:dx:xmax;
nx=length(x);
seis=zeros(nt2,nx);
%loop over x values
for k=1:length(x)
    dtx=x(k)*p;
    %calculate phase shift operator to flatten linear events
    shiftr=exp(-1i*2.*pi*f*dtx);
    %apply the phase shift and then sum (stack) traces
    trcf=f.*sum(stpf.*shiftr,2);
    trcf(end)=real(trcf(end));%make sure Nyquist is real
    seis(:,k)=ifftrl(trcf,f);%inverse transform
end
seis=2*pi*seis/length(x);