function [quimp,tq,b]=einar(Q,x,cnot,dt,tmax,flag,frac_shift)
%EINAR: generate a constant Q impulse response
%
% [quimp,tq]=einar(Q,x,cnot,dt,tmax,flag,frac_shift)
%
% Generate a constant Q impulse response. Based on Einar Kjartanssen's
% paper: Kjartansson, E., 1979, Constant Q-wave propagation and attenuation: 
%        Journal of Geophysical Research, 84, 4737-4748.
%
% Q ... Q
% x ... distance traveled
% cnot ... high frequency phase velocity
% dt ... time sample rate
% tmax ... maximum time output
% flag ... 0 means the wavlet is time shifted to the origin
%          1 means the wavelet is time delayed according to the propagation
%           distance
%   ************** default 1 ************
% frac_shift ... a number between 0 and 1 that is used only when flag=0.
%       frac_shift=1 removed the full timme delay at computed at cnot while
%       frac_shift=.9 removes only 90% of the delay.
%   ************** default = .95 **********
%
% quimp ... impulse response (in retarded time or real time... see flag)
% tq ... time vector for quimp
%
% G.F. Margrave, CREWES, 1999
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
    frac_shift=.95;
end
if(nargin<6)
    flag=1;
end

%double tmax as a kind of zero pad
tmax2=tmax*2;

%fnot= f(length(f));
tq=(0:dt:tmax)';
tq2=(0:dt:tmax2)';

%Find a power of 2 thats at least 4 times the length of tq
nf=length(tq2);
while(nf < 4*length(tq2) )
	nf = 2^nextpow2(nf+1);
end

%build frequency vector
fnyq=1/(2*dt);
df=1/((nf-1)*dt);
f=0:df:2*fnyq;
ind=nf/2 + 1;
f=[f(1:ind) -f(ind-1:-1:2)];

%amplitude spectrum
B= exp(-x*pi*abs(f)/(Q*cnot));

%phase velocity
cw=cnot*(1+log(abs(f(2:length(f)))/fnyq)/(pi*Q));
cw=[cw(1) cw];

%complex spectrum
if(flag)
    B = B.*exp(-1i*2*pi*f.*x./cw);
else
    B = B.*exp(1i*2*pi*f.*(frac_shift*x/cnot - x./cw));
end

%to time domain
b=real(ifft(B))';

%apply window
quimp=b(1:length(tq));