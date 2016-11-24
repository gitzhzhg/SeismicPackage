function [quimp,tq,b]=einar(Q,x,cnot,dt,tmax)
% [quimp,tq]=einar(Q,x,cnot,dt,tmax)
%
% Generate a constant Q impulse response
%
% Q ... Q
% x ... distance traveled
% cnot ... high frequency phase velocity
% dt ... time sample rate
% tmax ... maximum time output
% quimp ... impulse response (in retarded time)
% tq ... time vector for quimp
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
%fnot= f(length(f));
tq=0:dt:tmax;
%Find a power of 2 thats at least 4 times the length of tq
nf=length(tq);
while(nf < 4*length(tq) )
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
B = B.*exp(i*2*pi*f.*(x/cnot - x./cw));
%to time domain
b=real(ifft(B));
%apply window
quimp=b(1:length(tq)).*mwhalf(length(tq));