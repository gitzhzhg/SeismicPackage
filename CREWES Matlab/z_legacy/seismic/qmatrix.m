function qmat=qmatrix(q,t,w,tw)
% qmat=qmatrix(q,t,w,tw)
%
% Generates a matrix which applies a forward q filter. The qmatrix is
% also bandlimited by a stationary "source signature" waveform.
%
% q ... value of Q
% t ... vector of times to generate q response at
% w ... stationary source waveform
% tw ... time vector for w
% qmat ... length(t) by length(t) matrix which applies a forward Q
%		filter.
% 		If r is a column vector of reflection coefficients, then
%		s=qmat*r; is a simple synthetic seismogram with attenuation.
%
% G.F. Margrave, July 1996, The CREWES Project
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
v=2000;
x=v*t;
nt=length(t);
tmax=max(t);
dt=t(2)-t(1);
qmat=zeros(nt,nt);
if(nargin>2)
	izero=near(tw,0);
end
for k=1:nt
	tmp=einar(q,x(k),v,dt,tmax);
	nuse=round((tmax-t(k))/dt) +1;
	
	%include stationary wavelet
	if(nargin>2)
		tmp=convz(tmp,w,izero,nt,0);
	end
	
	qmat(k:nt,k)=tmp(1:nuse)';
end
%wconv=convmtx(w(:),nt);
%qmat=wconv(1:nt,:)*qmat;
	
	