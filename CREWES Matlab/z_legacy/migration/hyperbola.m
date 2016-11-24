function aout=hyperbola(x,t,xnot,tnot,v,fdom)
% aout=hyperbola(x,t,xnot,tnot,v,fdom)
%
% Generate a set of unit spikes at an approximate hyperbolic trajectory.
% A ricker wavelet of dominant frequency  fdom is then convolved.
%
% x ... vector of x coordinates of traces
% t ... vector of time coordinates of samples
% xnot ... x location of hyperbola
% tnot ... t location of hyperbola
% v ... velocity
% fdom ... ricker wavelet dominant frequency
% aout ... output matrix of length(t) rows by length(x) columns
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
nsamp=length(t);
ntr=length(x);
%make the wavelet
tlen=.2*(t(nsamp)-t(1));
dt=t(2)-t(1);
[w,tw]=ricker(dt,fdom,tlen);
nw=length(w);
aout=zeros(nsamp+nw,ntr);
v=v/2;
for k=1:ntr
	ispike = round(1+ sqrt( tnot*tnot + ((x(k)-xnot)/v)^2 )/dt);
	if( ispike< (nsamp+nw) )
		aout(ispike,k)=1.0;
		aout(:,k) = convz(aout(:,k),w);
	end
end
aout=aout(1:nsamp,:);