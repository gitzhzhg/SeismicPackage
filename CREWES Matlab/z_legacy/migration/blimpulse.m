function aout=blimpulse(x,t,xnot,tnot,fdom)
% aout=blimpulse(x,t,xnot,tnot,fdom)
%
% Generate a unit spike at a particular point in a zero matrix.
% A ricker wavelet of dominant frequency fdom is then convolved.
%
% x    ... vector of x coordinates of traces
% t    ... vector of time coordinates of samples
% xnot ... x location of spike
% tnot ... t location of spoke
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
aout=zeros(nsamp,ntr);
nspike= near(x,xnot);
ispike=near(t,tnot);
aout(ispike,nspike)=1.0;
aout(:,nspike) = convz(aout(:,nspike),w);