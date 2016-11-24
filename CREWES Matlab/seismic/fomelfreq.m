function [freqloc,freqins]=fomelfreq(s,t,tsmo,lambda)
% FOMELFREQ ... calulate a local frequency based on a method by S. Fomel
%
% [freqloc,freqins]=fomelfreq(s,t,tsmo,lambda)
%
% Method: See "Local seismic attributes", by S. Fomel, Geophysics, 2007
%
% s = signal
% t = time coordinate for s
% tsmo = halfwidth of triangle smoother (seconds)
% lambda = stabilization constant
%
% freqloc ... Fomel's local frequency
% freqins ... classic instantaneous frequency
%
% by G.F. Margrave, 2013
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

s=s(:);
dt=t(2)-t(1);
%differentiate signal by central finite difference
sprime=zeros(size(s));
sprime(2:end-1)=(s(3:end)-s(1:end-2))/(4*dt*pi);
%hilbert transforms
sh=imag(hilbert(s));
sprimeh2=imag(hilbert(sprime));
sprimeh=zeros(size(s));
sprimeh(2:end-1)=(sh(3:end)-sh(1:end-2))/(4*dt*pi);
%numerator of equation 2
top=s.*sprimeh-sprime.*sh;
%denominator of equation 3
bot=s.^2+sh.^2;
%big D
D=diag(bot);
%make the triangle smoothing matrix
nsmo=round(tsmo/dt);
tri=triang(2*nsmo+1);
S=convmtx(tri/sum(tri),length(s));
S(1:nsmo,:)=[];
S(end-nsmo+1:end,:)=[];
%now build the denominator of equation 7
Bot=lambda^2*eye(length(s))+S*(D-lambda^2*eye(length(s)));
%calculate both frequencies
freqloc=Bot\S*top;
freqins=top./bot;