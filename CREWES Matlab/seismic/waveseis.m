function [w,tw]=waveseis(s,t,fsmo,sigma)
% WAVESEIS: Create a zero phase wavelet with the amplitude spectrum of a seismic trace
%
% [w,tw]=waveseis(s,t,fsmo,sigma)
% 
% s ... the seismic trace upon which the amplitude spectrum is modelled
% t ... time coordinate for s
% fsmo ... length of a convolutional smoother (HZ) applied to the amplitude
%       spectrum of s
% ************** default 10 Hz *************
% sigma ... standard deviation of a Gaussian window applied to the wavelet (time domain truncation).
% The wavelet will be truncated at +/- 3*sigma from t=0.
% ************** default 0.1 sec ***********
%
% w ... the wavelet
% tw ... time coordinate for the wavelet (same size as w)
% 
%
% by G.F. Margrave, 2016
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
if(nargin<4)
    sigma=.1;
end
if(nargin<3)
    fsmo=10;
end
s=s(:);
t=t(:);
dt=t(2)-t(1);
nsigma=round(3*sigma/dt);
nsamps=length(t);
if(nsamps<2*nsigma)
   npad=2*nsigma-nsamps+1;
   s=[s;zeros(npad,1)];
   nsamps=length(s);
   t=dt*(0:nsamps-1)';
end

inot=round(nsamps/2);

df=1/(length(t)*dt);

nsmo=round(fsmo/df);

imp=impulse(s,inot);
tnot=t(inot);

tmp=bandwidth_xfer(s,imp,nsmo);

tmp2=tmp.*exp(-(t-tnot).^2/sigma^2);



w=tmp2(inot-nsigma:inot+nsigma);
w=w/max(w);
tw=dt*(-nsigma:nsigma);