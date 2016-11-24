function freqloc=gaborfreq(s,t,twin,tinc,p)
% GABORFREQ ... calculates a local dominant frequency using the Gabor transform
%
% freqloc=gaborfreq(s,t,twin,tinc,p)
%
% An input signal, s, is Gabor transformed using fgabor.  This gives a
% time-frequency decomposition which has a local Fourier spectrum at each
% of the times t0=0, tinc, 2*tinc, ... max(t). The spectral resolution of
% these is determined by twin which is the half-width of a temporal
% Gaussian window. The dominant frequency of each of these local spectra is
% then calculated by fdom=integral(f.*spec.^p)/integral(spec.^p). In this
% expression, "integral" is an integration over all frequencies, f is
% frequency, and p is a small integrer usually 1 or 2. (fdom is often
% called the "centroid frequency".) The result is a local frequency which
% is assigned to each window center. As a final step, these local
% frequecies are interpolated to the time coordinate of the input signal s.
%
% s... input signal
% t... time coordinate vector for s in seconds
% twin ... Gaussian window half width in seconds. A good value here might
%       be 2-10% of the signal length.
% tinc ... increment between Gaussians in seconds. Typically this is
%       smaller than twin by a factor of 2-10.
% p ... spectral exponent to use in calculating the local frequency in each
%       Gabor window.
%   ****** default 2 ****** (Means use Power spectrum)
%
% freqloc ... local frequency computed from Gabor slices. freqloc will be
%       the same size as s and can be plotted by plot(t,freqloc)
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

[S,trow,fcol]=fgabor(s,t,twin,tinc);

fdom=zeros(size(trow));
for k=1:length(trow)
      fdom(k)=sum(fcol.*abs(S(k,:)).^p)/sum(abs(S(k,:)).^p);
end

%interpolate
freqloc=interpextrap(trow,fdom,t,0);