function [wavelet,twave]=wavedyn(dt,fdom,tlength)
% WAVEDYN: minimum phase wavelet for impulsive sources
%
% [wavelet,twave]=wavedyn(dt,fdom,tlength)
% [wavelet,twave]=wavedyn(dt,fdom) 
% [wavelet,twave]=wavedyn(dt) 
% 
% WAVEDYN returns a minimum phase wavelet which simulates a 
% possible noise free impulsive source. Function uses TNTAMP and 
% MINWAVE
%
% dt= desired temporal sample rate
% fdom= dominant frequency in Hz (default: 15 Hz)
% tlength= wavelet length in seconds (default: 127*dt 
%                                     (ie a power of 2))
% 
% The wavelet is generated from an analog expression for a nice
% amplitude spectrum which is sampled and the Hilbert transformed.
% This frequency domain sampling, if too coarse, produces an
% ugly time domain aliasing effect. To minimize this problem,
% the wavelet is first generated with a very fine frequency 
% sample rate and then truncated to the desired length in the
% time domain. 
%
% by G.F. Margrave, May 1991
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

 if(nargin<3)
   tlength=127.*dt;
 end
 if(nargin<2)
   fdom=15.; 
 end
 
% create a time vector
  tveclen=2.*tlength/dt+1;
  tveclen=2.^nextpow2(tveclen);
  tmax=dt*(tveclen-1);
  tw= 0.:dt:tmax;
% create a frequency vector
  fnyq=1./(2*(tw(2)-tw(1)));
  f=linspace(0.,fnyq,length(tw)/2+1);
% create the amplitude spectrum
  ampspec=tntamp(fdom,f); 
% create the wavelet by running minwave
  [w,tw]=minwave(ampspec,f,1E-10); 
% Truncate it using an mwindow
  twave=0.:dt:tlength;
  n=length(twave);
  mw=mwindow(2*n-1,5);
  %wavelet=w(1:n).*mw(n:length(mw));
  [ww,wavelet]=rceps(w(1:n).*mw(n:length(mw))');
% now normalize the wavelet
  powrms=sqrt(wavelet*wavelet');
  wavelet=wavelet/powrms;
 




 		
    