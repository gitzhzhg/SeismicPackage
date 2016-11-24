function w=wavenorm(win)
% w=wavenorm(win)
%
% win= input wavelet
%
% WAVENORM normalizes a wavelet in the sense that the a sinusoidal
% signal at the peak of the wavelets passband will be unchanged in
% amplitude if the wavelet is convolved with said signal.
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
%determine the peak frequency
%fake a time axis
t=xcoord(0,.002,length(win));
%compute spectrum
[s,f]=fftrl(win,t);
s=abs(s);
%find the max of s
maxs=max(s);
ifreq=find(s==maxs);
fnot=f(ifreq);
%make sure there is only 1 fnot
fnot=fnot(length(fnot));
%generate a reference sinusoid
ref=sin(2*pi*fnot*t);
%convolve
c=conv(ref,win);
%find the max of c
cmax=max(c);
%normalize
w=win/cmax;