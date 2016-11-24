function [wavelet,twave]=wavez2(f1,f2,tlength,t)
% [wavelet,twave] = wavez2(f1,f2,tlength,t)
%
% returns a zero phase wavelet of passband as specified
%
% f1= low end of frequency bandpass (Hz)
% f2= high end of frequency bandpass (Hz)
% tlength=temporal length of wavelet
% t= time sample vector with at least two entries 
%     (provides time sample rate)
% wavelet = returned zero phase wavelet
% twave= returned time coordinate vector for wavelet
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
 n=round(tlength/(t(2)-t(1)))+1;
 fnyq= .5/(t(2)-t(1));
 wavelet= fir1(n-1,[f1/fnyq,f2/fnyq]);
 twave=linspace(-tlength/2,tlength/2,n);
% normalize the wavelet
% generate a refenence sinusoid at the dominant frequency
 refwave=sin(2*pi*fdom*twave);
 reftest=convm(refwave,wavelet);
 fact=max(refwave)/max(reftest);
 wavelet=wavelet*fact;