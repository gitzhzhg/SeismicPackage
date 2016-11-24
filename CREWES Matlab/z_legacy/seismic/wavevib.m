function [w,tw]=wavevib(fmin,fmax,dt,slength,wlength)
% [w,tw]=wavevib(fmin,fmax,dt,slength,wlength)
%
% WAVEVIB generates a vibroseis waveform (Clauder wavelet) by
% first calling SWEEP to generate a linear sweep and then calling
% AUTO to autocorrelate that sweep. Theoretically, the autocorrelation
% length is ~ 2*slength but only only the central 'wlength' long
% part is generated. This is like windowing the auto with a boxcar.
%
%
% fmin= minimum swept frequency in Hz
% fmax= maximum swept frequency in Hz
% dt= time sample rate in seconds
% slength= sweep length in seconds
% wlength= desired wavelet length
%
% w= output wavelet
% tw= time coordinate vector for w
% 
% by G.F. Margrave, June 1991
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
 wlength=.5*wlength;
 [s,t]=sweep(fmin,fmax,dt,slength);
 halfw=auto(s,fix(wlength/dt)+1);
 w=[halfw(length(halfw):-1:2) halfw];
 tw=xcoord(-(length(halfw)-1)*dt,dt,w);
 % normalize
 % generate a refenence sinusoid at the dominant frequency
 refwave=sin(2*pi*(fmin+fmax)*.5*tw);
 reftest=convm(refwave,w);
 fact=max(refwave)/max(reftest);
 w=w*fact;