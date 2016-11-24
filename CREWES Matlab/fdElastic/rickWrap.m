function [wave,tw] = rickWrap(Dt,fDom)
%[wave,tw] = rickWrap(Dt,fDom)
%Fix up a ricker wavelet for finite-difference use 
% Dt    = The time sample rate in seconds
% fDom  = The central (dominant) frequency
% wave  = The wavelet amplitudes in sequence
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

% tw    = The wavelet time series in sequence
tlen = Dt*500;
[wavelet,tw1] = ricker(Dt,fDom,tlen);
%plot(wavelet)
%disp(wavelet(1:9))
%stop
[rMax,iMax] = max(wavelet); sc = 1/rMax;
wave1 = sc*wavelet;
%iGood = find(abs(wave1)>.001);
iGood = find(abs(wave1)>.0002);     %Determines taper at end of filter
 %disp(iGood(1))
% figure
% plot(tw1,wave1)
iEnd = iMax*2-iGood(1);
if iEnd>length(wave1)
    error('Time sample rate and wavelet frequency are incompatible')
end
% wave = wave1(iGood(1):len+1-iGood(1));
wave = wave1(iGood(1):iEnd);
len = length(wave);
tw = tw1(1:len);
% disp(rMax); disp(wave(1:5))