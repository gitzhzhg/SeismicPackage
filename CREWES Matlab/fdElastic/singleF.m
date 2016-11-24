function [wave,tw] = singleF(Dt,freq,cycles)
%[wave,tw] = singleF(Dt,freq,cycles)
%Design (near) single frequency zero phase sweep (tapered)
% Dt     = The time sample rate in seconds
% freq   = The design frequency
% cycles = The basic number of cycles on each side of the centre
% wave   = The wavelet amplitudes in sequence
% tw     = The wavelet time series in sequence
%
% P.M. Manning, Dec 2011
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
%Dt = .0005; %.002;          %Depends on frequency
%freq = 30; %60; %30;      %Frequency in Hz
disp(cycles)
top = 1.25*cycles/freq;
nHalf = ceil(top/Dt);
%nFilt = nHalf*2-1;
high = (nHalf-1)*Dt;
tw = (-high:Dt:high);
signal = cos(tw*freq*2*pi);
A = 1/sqrt(2*pi);
%fact = freq*3.4/(1.25*cycles);
fact = freq*4.5/(1.25*cycles);      %Tapers wavelet end
power = ((fact*tw).^2)*0.5;
env = A*exp(-power);
wave = signal.*env;
%plot(tw,signal,tw,wave)
%plot(tw,env)
disp(wave(1:5))