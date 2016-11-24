function zroots=s_wavelet2roots(wavelet)
% Compute the zeros of the z-transform of a wavelet; for a minimum-phase 
% wavelet the absolute value of the roots must be > 1.
% Written by: E. Rietsch: June 13, 2004
% Last updated:
%
%          zroots=s_wavelet2roots(wavelet)
% INPUT
% wavelet  one-trace seismic structure
% OUTPUT
% zroots   roots of the z-transform

zroots=roots(flipud(wavelet.traces));
