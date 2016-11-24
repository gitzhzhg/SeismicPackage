% The CREWES Project Seismic toolbox
% 
% Almost 100 tools for single channel seismic processing and time 
% series analysis.  Various decons, gain corrections, spectral analyses, 
% resampling, wavelets, etc.
% 
% aec       --- automatic envelope correction (gain adjustment like agc)
% auto      --- compute one sided autocorrelation function
% auto2     --- compute two sided autocorrelation function
% backus    --- 3 pt Backus inverse filter to multiple series from waterbtm
% balans    --- adjust the rms power of one trace to equal that of another
% boxf      --- return a boxcar whose width is specified in Hertz
% burg      --- compute the Burg (maximum entropy) spectrum
% burgpr    --- compute a Burg (maximum entropy) prediction error filter
% calfil    --- time variant spectral whitening (zero phase)
% ccorr     --- compute 2n+1 lags of the cross correlation function
% clip      --- "clip" the maximum amplitude on a trace
% colorwell --- design a min phs filter to correct a decon for colored
%               reflectivity
% comb      --- designs a comb function (unit spikes every n samples)
% convm     --- a truncated convolution appropriate for min phs filters
% convz     --- a truncated convolution for non-min phs filters
% dbspec    --- interactive display of decibel fourier amplitude spectrum
% deconb    --- maximum entropy deconvolution
% deconf    --- frequency domain deconvolution
% deconpr   --- Weiner predictive deconvolution
% deconw    --- Weiner spiking deconvolution
% desig     --- deconvolve with a known wavelet (frequency domain)
% desigpoly --- deconvolve with a known wavelet (polynomial division)
% fftave    --- compute the ensemble averaged Fourier amplitude spectrum
% fftrl     --- forward fft of a real trace (returns only positive f's)
% fftshiftc --- column optimized version of fftshiftm
% fftshiftm --- corrected version of Matlabs fftshift
% filtf     --- frequency doamin bandpass filter
% filtspec  --- returns the frequency spectrum applied by filtf
% filtt     --- time domain bandpass filter
% filttest  --- returns the frequency spectrum of the filter applied by filtt
% findex    --- finds all the local extrema (peaks and troughs) of a trace
% freqfft   --- returns a frequency coordinate vector for an fft
% fromdb    --- convert a complex spectrum from (decibels,phs) to (real,imag)
% fxtran    --- compute the fx spectrum with random windowing
% gain      --- apply exponential gain to a trace
% gauss     --- returns a gaussian window
% hilbm     --- Hilbert transform (modified from Matlab to require 
%               power of 2 length)
% hmask     --- compute the Hilbert mask of a trace
% ifftrl    --- inverse fft from complex to real
% impulse   --- generate a unit impulse
% levrec    --- Levinson recursion
% match    --- time domain match filter
% matchf   --- frequency domain match filter
% maxcorr  --- find the maximum cross correlation and its lag
% maxima   --- interpolated estimates of local maxima
% minwave  --- compute a minimum phase wavelet given its amplitude spectrum
% mwhalf   --- compute a half margrave window
% mwindow  --- compute a margrave window
% near     --- return a vector of indicies targeting a time zone
% nmor     --- normal moveout removal
% pad      --- pad (or truncate) a trace to the same length as another
% padpow2  --- pad a trace to the next power of 2
% phsplt   --- interactive display of Fourier phase spectra
% phsrot   --- phase rotate a trace
% predict  --- return a Weiner prediction filter
% qfilter  --- apply a constant q forward Q filter
% reflec   --- generate a synthetic pseudo-random reflectivity
% resamp   --- change the sample rate of a trace
% ricker   --- generate a Ricker wavelet
% rnoise   --- generate a random noise vector
% sinc     --- generate a sinc function
% sinci    --- optimal sinc function interpolation for band limited functions
% sincinan --- version of sinci designed to handle data with NaN's
% sprat    --- estimate Q using the spectral ratio method
% stat     --- static shift (ie time shift) a trace using FFT's
% sweep    --- generate a linear synthetic Vibroseis sweep
% tntamp   --- return an amplitude spectrum characteristic of an 
%              impulsive source
% toall    --- convert an arbitrary time series to an all pass (phase only)
% todb     --- convert complex spectrum (real,imag) to (decibels,phase angle)
% toinv    --- compute the least squares causal inverse of a wavelet
% toinvf   --- compute a non-causal inverse wavelet in the frequency domain
% tomin    --- compute the minimum phase equivalent of a wavelet
% tozero   --- compute the zero phase equivalent of a wavelet
% tr       --- time reverse a trace
% tvfilt   --- apply a time variant filter in the frequency domain
% tvspec   --- compute the complex values time-variant Fourier spectrum
% tvsqf    --- compute the time variant spectrum of  a Q filter
% waterbtm --- compute the impulse response of a zero offset water 
%              bottom multiple
% wavedyn  --- substandard minimum phase wavelet
% wavemin  --- nice minimum phase wavelet
% wavenorm --- normalize a wavelet
% wavevib  --- generate a vibroseis (Klauder) wavelet
% wavez    --- generate a zero phase wavelet with specific dominant frequency
% wavez2   --- generate a zero phase wavelet with specific passband
% wwow     --- estimate a wavelet from a trace without a well
%
% (C) The CREWES Project, 1996  
% NOTE: It is illegal for you to use this software for a purpose other
% than non-profit education or research UNLESS you are employed by a CREWES
% Project sponsor. By using this software, you are agreeing to the terms
% detailed in this software's Matlab source file.
 
% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by 
% its author (identified above) and the CREWES Project.  The CREWES 
% project may be contacted via email at:  crewesinfo@crewes.org
% 
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) Use of this SOFTWARE by any for-profit commercial organization is
%    expressly forbidden unless said organization is a CREWES Project
%    Sponsor.
%
% 2) A CREWES Project sponsor may use this SOFTWARE under the terms of the 
%    CREWES Project Sponsorship agreement.
%
% 3) A student or employee of a non-profit educational institution may 
%    use this SOFTWARE subject to the following terms and conditions:
%    - this SOFTWARE is for teaching or research purposes only.
%    - this SOFTWARE may be distributed to other students or researchers 
%      provided that these license terms are included.
%    - reselling the SOFTWARE, or including it or any portion of it, in any
%      software that will be resold is expressly forbidden.
%    - transfering the SOFTWARE in any form to a commercial firm or any 
%      other for-profit organization is expressly forbidden.
%
% END TERMS OF USE LICENSE
