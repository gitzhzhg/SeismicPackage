% CREWES Seismic toolbox
% Seismic processing tools 
%
%
% Transforms
%  FFTRL: forward Fourier transform for real vectors.
%  IFFTRL: inverse Fourier transform for real time series
%  IFKTRAN: inverse fk transform
%  FKTRAN_MC: forward fk transform using less memory than FKTRAN
%  FKTRAN: Forward fk transform
%  IFKTRAN_MC: memory conserving inverse fk transform
%  TPTRAN: tau-p transform for linear trajectories (slant stacking)
%  ITPTRAN: inverse tau-p transform for linear trajectories (filtered back projection)
%  TEST_TPTRAN: demo script for forward and inverse tau-p transforms
%  TR: time reverse a trace
%
% Spectra
%  AVEAMPSPECTRUM ... the average Fourier amplitude spectrum of a gather
%  BURG: compute the Burg (maximum entropy) spectrum
%  MULTITAPER: estimate the spectrum of a short seismic trace using multitaper method
%  FXANALYSIS: perform an f-x spectral analysis of a seismic section
%
% Filters, convolution
%  CONVM: convolution followed by truncation for min phase filters
%  CONVZ: convolution then truncation for non-min phase filters
%  BURGPR: compute a Burg (maximum entropy) prediction error filter
%  BUTTERBAND: apply a Butterworth (bandpass, highpass, or lowpass) filter 
%               to a seismic trace or section
%  BUTTERFILTER: an improved version of BUTTERBAND
%  FILTF: apply a bandass filter to a trace
%  FILTSPEC: designs the filter spectrum for FILTF
%  FILTORM: filter a seismic gather with an Ormsby filter constraint
%  SECTCONV: outdated, just use convz instead
%  SECTFILT: runs filtf on a section (gather)
%  TRACEMIX: trace mixing or spatial filtering.
%  TVSW: time variant spectral whitening
%
% Amplitude adjustment
%  AEC: automatic envelope correction, a better AGC.
%  BALANS: match the rms power of one trace to another
%  CLIP: clips the amplitudes on a trace
%  GAINMUTE: Apply tgain and top mute to a shot record
%  BANDWIDTH_XFER: transfer the bandwidth of one signal to another
%  TGAIN: gain by t^n.
%  GAIN: gain by exp(at)
%  
% Interpolation, resampling
%  RESAMP: resample a signal using sinc function interpolation
%  SINC: sinc function evaluation
%  SINCI: sinc function interpolation for time series without nan's
%  INTERPBL: band-limited sinc-function interpolation (preferred to sinci)
%  INTERPSINC: identical to SINCI except for the order of the input arguments.
%  SINCINAN: sinc function interpolation for signals with embedded nan's 
%  SINQUE: sinc function evaluation
%  SECTRESAMP: runs resamp on each trace in a seismic section
%  STRETCHTIE: time-variant stretch/squeeze to tie a trace to another
%  TREND: estimate the trend of a signal (low order polynomial fit)
%
% Attributes
%  PICKER: make picks in a seismic matrix
%  IPICK: interactive interface to PICKER
%  PICKTOOL: interactive interface to PICKER
%  FIND_ZERO_CROSSINGS: as the name says
%  INS_PHASE: Compute the instantaneous phase useing complex trace theory
%  INS_AMP: Compute the magnitude of the complex trace.
%  INS_FREQ: Compute the instantaneous frequency useing complex trace theory
%  ENV: compute the trace envelope (same thing as INS_AMP).
%  FOMELFREQ: Compute a local frequency by Fomel's method
%  GABORFREQ: Compute a local frequency by a Gabor Method
%  DOM_FREQ: Compute the dominant frequency of a signal (centroid method)
%  TEST_INSTANTANEOUS_FREQ: compare various methods of local frequency
%
% Deconvolution, inversion
%  LEVREC: solve Tx=b using Levinson's recursion
%  DECONF: frequency domain stationary spiking deconvolution
%  DECONW: time domain stationary spiking decon (Wiener)
%  DECONB: time domain stationary spiking decon using Burg spectra
%  DECONW_SHOT: Wiener decon on shot gathers (2D)
%  DECONW_SHOT3D: Wiener decon on 3D shot gathers
%  DECONF_SHOT: frequency domain decon on shot gathers (2D)
%  DECONW_STACK: Weiner decon post stack
%  DECONF_STACK: frequency domain decon post stack
%  DECONB_STACK: Burg decon post stack
%  DECON_WORKBENCH: interactive tool for decon testing
%  TVSW: time variant spectral whitening
%  TVSW_STACK: tvsw post stack
%  SEE ALSO GABORDECON in the gabor_decon folder
%
% Utilities
%  CONSTPHASE: estimate constant phase rotation between two signals by lsq
%  CONSTPHASE2: estimate cons. phs. rotation by systematic search over angles
%  MAKE3DVOL: take 3D seismic in a 2D matrix into a 3D matrix
%  UNMAKE3DVOL: the reverse of make3Dvol
%  PHSROT: apply a constant phase rotation to a seismic trace
%  PHASER: An interactive tool to test your ability to guess phase
%  PHASEERR: compute the error between two phase angles in degrees (quadrant sensitive)
%  TVPHSROT: Time-variant constant-phase rotation of a trace with a Hilbert method
%  TVCONSTPHSROT: Measure and apply constant-phase rotation using Gaussian time windows
%  TVCONSTPHASE: time variant version of constphase
%  TODB: convert to decibels
%  TOMIN: compute the minimum phase equivalent of a signal or wavelet
%  TOINV: compute the causal (min. phs.) inverse of a signal
%  TOINVF: compute freq. domain non-causal inverse of a signal
%  TOALL: compute the all pass equivalent of a signal
%  TOZERO: compute the zero phase equivalent of a signal
%  TR: time reverse a trace
%  ZPLANE_ZEROPLOT: plot the locations of the zeros of a wavelet in the complex z plane
%  
% Auto and cross correlation and dynamic time warping
%  AUTO: single-sided autocorrelation
%  AUTO2: returns the two-sided autocorrelation 
%  AUTOTV: computes and displays time-variant autocorrelations
%  MAXCORR: given two signals, find max crosscorrelation coef and its lag
%  MAXCORR_PHS: given two signals, find max crosscorrelation coef, its lag, and the best
%       constant phase rotation
%  MAXCORR_EPHS: given two signals, cross correlate their envelopes to find the lag and then
%       shift the second signal by this lag and determine the best phase rotation. Also returns
%       correlations before and after.
%  TVMAXCORR: time variant version of maxcorr
%  DTW: dynamic time warping
%  DTWs: smooth dynamic time warping
%  TVCORR: time variant cross correlation
%  demo_DTW_DTWs_tvccorr: demo of the three tools above
% 
%
% Moveout and Traveltime adjustment
%  NMOR: normal moveout removal (forward and inverse)
%  NMOR_SRM: Normal movout for surface related multiples (forward and reverse)
%  STAT: static shift a trace
%  FLATTEN: flatten an event on a seismic section using crosscorrelations
%
% Stacking
%  NMOR_CMP: Remove NMO from a shot gather and map traces to CMP locations
%  CMPSTACK: Common midpoint stack and gather creation.
%
% Seismic line geometry
%  NOMINAL_LINE_GEOM: create source/receiver positions for a 2D model
%  STACKING_CHART: create plots of source/receiver and midpoint/offset coords
%
